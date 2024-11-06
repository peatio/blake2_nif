#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "blake2.h"

static ErlNifResourceType* blake2_hashstate;


typedef struct
{
	uint8_t  digest_length;
	blake2b_state state;
} blake2_handle;

// Prototypes
ERL_NIF_TERM blake2_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM blake2_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM blake2_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM blake2_hash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM blake2_hash_with_personal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM blake2_hash_with_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

// lifecycle
int load(ErlNifEnv* env, void ** priv_data, ERL_NIF_TERM load_info);
int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info);
void unload(ErlNifEnv* env, void* priv);

static ErlNifFunc nif_funcs[] =
{
	{"init", 1, blake2_init},
	{"update", 2, blake2_update},
	{"final", 1, blake2_final},
	{"hash", 2, blake2_hash},
    {"hash_with_personal", 3, blake2_hash_with_personal},
    {"hash_with_key", 3, blake2_hash_with_key},
};

ERL_NIF_INIT(blake2_nif, nif_funcs, load, NULL, NULL, NULL)

int load(ErlNifEnv* env, void ** priv_data, ERL_NIF_TERM load_info)
{
	blake2_hashstate = enif_open_resource_type_compat(env, "hashstate", NULL, ERL_NIF_RT_CREATE, NULL);
	return 0;
}

int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
	return 0;
}

int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
	return 0;
}

void unload(ErlNifEnv* env, void* priv)
{
	return;
}


ERL_NIF_TERM blake2_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{   
	ERL_NIF_TERM handle_term;
	int bits = 0;
	if(!enif_get_int(env, argv[0], &bits))
		return enif_make_badarg(env);

	blake2_handle *handle = (blake2_handle *)enif_alloc_resource_compat(env, blake2_hashstate, sizeof(blake2_handle));
	handle->digest_length = bits / 8;

	int r = blake2b_init(&(handle->state), handle->digest_length);
	if (r == 0) {
		handle_term = enif_make_resource(env, handle);
		enif_release_resource_compat(env, handle);
		return enif_make_tuple2(env, enif_make_atom(env, "ok"), handle_term);
	} else {
		enif_release_resource_compat(env, handle);
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "init_failure"));
	}
}

ERL_NIF_TERM blake2_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    blake2_handle *handle = NULL;
    enif_get_resource(env, argv[0], blake2_hashstate, (void**)&handle);

    ErlNifBinary bin;
    enif_inspect_binary(env, argv[1], &bin);

    int r = blake2b_update(&(handle->state), (const uint8_t *)bin.data, (uint64_t)bin.size);
    if (r == 0)
    {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_resource(env, handle));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "update_failure"));
    }
}

ERL_NIF_TERM blake2_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    blake2_handle *handle = NULL;
    enif_get_resource(env, argv[0], blake2_hashstate, (void**)&handle);

    ErlNifBinary out;
    enif_alloc_binary_compat(env, (size_t)(handle->digest_length), &out);

    int r = blake2b_final(&(handle->state), (uint8_t *)out.data, handle->digest_length);
    if (r == 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &out));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "finalization_failure"));
    }
}


ERL_NIF_TERM blake2_hash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int bits = 0;
    enif_get_int(env, argv[0], &bits);

    ErlNifBinary bin, out;
    enif_inspect_binary(env, argv[1], &bin);
    enif_alloc_binary_compat(env, (size_t)(bits/8), &out);

    int r = blake2b((uint8_t *)out.data, (bits/8), (const void *)bin.data, bin.size, NULL, 0);
    if (r == 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &out));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "hash_failure"));
    }
}

ERL_NIF_TERM blake2_hash_with_personal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int bits = 0;
    enif_get_int(env, argv[0], &bits);

    ErlNifBinary bin, personal, out;
    enif_inspect_binary(env, argv[1], &personal);
    enif_inspect_binary(env, argv[2], &bin);

    enif_alloc_binary_compat(env, (size_t)(bits/8), &out);

    int r = blake2b_personal((uint8_t *)out.data, (bits/8), (const void *)bin.data, bin.size ,(const void *)personal.data);
    if (r == 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &out));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "hash_failure"));
    }
}

ERL_NIF_TERM blake2_hash_with_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int bits = 0;
    enif_get_int(env, argv[0], &bits);

    ErlNifBinary bin, key, out;
    enif_inspect_binary(env, argv[1], &key);
    enif_inspect_binary(env, argv[2], &bin);

    enif_alloc_binary_compat(env, (size_t)(bits/8), &out);

    int r = blake2b((uint8_t *)out.data, (bits/8), (const void *)bin.data, bin.size ,(const void *)key.data, key.size);
    if (r == 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &out));
    } else {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "hash_failure"));
    }
}
