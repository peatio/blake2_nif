-module(blake2_nif).
-export([init/1,
	update/2,
	final/1,
	hash/2,
    hash_with_personal/3]).
-on_load(init/0).

-define(APPNAME, blake2_nif).
-define(LIBNAME, blake2_nif).

-spec init(non_neg_integer()) -> {ok, binary()} | {error, atom()}.
init(_Bits) ->
	erlang:nif_error({error, not_loaded}).

-spec update(binary(), binary()) -> {ok, binary()} | {error, atom()}.
update(_State, _Data) ->
	erlang:nif_error({error, not_loaded}).

-spec final(binary()) -> {ok, binary()} | {error, atom()}.
final(_State) ->
	erlang:nif_error({error, not_loaded}).

-spec hash(non_neg_integer(), binary()) -> {ok, binary()} | {error, atom()}.
hash(_Bits,_Data) ->
	erlang:nif_error({error, not_loaded}).


-spec hash_with_personal(non_neg_integer(), binary(), binary()) -> {ok, binary()} | {error, atom()}.
hash_with_personal(_Bits,_Personal,_Data) ->
	erlang:nif_error({error, not_loaded}).


init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

