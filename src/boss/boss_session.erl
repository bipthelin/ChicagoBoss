%% @doc Chicago Boss session handler abstraction

-module(boss_session).

-export([start/0, start/1, stop/0]).
-export([get_session_key/0, get_session_exp_time/0]).
-export([get_session_data/1, get_session_data/2]).
-export([new_session/1, set_session_data/2, set_session_data/3]).
-export([remove_session_data/2, delete_session/1]).

-define(ADAPTER_CONNECTION, boss_session).

start() ->
    SessionOptions = lists:foldl(fun(OptName, Acc) ->
                case application:get_env(OptName) of
                    {ok, Val} -> [{OptName, Val}|Acc];
                    _ -> Acc
                end
        end, [], [session_key, session_exp_time]),
    SessionDriver = boss_env:get_env(session_adapter, mock),
    SessionOptions1 = [{adapter, list_to_atom("boss_session_adapter_"++atom_to_list(SessionDriver))}|SessionOptions],
    start(SessionOptions1).

start(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_session_adapter_mock),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    {ok, Conn} = Adapter:start(Options),
    boss_registry:put(?ADAPTER_CONNECTION, {Adapter, Conn}).

stop() ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    Adapter:stop(Connection).

get_session_key() ->
    boss_env:get_env(session_key, "_boss_session").

get_session_exp_time() ->
    boss_env:get_env(session_exp_time, 1440).

%% @spec new_session(Cookie::string()) -> string | {error, Reason}
%% @doc Starts new session with the specified `Cookie'.
new_session(Cookie) ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    case Adapter:session_exists(Connection, Cookie) of
        true ->
            Cookie;
        false ->
            SessionID = generate_session_id(),
            Adapter:create_session(Connection, SessionID, []),
            SessionID
    end.

%% @spec get_session_data(SessionID) -> list | {error, Reason}
%% @doc Get session data for the `SessionID'.
get_session_data(SessionID) ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    Adapter:lookup_session(Connection, SessionID).

%% @spec get_session_data(SessionID, Key) -> list | {error, Reason}
%% @doc Get session data for the `SessionID' for a given `Key'.
get_session_data(SessionID, Key) ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    Adapter:lookup_session_value(Connection, SessionID, Key).

%% @spec set_session_data(SessionID, Value) -> ok | {error, Reason}
%% @doc Set session data for the `SessionID'.
set_session_data(SessionID, Value) ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    Adapter:set_session_value(Connection, SessionID, Value).

%% @spec set_session_data(SessionID, Key, Value) -> ok | {error, Reason}
%% @doc Set session data for the `SessionID'.
set_session_data(SessionID, Key, Value) ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    Adapter:set_session_value(Connection, SessionID, Key, Value).

%% @spec delete_session(SessionID) -> ok | {error, Reason}
%% @doc Delete session for given `SessionID'.
delete_session(SessionID) ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    Adapter:delete_session_value(Connection, SessionID).

%% @spec remove_session_data(SessionID, Key) -> ok | {error, Reason}
%% @doc Remove the Key from session data for the `SessionID'.
remove_session_data(SessionID, Key) ->
    {Adapter, Connection} = boss_registry:get(?ADAPTER_CONNECTION),
    Adapter:delete_session_value(Connection, SessionID, Key).

generate_session_id() ->
    Data = crypto:rand_bytes(2048),
    Sha_list = binary_to_list(crypto:sha(Data)),
    lists:flatten(list_to_hex(Sha_list)).
%% Convert Integer from the SHA to Hex
list_to_hex(L)->
       lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
       [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
