-module(boss_cache).
-export([start/0, start/1]).
-export([stop/0]).
-export([get/2, set/4, delete/2]).

-define(ADAPTER, boss_cache_adapter).
-define(CONNECTION, boss_cache_connection).

start() ->
    CacheAdapter = boss_env:get_env(cache_adapter, memcached_bin),
    start([{adapter, list_to_atom("boss_cache_adapter_"++atom_to_list(CacheAdapter))},
            {cache_servers, boss_env:get_env(cache_servers, [{"127.0.0.1", 11211, 1}])}]).

start(Options) ->
    Adapter = proplists:get_value(adapter, Options, boss_cache_adapter_memcached_bin),
    {ok, Conn} = Adapter:start(Options),
    boss_registry:put(?ADAPTER, Adapter),
    boss_registry:put(?CONNECTION, Conn).

stop() ->
    Adapter = boss_registry:get(?ADAPTER),
    Connection = boss_registry:get(?CONNECTION),
    Adapter:stop(Connection).

set(Prefix, Key, Value, TTL) ->
    Adapter = boss_registry:get(?ADAPTER),
    Connection = boss_registry:get(?CONNECTION),
    Adapter:set(Connection, Prefix, Key, Value, TTL).

get(Prefix, Key) ->
    Adapter = boss_registry:get(?ADAPTER),
    Connection = boss_registry:get(?CONNECTION),
    Adapter:get(Connection, Prefix, Key).

delete(Prefix, Key) ->
    Adapter = boss_registry:get(?ADAPTER),
    Connection = boss_registry:get(?CONNECTION),
    Adapter:delete(Connection, Prefix, Key).
