-module(boss_flash).
-export([get_and_clear/1, add/4, add/3]).

%% @spec get_and_clear(SessionID) -> [Message]
%% @doc Retrieve the current flash messages for `SessionID' and flush the message stack.
get_and_clear(SessionID) ->
    case boss_session:get_session_data(SessionID) of
        undefined -> [];
        BossFlash ->
            case proplists:get_value(boss_flash, BossFlash) of
                undefined -> [];
                Flash ->
                    boss_session:set_session_data(SessionID, proplists:delete(boss_flash, BossFlash)),
                    [{boss_flash, lists:reverse(Flash)}]
            end
    end.

%% @spec add(SessionID, Type, Title) -> ok | {error, Reason}
%% @doc Add a message to the flash message stack for `SessionID'.
add(SessionID, Type, Title) ->
    add(SessionID, Type, Title, undefined).

%% @spec add(SessionID, Type, Title, Message) -> ok | {error, Reason}
%% @doc Add a message to the flash message stack for `SessionID'.
add(SessionID, Type, Title, Message) ->
    Msg = [{method, atom_to_list(Type)}, {title, Title}, {message, Message}],
    Flash = case boss_session:get_session_data(SessionID, boss_flash) of
		undefined ->
		    [Msg];
		ExistingFlash ->
		    [Msg|ExistingFlash]
	    end,
    boss_session:set_session_data(SessionID, boss_flash, Flash).
