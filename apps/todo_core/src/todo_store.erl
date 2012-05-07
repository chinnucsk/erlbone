-module(todo_store).

-include_lib("todo_core/include/todo.hrl").

-define(TABLE_ID,?MODULE).

-export([init/0,insert/1,lookup/1,remove/1,update/1]).
-export([get_all/0,get_all_open/0]).

init() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    mnesia:create_table(todo, [{attributes, record_info(fields,todo)}]).

insert(Todo) ->
    Id = get_new_id(),
    NewTodo = Todo#todo{id=Id},
    case mnesia:dirty_write(NewTodo) of
        ok -> {ok, NewTodo};
        {aborted,Reason} -> {error, Reason}
    end.

lookup(Id) ->
    case mnesia:dirty_read(todo,Id) of
        [Todo] -> {ok,Todo};
        [] -> {error, not_found}
    end.

remove(Id) ->
    mnesia:dirty_delete(todo,Id).

update(#todo{id=Id,text=Text,order=_Order,done=Done}) ->
    F = fun() ->
            [T] = mnesia:read(todo,Id,write),
            New = T#todo{text=Text,done=Done},
            mnesia:write(New),
            New
        end,
    case mnesia:transaction(F) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.

get_all() ->
    Wild = mnesia:table_info(todo,wild_pattern),
    {ok, mnesia:dirty_match_object(todo,Wild)}.

get_all_open() ->
    {ok, mnesia:dirty_match_object(todo,#todo{done=false, _ = '_'})}.

%%=================================================
%% Private
%%=================================================
get_new_id() ->
    {ok,U} = id_server:guid(),
    U.



