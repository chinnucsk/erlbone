-module(todo_core).

-export([find/1, create/1, remove/1, update/1]).
-export([all_todos/0, all_open/0]).
-export([start/0,stop/0]).

start() ->
    application:start(?MODULE).
stop() ->
    application:stop(?MODULE).

create(Todo) ->
    todo_core_server:create(Todo).

update(Todo) ->
    todo_core_server:update(Todo).

remove(Id) ->
    todo_core_server:remove(Id).

find(Id) ->
    todo_core_server:find(Id).
 

all_todos() ->
    todo_core_server:all_todos().
all_open() ->
    todo_core_server:all_open().
    
