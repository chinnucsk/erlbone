-module(todo_web_app).

-behaviour(application).

-export([start/2,stop/1]).


start(_Type, _StartArgs) ->
    todo_web_sup:start_link().

stop(_State) ->
    ok.
