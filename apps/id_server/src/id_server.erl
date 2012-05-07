-module(id_server).

-export([start/0,stop/0]).

-export([guid/0]).


start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

guid() ->
    id_server_server:guid().    
