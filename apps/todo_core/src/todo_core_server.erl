-module(todo_core_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state,{}).

-export([start_link/0,create/1,update/1,all_open/0,all_todos/0,remove/1,find/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Interface Function Definitions
%% ------------------------------------------------------------------


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(Todo) ->
    gen_server:call(?SERVER, {create_todo,Todo}, infinity).
update(Todo) ->
    gen_server:call(?SERVER, {update_todo,Todo}, infinity).
all_open() ->
    gen_server:call(?SERVER, open_todos, infinity).
all_todos() ->
    gen_server:call(?SERVER, all_todos, infinity).
remove(Todo) ->
    gen_server:call(?SERVER, {remove_todo,Todo}, infinity).
find(Todo) ->
    gen_server:call(?SERVER, {find, Todo}, infinity).





%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call({create_todo, Todo}, _From, State) ->
    {ok, Res} = todo_store:insert(Todo),
    {reply, {ok, Res}, State};
handle_call({update_todo, Todo}, _From, State) ->
    {ok, Res} = todo_store:update(Todo),
    {reply, {ok, Res} , State};
handle_call(open_todos, _From, State) ->
    {ok, Res} = todo_store:get_all_open(),
    {reply, {ok, Res}, State};
handle_call(all_todos, _From, State) ->
    {ok, Res} = todo_store:get_all(),
    {reply, {ok, Res}, State};
handle_call({find,Id}, _From, State) ->
    Res = todo_store:lookup(Id),
    {reply, Res, State};
handle_call({remove_todo, Id}, _From, State) ->
    case todo_store:remove(Id) of
        ok -> {reply, ok, State};
        _  -> {reply, error, State}
    end.

handle_cast(_ReqData,State) ->
    {noreply,State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


