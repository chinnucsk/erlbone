-module(todo_web_resource).
-export([init/1, 
         allowed_methods/2, 
         content_types_accepted/2, 
         content_types_provided/2, 
         resource_exists/2, 
         to_json/2, 
         delete_resource/2,
         process_post/2, 
         from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("todo_core/include/todo.hrl").

init([]) -> {ok, undefined}.


%% FIXME: let that depend on the request path
%% /todos only GET, POST
%% /todos/:id GET, PUT , DELETE, POST
allowed_methods(ReqData,Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT','DELETE'], ReqData, Ctx}.

%%==========================================================
%% GET
%%==========================================================
content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

resource_exists(ReqData,Ctx) ->
    case wrq:path_info(id,ReqData) of
        undefined ->
            {ok, Todos} = todo_core:all_open(),
            {true, ReqData, Todos};
        Id ->
            case todo_core:find(list_to_binary(Id)) of 
                {ok, Todo} ->  
                    {true, ReqData, Todo};
                {error,_} ->
                    {false, ReqData, Ctx}
            end
    end.

to_json(ReqData, ItemData) ->
    Proplist = todo_to_proplist(ItemData),
    {mochijson2:encode(Proplist),ReqData,ItemData}.

%%==========================================================
%% DELETE
%%==========================================================
delete_resource(ReqData,Ctx) ->
     case wrq:path_info(id, ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        ID ->
            case todo_core:remove(list_to_binary(ID)) of 
                ok ->
                    {true, ReqData, Ctx};
                _ ->
                    {false, ReqData, Ctx}
            end
    end.   


%%==========================================================
%% PUT/POST
%%==========================================================
content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    {ok, NewDoc} = todo_core:create(proplist_to_todo(Doc)), 
    {NewJsonDoc,_,_} = to_json(ReqData,NewDoc),
    ReqData2 = wrq:set_resp_body(NewJsonDoc, ReqData),
    {true, ReqData2, Ctx}.

from_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        _ID ->
            {struct, Doc} = mochijson2:decode(wrq:req_body(ReqData)),
            Todo = proplist_to_todo(Doc),
            {ok, NewDoc} = todo_core:update(Todo),
            {NewJsonDoc,_,_} = to_json(ReqData,NewDoc),
            ReqData2 = wrq:set_resp_body(NewJsonDoc, ReqData),
            {true, ReqData2, Ctx}
    end.

%%==========================================================
%% PRIVATE
%%==========================================================

todo_to_proplist(Todo) when is_record(Todo,todo) ->
    {struct,[{id,Todo#todo.id},
     {text, case is_binary(Todo#todo.text) of 
                    true -> Todo#todo.text;
                    _ -> list_to_binary(Todo#todo.text)
             end},
     {order, Todo#todo.order},
     {done, Todo#todo.done}]};
todo_to_proplist([Todo | _Rest] = Todos) when is_record(Todo,todo) ->
    lists:map(fun(X) -> todo_to_proplist(X) end, Todos);
todo_to_proplist([]) -> [].
    
proplist_to_todo(Plist) ->
    {_, Text} = proplists:lookup(<<"text">>,Plist),
    {_, Done}  = proplists:lookup(<<"done">>,Plist),
    {_, Order} = proplists:lookup(<<"order">>,Plist),
    Todo  = #todo{text=Text, done=Done, order=Order},
    case proplists:lookup(<<"id">>,Plist) of
        none -> Todo;
        {_,Id} -> Todo#todo{id=Id}
    end.
