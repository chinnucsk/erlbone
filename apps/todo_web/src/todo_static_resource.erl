%% The original has no copyright information and a license not related to the erlang part.
%% but the code here comes with minor alterations from 
%% https://github.com/tsloughter/eCloudEdit/blob/master/lib/ece_web/src/ece_resource_static.erl
-module(todo_static_resource).

-export([init/1, allowed_methods/2, resource_exists/2, content_types_provided/2, provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

-record(context, {docroot,fullpath,fileinfo,response_body}).

init([]) -> 
    {ok, App} = application:get_application(),
    PrivDir = code:priv_dir(App),
    {ok,#context{docroot=PrivDir ++ "/www/"}}.

allowed_methods(ReqData,Context) ->
    {['HEAD','GET'], ReqData, Context}.

%% FIXME: pick up resource here and pass along in Context (when found)
%% instead of waiting until provide_content
resource_exists(ReqData,Context) ->
    {true, ReqData, Context}.
    

content_types_provided(ReqData,Ctx)->
    Path = get_full_path(Ctx, wrq:disp_path(ReqData)),
    {[{webmachine_util:guess_mime(Path), provide_content}],ReqData,Ctx}.

provide_content(ReqData,Ctx) ->
    case maybe_fetch_object(Ctx, wrq:disp_path(ReqData)) of
        {true, NewContext} ->
            Body = NewContext#context.response_body,
            {Body,ReqData,Ctx};
        {false, NewContext} ->
            {error,ReqData,NewContext}
    end.


maybe_fetch_object (Ctx,Path) ->
    case Ctx#context.response_body of
        undefined ->
            case file_exists(Ctx,Path) of
                {true,FullPath} ->
                    {ok,Value} = file:read_file(FullPath),
                    {true,Ctx#context{response_body = Value}};
                false ->
                    {false, Ctx}
            end;
        _Body ->
            {true, Ctx}
    end.

file_exists(Ctx,Path) ->
    FPath = get_full_path(Ctx,Path),
    case filelib:is_regular(FPath) of
        true -> {true, FPath};
        false -> false
    end.

get_full_path(Ctx,Path) ->
    Root = Ctx#context.docroot,
    case mochiweb_util:safe_relative_path(Path) of
        undefined -> undefined;
        RelPath ->
            FullPath = filename:join([Root,RelPath]),
            case filelib:is_dir(FullPath) of
                true ->
                    filename:join([FullPath,"index.html"]);
                false ->
                    FullPath
            end
    end.

