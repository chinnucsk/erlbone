%% Parts of the guid-generating code below is adapted
%% from https://github.com/rabbitmq/rabbitmq-server/blob/master/src/rabbit_guid.erl
%% published under the http://www.mozilla.org/MPL/ (Version 1.1) 
%% with the following copyright statement:
%%
%% %% The Initial Developer of the Original Code is VMware, Inc.
%% %% Copyright (c) 2007-2012 VMware, Inc.  All rights reserved.

-module(id_server_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state,{last_serial, blox}).

-export([start_link/0,guid/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



%%========================================================
%% interface
%%========================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [build_serial()], []).

guid() ->
    gen_server:call(?SERVER, {guid}, infinity).

%%========================================================
%% gen_server handlers
%%========================================================

init([Serial]) ->
    {ok, #state{last_serial=Serial}}.


handle_call({guid}, _From, State) ->
    {NewState,Guid} = generate_guid(State),
    {reply, {ok, Guid}, NewState}.

handle_cast(_Some, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=================================
%% private functions
%%=================================

build_serial() ->
    {A,B,C} = now(),
    (A * 1000000 + B) * 1000000 + C.

make_fresh(Serial) ->
    Unhashed = {Serial,node(),make_ref()},
    Bin = term_to_binary(Unhashed),
    Hashed = erlang:md5(Bin),
    <<B1:32, B2:32, B3:32, B4:32>> = Hashed,
    {{B1,B2,B3,B4},0}.

advance_blocks({{B1,B2,B3,B4}, I}) ->
    New = erlang:phash2({B1,I},4294967296),
    {{(B2 bxor New), (B3 bxor New), (B4 bxor New), New}, I+1}.

blocks_to_binary({B1,B2,B3,B4}) -> <<B1:32, B2:32, B3:32, B4:32>>.



generate_guid(State) ->
    {Blocks, I} = case State#state.blox of
                    undefined ->
                        make_fresh(State#state.last_serial);
                    {BSX,IX} -> 
                        advance_blocks({BSX,IX})
                  end,
    Guid =lists:foldl(fun ($\+,Acc) -> [$\-|Acc];
                          ($\/,Acc) -> [$\_|Acc];
                          ($\=,Acc) -> Acc;
                          (Chr,Acc) -> [Chr|Acc]
                      end, [],base64:encode_to_string(blocks_to_binary(Blocks))),           
    {State#state{blox={Blocks,I}},list_to_binary(Guid)}.
