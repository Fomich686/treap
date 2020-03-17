-module(treap_store).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([
          add/1,
          add/2,
          is_key/1,
          remove/1,
          fetch/1,
          from_list/1,
          to_list/0,
          keys/0,
          is_empty/0
        ]).


add(X)->
  gen_server:call(?MODULE,{add,X}).
add(X,Y)->
  gen_server:call(?MODULE,{add,X,Y}).
is_key(X)->
  gen_server:call(?MODULE,{is_key,X}).
remove(X)->
  gen_server:call(?MODULE,{remove,X}).
fetch(X)->
  gen_server:call(?MODULE,{fetch,X}).
from_list(L)->
  gen_server:call(?MODULE,{from_list,L}).
to_list()->
  gen_server:call(?MODULE,to_list).
keys()->
  gen_server:call(?MODULE,keys).
is_empty()->
  gen_server:call(?MODULE, is_empty).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, nil}.


handle_call({add, X}, _From, State) ->
    NewState = treap:add(X, State),
    {reply, ok, NewState};
handle_call({add,X,Y}, _From, State) ->
    NewState = treap:add(X, Y, State),
    {reply, ok, NewState};
handle_call({is_key,X}, _From, State) ->
    Result = treap:is_key(X, State),
    {reply, Result, State};
handle_call({remove,X}, _From, State) ->
    NewState = treap:remove(X, State),
    {reply, ok, NewState};
handle_call(to_list, _From, State) ->
    List = treap:to_list(State),
    {reply, List, State};
handle_call(keys, _From, State) ->
    Keys = treap:keys(State),
    {reply, Keys, State};
handle_call(is_empty, _From, State) ->
    Result = treap:is_empty(State),
    {reply, Result, State};
handle_call({from_list,L}, _From, State) ->
    case treap:is_empty(State) of
         true -> NewState = treap:from_list(L),
                 {reply, ok, NewState};             
         false -> {reply, not_empty, State}
    end;    
handle_call({fetch,X}, _From, State) ->
    Result = treap:fetch(X, State),
    {reply, Result, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.