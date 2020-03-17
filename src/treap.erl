-module(treap).

-export([new/1,
         new/2, 
         is_empty/1,
         merge/2, 
         split/2,
         add/2,
         add/3,
         remove/2,
         is_key/2,
         fetch/2,
         from_list/1,
         to_list/1,
         keys/1
    ]).

-include("treap.hrl").


new(X,Y) ->  #treap{x = X, y = Y}.
new(X) ->    #treap{x = X, y = rand:uniform()}.

merge(nil, Right)-> Right;
merge(Left, nil)->  Left;
merge(#treap{y = Y1, left = Left1, right = Right1}=Left,
      #treap{y = Y2, left = Left2, right = Right2}=Right)-> 
    if
        Y1 > Y2  -> Left#treap{left = Left1, right = merge(Right1, Right)};
        true ->     Right#treap{left = merge(Left, Left2), right = Right2}
            
    end.
split(_X,nil) -> {nil,nil};
split(X0, #treap{x = X, left = Left, right = Right} = T)-> 
    if
        X =< X0 -> {T1, T2} =  split(X0,Right),
                  {T#treap{right = T1},T2};
        true ->   {T1, T2} =  split(X0,Left),
                  {T1,T#treap{left = T2}}
    end.

add(X,T)->
    {T1, T2} = split(X,T),
    New = new(X),
    merge(merge(T1, New),T2).

add(X,Y,T)->
    {T1, T2} = split(X,T),
    New = new(X,Y),
    merge(merge(T1, New),T2).

remove(X, T)-> 
    {L,R0} = split(X-1, T),
    {_, R} = split(X, R0),
    merge(L,R).

is_empty(nil)-> true;
is_empty(#treap{})-> false.

is_key(_X, nil) -> false;
is_key(X,#treap{x = X})-> true;
is_key(X,#treap{x = X2, left = Left, right = Right})->
    if
        X < X2 -> is_key(X,Left);            
        X > X2 -> is_key(X,Right)            
    end.
    
fetch(X, T)->
    {_,R0} = split(X-1, T),
    {L, _} = split(X, R0),
    parse(L).
    
parse(nil)-> [];
parse(#treap{x = X, y = Y, left = Left, right = Right})->
    [{X,Y}] ++ parse(Left) ++ parse(Right).
    
from_list(L)-> 
    lists:foldl(
                fun
                    ({X,Y},T)-> add(X,Y,T);
                    (X,T) ->    add(X,T)
                end, 
    nil, L).
   
to_list(#treap{x = X, y = Y, left = Left, right = Right})-> 
   [{X,Y}] ++ parse(Left) ++ parse(Right).
keys(nil)-> [];
keys(#treap{x = X, left = Left, right = Right})-> [X] ++ keys(Left) ++ keys(Right).
