-module(treap).

-export([new/1,
         new/2, 
         merge/2, 
         split/2,
         add/2
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
        X < X0 -> {T1, T2} =  split(X0,Right),
                  {T#treap{right = T1},T2};
        true ->   {T1, T2} =  split(X0,Left),
                  {T1,T#treap{left = T2}}
    end.

add(X,T)->
    {T1, T2} = split(X,T),
    New = new(X),
    merge(merge(T1, New),T2).