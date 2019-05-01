%%%-------------------------------------------------------------------
%%% @author dominicburkart
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. avr. 2019 12:29
%%%-------------------------------------------------------------------
-module(utils).
-author("dominicburkart").

%% API
-export([select/2, sub/2, indexed/1, in_match_specification/2, not_in_match_specification/2]).


select(Key, L) ->  [Value || {Key1, Value} <- L, Key == Key1].

sub(KeyList, L) -> [{K1, V} || {K1, V} <- L, lists:member(K1, KeyList)]. % retains order in L

indexed(L) ->
  Length = length(L),
  lists:zip(lists:seq(1, Length), L).

in_match_specification_helper(V, [H | T], Partial) ->
  case T of
    [] ->
      {'orelse',  Partial, {'=:=', V, H}};
    T ->
      in_match_specification_helper(V, T, {'orelse',  Partial, {'=:=', V, H}})
  end.

in_match_specification(V, []) ->
  {false};

in_match_specification(V, [H]) ->
  {'=:=', V, H};

in_match_specification(V, [H | T]) ->
  in_match_specification_helper(V, T, {'=:=', V, H}).

not_in_match_specification(V, L) ->
  {'not', in_match_specification(V, L)}.