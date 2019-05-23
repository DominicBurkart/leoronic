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
-export([
  select/2,
  sub/2,
  indexed/1,
  in_match_specification/2,
  not_in_match_specification/2,
  root_directory/0
]).

root_directory() ->
  % formerly did git root directory ("git rev-parse --show-toplevel")
  case os:getenv("HOME") of
    false ->
      os:getenv("%userprofile%"); % windows
    Dir ->
      Dir
  end.

select(Key, L) ->
  element(2, lists:keyfind(Key, 1, L)).

sub(KeyList, L) -> [{K1, V} || {K1, V} <- L, lists:member(K1, KeyList)]. % retains order in L

indexed(L) ->
  lists:zip(lists:seq(1, length(L)), L).

in_match_specification_helper(V, [H | T], Partial) ->
  case T of
    [] ->
      {'orelse',  Partial, {'=:=', V, H}};
    T ->
      in_match_specification_helper(V, T, {'orelse',  Partial, {'=:=', V, H}})
  end.

in_match_specification(_, []) ->
  {false};

in_match_specification(V, [H]) ->
  {'=:=', V, H};

in_match_specification(V, [H | T]) ->
  in_match_specification_helper(V, T, {'=:=', V, H}).

not_in_match_specification(V, L) ->
  {'not', in_match_specification(V, L)}.