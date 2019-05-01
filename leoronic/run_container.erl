%%%-------------------------------------------------------------------
%%% @author dominicburkart
%%% @doc
%%%
%%% @end
%%% Created : 25. avr. 2019 20:59
%%%-------------------------------------------------------------------
-module(run_container).
-author("dominic burkart").

%% API
-export([run_container/2]).


parse_tags([Tag | RemainingTags]) ->
  case Tag of
    {memory, M} -> "--memory " ++ integer_to_list(M);
    {cpus, C} -> "--cpus " ++ float_to_list(C, [{decimals, 2}]);
    {storage, S} -> "--mount type=tmpfs,destination=/temp,tmpfs-size=" ++ integer_to_list(S) ++ "M"
  end ++ " " ++ parse_tags(RemainingTags).


start_pipe(PipeName) ->
  os:cmd("mkfifo " ++ PipeName),
  spawn(run_container, pipe_listener, [PipeName, open_port(PipeName, [eof]), []]).


pipe_listener(PipeName, Pipe, CollectedStr) ->
  receive
    {Pipe, {data, Str}} ->
      pipe_listener(PipeName, Pipe, CollectedStr ++ Str); % todo does this preserve newlines ?
    {Pid, done} ->
      os:cmd("rm "  ++ PipeName),
      Pid ! {PipeName, CollectedStr}
  end.


collect_listener(Pid) ->
  Pid ! { self(), done },
  receive
    {PipeName, CollectedStr} ->
      case PipeName of
        "result" -> {result, CollectedStr};
        "stdout" -> {stdout, CollectedStr};
        "stderr" -> {stderr, CollectedStr}
      end
  end.


get_completed_values(ListenerPids, StartedAt, EndedAt) ->
  Unsorted =
    lists:map(fun (Pid) -> collect_listener(Pid) end, ListenerPids) ++
    [{started_at, StartedAt}, {ended_at, EndedAt}],
  lists:keysort(1, Unsorted).


run_container(Container, Tags) ->
  ListenerPids = lists:map(fun (Name) -> start_pipe(Name) end, ["result", "stdout", "stderr"]),
  StartingTime = os:system_time(),
  os:cmd("docker run " ++ parse_tags(Tags) ++ Container),
  get_completed_values(ListenerPids, StartingTime, os:system_time()).