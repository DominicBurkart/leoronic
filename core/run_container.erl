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
-export([run_container/3]).
-dialyzer({nowarn_function, [start_pipe/1, run_container/3]}).
% ^ todo localize this to the open_port calls


parse_tags([Tag | RemainingTags]) ->
  FormattedTag =
    case Tag of
      {memory, M} -> "--memory " ++ integer_to_list(M);
      {cpus, C} -> "--cpus " ++ float_to_list(C, [{decimals, 2}]);
      {storage, S} ->
        "--mount type=tmpfs,destination=/temp,tmpfs-size=" ++
          integer_to_list(S) ++ "M"
    end,
  case RemainingTags of
    [] ->
      FormattedTag;
    _ ->
      FormattedTag ++ " " ++ parse_tags(RemainingTags)
  end.


start_pipe(PipeName) ->
  os:cmd("mkfifo " ++ PipeName),
  spawn(run_container, pipe_listener, [PipeName, open_port(PipeName, [eof]), []]).


pipe_listener(PipeName, Pipe, CollectedStr) ->
  receive
    {Pipe, {data, Str}} ->
      pipe_listener(PipeName, Pipe, CollectedStr ++ Str);
    {Pid, done} ->
      os:cmd("rm " ++ PipeName),
      Pid ! {PipeName, CollectedStr}
  end.


collect_listener(Pid) ->
  Pid ! {self(), done},
  receive
    {PipeName, CollectedStr} ->
      case string:sub_string(PipeName, 1, 6) of
        "result" -> {result, CollectedStr};
        "stdout" -> {stdout, CollectedStr};
        "stderr" -> {stderr, CollectedStr}
      end
  end.


get_completed_values(ListenerPids, AdditionalValues) ->
  lists:keysort(1,
    AdditionalValues ++ [collect_listener(Pid) || Pid <- ListenerPids]
  ).


run_container(Container, Tags, TaskIdStr) when is_list(TaskIdStr) ->
  % open pipes
  ListenerPids = [start_pipe(V ++ TaskIdStr) || V <- ["result", "stdout", "stderr"]],
  StartingTime = os:system_time(second),

  % make commands to build image from container & run it
  ImageName =
    "leoronic_container_" ++
    re:replace(
      pid_to_list(self()),
      "[^0-9]",
      "",
      [global, {return, list}]
    ),
  DockerBuildCommand =
    "echo \"" ++
    string:replace(
      Container,
      "LEORONIC_RESULT",
      "result"++TaskIdStr
    ) ++ "\" | docker build -t " ++
    ImageName ++
    " -f- .", % todo these containers shouldn't need build contexts (the ".")
  DockerRunCommand =
    "docker run " ++
    parse_tags(Tags) ++
    " " ++
    ImageName ++
    " 1> stdout" ++
    TaskIdStr ++
    " 2> stderr" ++
    TaskIdStr,
  DockerCleanUpCommand =
    "docker image rm -f " ++ ImageName,
  % todo remove the container here, not just the image
  CombinedCommand =
    string:join([DockerBuildCommand, DockerRunCommand, DockerCleanUpCommand], "; "),

  % run commands
  os:cmd(CombinedCommand),

  % collect results & return with runtime information
  get_completed_values(ListenerPids,
    [{started_at, StartingTime}, {ended_at, os:system_time(second)}]).