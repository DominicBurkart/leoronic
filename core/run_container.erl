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
-export([run_container/3, pipe_listener/3]).
-dialyzer({nowarn_function, [start_pipe/1, run_container/3]}).
% ^ todo localize this to the open_port calls


parse_tags([Tag | RemainingTags]) ->
  FormattedTag =
    case Tag of
      {memory, M} -> "--memory " ++ utils:number_to_list(M) ++"m";
      {cpus, C} -> "--cpus " ++ utils:number_to_list(C);
      {storage, S} ->
        "--mount type=tmpfs,destination=/temp,tmpfs-size=" ++
          utils:number_to_list(S) ++ "M"
    end,
  case RemainingTags of
    [] ->
      FormattedTag;
    _ ->
      FormattedTag ++ " " ++ parse_tags(RemainingTags)
  end.

start_pipe(PipeName) ->
  io:format("Making pipe ~p~n", [PipeName]),
  os:cmd("mkfifo " ++ PipeName),
  spawn(
    run_container,
    pipe_listener,
    [PipeName, open_port(PipeName, [eof]), []]
  ).


pipe_listener(PipeName, Pipe, CollectedStr) ->
  receive
    {Pipe, {data, Str}} ->
      pipe_listener(PipeName, Pipe, CollectedStr ++ Str);
    {Pid, done} ->
      os:cmd("rm " ++ PipeName),
      Pid ! {pipe_collected, PipeName, CollectedStr}
  end.


collect_listener(Pid) ->
  Pid ! {self(), done},
  io:format("Awaiting pipe collection from ~p~n", [Pid]),
  receive
    {pipe_collected, PipeName, CollectedStr} ->
      io:format("Collecting ~p~nValue so far: ~p", [PipeName, CollectedStr]),
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
  StartingTime = os:system_time(1),
  ResultPipe = "result"++TaskIdStr,
  {ok, CurDir} = file:get_cwd(),

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
    "docker build -t " ++
    ImageName ++
    " -<<EOF\n" ++
    string:replace( % todo base64 in container could include string "EOF"
      Container,
      "LEORONIC_RESULT",
      "result"++TaskIdStr,
      all
    ) ++
    "EOF",
  DockerRunCommand =
    "docker run " ++
    parse_tags(Tags) ++
    " -v " ++
    filename:join(CurDir, ResultPipe) ++ % the program needs to be able to write to the result pipe.
    ":/" ++
    ResultPipe ++
    " " ++
    ImageName ++
    " 1> stdout" ++
    TaskIdStr ++
    " 2> stderr" ++
    TaskIdStr,
  DockerCleanUpCommand =
    "docker image rm -f " ++ ImageName,

  % run commands
  DockerFullCommand = lists:flatten([DockerBuildCommand, "\n", DockerRunCommand, "\n", DockerCleanUpCommand]),
  io:format("Full command : ~p~n", [DockerFullCommand]),

  io:format("Launching command...~n"),
  os:cmd(DockerFullCommand),

  % collect results & return with runtime information
  get_completed_values(ListenerPids,
    [{started_at, StartingTime}, {ended_at, os:system_time(1)}]).