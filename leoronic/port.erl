%%%-------------------------------------------------------------------
%%% @author dominicburkart
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. avr. 2019 18:03
%%%-------------------------------------------------------------------
-module(port).
-author("dominicburkart").

%% API
-import(link_to_leoronic,
[link_to_leoronic/0]).
-import(head,
[heads/0, head_pid/0]
).
-import(
utils,
[git_root_directory/0]
).
-export([start/0, stop/0, init/0]).
-dialyzer({nowarn_function, [connect_to_pipe_and_loop/0, init/0]}).
% todo constrain this to the call to open_port (dialyzer can't handle it).

start() ->
  spawn(port, init, []).

stop() ->
  leoronic_port ! stop.

pipe_name() ->
  filename:join(git_root_directory(), "leoronic.pipe").

init() ->
  register(leoronic_port, self()),
  process_flag(trap_exit, true),
  make_pipe(),
  connect_to_pipe_and_loop().

make_pipe() ->
  os:cmd("mkfifo " ++ pipe_name()).

remove_pipe() ->
  os:cmd("rm " ++ pipe_name()).

connect_to_pipe_and_loop() ->
  PipeName = pipe_name(),
  case open_port(PipeName, [eof]) of
    Pipe when erlang:is_port(Pipe) ->
      loop(Pipe);
    Error ->
      Error
  end.

loop(Pipe) ->
  receive
    {Pipe, {data, Str}} ->
      case Str of
        "add task " ++ TaskStr ->
          head_resp_to_pipe(Pipe, add_task, format_task_str(TaskStr));
        "remove task " ++ TaskId ->
          head_resp_to_pipe(Pipe, remove_task, TaskId);
        "retrieve task " ++ TaskId ->
          head_resp_to_pipe(Pipe, retrieve, TaskId);
        "all heads true" ->
          head_resp_to_pipe(Pipe, all_heads, true);
        "all heads false" ->
          head_resp_to_pipe(Pipe, all_heads, false);
        "has outstanding" ->
          head_resp_to_pipe(Pipe, has_outstanding, undefined);
        "save state " ++ StateName ->
          head_resp_to_pipe(Pipe, save_state, StateName);
        "resume state " ++ StateName ->
          head_resp_to_pipe(Pipe, resume_state, StateName);
        "remove state " ++ StateName ->
          head_resp_to_pipe(Pipe, remove_state, StateName);
        "idle true" ->
          head_resp_to_pipe(Pipe, idle, true);
        "idle false" ->
          head_resp_to_pipe(Pipe, idle, false)
      end,
      loop(Pipe);
    {Pipe, eof} ->
      connect_to_pipe_and_loop();
    {task_complete, CompletedTask} ->
      Pipe ! {self(), {data, task_to_str(CompletedTask)}},
      loop(Pipe);
    stop ->
      Pipe ! {self(), close},
      receive
        {Pipe, closed} ->
          remove_pipe(),
          exit(normal)
      end;
    {'EXIT', Pipe, Reason} ->
      remove_pipe(),
      exit(port_terminated)
  end.


format_task_str(TaskStr) ->
  [Await, CPUS, Memory, Storage, Dockerless, Container] =
    string:tokens(TaskStr, "\""), % todo desuck this
  [
    {port_pid, self()},
    {await, Await},
    {cpus, CPUS},
    {memory, Memory},
    {storage, Storage},
    {dockerless, Dockerless},
    {container, Container}
  ].


task_to_str(Task) ->
  [
    {id, ID},
    {has_run, _},
    {respond_to, _},
    {created_at, CreatedAt},
    {started_at, StartedAt},
    {finished_at, FinishedAt},
    {dockerless, _},
    {memory, _},
    {storage, _},
    {cpus, _},
    {stdout, StdOut},
    {stderr, StdErr},
    {result, Result},
    {container, _}
  ] = Task,

  "id=\"" ++ ID ++
    "\"created_at=\"" ++ CreatedAt ++
    "\"started_at=\"" ++ StartedAt ++
    "\"finished_at=\"" ++ FinishedAt ++
    "\"stdout=\"" ++ StdOut ++
    "\"stderr=\"" ++ StdErr ++
    "\"result=\"" ++ Result ++
    "\"".


to_head(Type, Value) ->
  head_pid() ! {self(), Type, Value},
  receive
    Response -> Response
  end.


head_resp_to_pipe(Pipe, Type, Value) ->
  spawn(fun () -> Pipe ! {self(), to_head(Type, Value)} end).