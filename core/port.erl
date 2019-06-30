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
[root_directory/0]
).
-export([start/0, stop/0, init/0]).
-dialyzer({nowarn_function, [connect_to_pipe_and_loop/0, init/0]}).
% todo constrain this to the call to open_port (dialyzer can't handle it).

start() ->
  spawn(port, init, []).

stop() ->
  leoronic_port ! stop.

pipe_name(Type) ->
  case Type of
    in ->
      filename:join(root_directory(), "leoronic_in.pipe");
    out ->
      filename:join(root_directory(), "leoronic_out.pipe")
  end.

init() ->
  io:format("initiating leoronic port...~n"),
  register(leoronic_port, self()),
  process_flag(trap_exit, true),
  io:format("making pipes...~n"),
  make_pipes(),
  io:format("connecting to pipes...~n"),
  connect_to_pipe_and_loop().

pipe_cmd(Cmd) when is_list(Cmd) ->
  [os:cmd(Cmd ++ " " ++ pipe_name(Pipe)) || Pipe <- [in, out]].

make_pipes() ->
  pipe_cmd("mkfifo").

remove_pipes() ->
  pipe_cmd("rm").

connect_to_pipe_and_loop() ->
  loop(
    open_port(pipe_name(in), [eof]),
    open_port(pipe_name(out), [eof])
  ).

loop(PipeIn, PipeOut) ->
  io:format("port awaiting input...~n"),
  receive
    {_PipeIn, {data, Str}} ->
      io:format("received pipe input: "++Str),
      case Str of
        "add task " ++ TaskStr ->
          io:format("pipe input parsed as new task~n"),
          head_resp_to_pipe(PipeOut, add_task, format_task_str(TaskStr));
        "remove task " ++ TaskId ->
          head_resp_to_pipe(PipeOut, remove_task, TaskId);
        "retrieve task " ++ TaskId ->
          head_resp_to_pipe(PipeOut, retrieve, TaskId);
        "all heads true" ->
          head_resp_to_pipe(PipeOut, all_heads, true);
        "all heads false" ->
          head_resp_to_pipe(PipeOut, all_heads, false);
        "has outstanding" ->
          head_resp_to_pipe(PipeOut, has_outstanding, undefined);
        "save state " ++ StateName ->
          head_resp_to_pipe(PipeOut, save_state, StateName);
        "resume state " ++ StateName ->
          head_resp_to_pipe(PipeOut, resume_state, StateName);
        "remove state " ++ StateName ->
          head_resp_to_pipe(PipeOut, remove_state, StateName);
        "idle true" ->
          head_resp_to_pipe(PipeOut, idle, true);
        "idle false" ->
          head_resp_to_pipe(PipeOut, idle, false)
      end,
      loop(PipeIn, PipeOut);
    {Pipe, eof} ->
      connect_to_pipe_and_loop();
    {task_complete, CompletedTask} ->
      PipeOut ! {self(), {data, task_to_str(CompletedTask)}},
      loop(PipeIn, PipeOut);
    stop ->
      PipeIn ! {self(), close},
      PipeOut ! {self(), close},
      receive
        {_, closed} ->
          remove_pipes(),
          exit(normal)
      end;
    {'EXIT', Pipe, Reason} ->
      remove_pipes(),
      exit(port_terminated)
  end.

list_to_bool(S) ->
  case string:lowercase(S) of
    "true" -> true;
    "false" -> false
  end.

format_task_str(TaskStr) ->
  [ClientId, Await, CPUS, Memory, Storage, Dockerless, Container] =
    string:tokens(TaskStr, ", "), % todo desuck this
  V = [
    {client_id, ClientId},
    {port_pid, self()},
    {await, list_to_bool(Await)},
    {cpus,
      case string:to_float(CPUS) of
         {error,no_float} -> list_to_integer(CPUS);
         {F,_Rest} -> F
      end
    },
    {memory, list_to_integer(Memory)},
    {storage, list_to_integer(Storage)},
    {dockerless, list_to_bool(Dockerless)}
  ],
  io:format("task string formatted~n"),
  V.


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
    "\"stdout=\"" ++ base64:encode(StdOut) ++
    "\"stderr=\"" ++ base64:encode(StdErr) ++
    "\"result=\"" ++ Result ++
    "\"".


to_head(Type, Value) ->
  head_pid() ! {self(), Type, Value},
  io:format("task sent from port to head. Type: "++atom_to_list(Type)++"~n"),
  receive
    Response -> Response
  end.

bs() ->
  list_to_binary(" ").

bn() ->
  list_to_binary("\n").

as_bin(Response) ->
  case Response of
    {new_task_id, ClientId, TaskId} ->
      atom_to_binary(new_task_id, utf8)
      ++ bs()
      ++ list_to_binary(ClientId)
      ++ bs()
      ++ list_to_binary(TaskId)
      ++ bn();
    {task_complete, Task} ->
      atom_to_binary(task_complete, utf8)
      ++ bs()
      ++ list_to_binary(task_to_str(Task))
      ++ bn();
    {task_not_complete, TaskId} ->
      atom_to_binary(task_not_complete, utf8)
      ++ bs()
      ++ list_to_binary(TaskId)
      ++ bn()
  end.


head_resp_to_pipe(Pipe, Type, Value) ->
  Pipe ! as_bin(to_head(Type, Value)).