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
-export([start/0, stop/0, init/0, list_to_bool/1]).
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
  register(leoronic_port, self()),
  process_flag(trap_exit, true),
  make_pipes(),
  connect_to_pipe_and_loop().

pipe_cmd(Cmd) when is_list(Cmd) ->
  [os:cmd(Cmd ++ " " ++ pipe_name(Pipe)) || Pipe <- [in, out]].

make_pipes() ->
  pipe_cmd("mkfifo").

remove_pipes() ->
  pipe_cmd("rm").

connect_to_pipe_and_loop() ->
  loop(
    open_port(pipe_name(in), [eof, in]),
    open_port(pipe_name(out), [eof, out])
  ).

loop(PipeIn, PipeOut) ->
  receive
    {_PipeIn, {data, Str}} ->
      case Str of
        "add task " ++ TaskStr ->
          io:format("L: task received"),
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
          head_resp_to_pipe(PipeOut, idle, false);
        Unknown ->
          io:format("bad input to port: ~p~n", [Unknown])
      end,
      loop(PipeIn, PipeOut);
    {Pipe, eof} ->
      connect_to_pipe_and_loop();
    {task_complete, CompletedTask} ->
      io:format("L: returning task in port loop"),
      port_command(PipeOut,
        [
          atom_to_binary(task_complete, utf8),
          bs(),
          list_to_binary(task_to_str(CompletedTask)),
          bn()
        ]),
      loop(PipeIn, PipeOut);
    stop ->
      io:format("Received STOP in port~n"),
      PipeIn ! {self(), close},
      PipeOut ! {self(), close},
      receive
        {_, closed} ->
          remove_pipes(),
          exit(normal)
      end;
    {'EXIT', Pipe, Reason} ->
      io:format("Received EXIT in port. Reason: ~p~n", [Reason]),
      connect_to_pipe_and_loop()
%%      remove_pipes(),
%%      exit(port_terminated)
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
        {error, no_float} -> list_to_integer(CPUS);
        {F, _Rest} -> F
      end
    },
    {memory, list_to_integer(Memory)},
    {storage, list_to_integer(Storage)},
    {dockerless, list_to_bool(Dockerless)},
    {container, string:replace(base64:decode_to_string(Container), "/n", "~n")}
  ],
  V.


task_to_str(Task) ->
  [
    {id, ID},
    {has_run, _has_run},
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

  "id=\"" ++ utils:number_to_list(ID) ++
    "\"created_at=\"" ++ utils:number_to_list(CreatedAt) ++
    "\"started_at=\"" ++ utils:number_to_list(StartedAt) ++
    "\"finished_at=\"" ++ utils:number_to_list(FinishedAt) ++
    "\"stdout=\"" ++ binary_to_list(base64:encode(StdOut)) ++
    "\"stderr=\"" ++ binary_to_list(base64:encode(StdErr)) ++
    "\"result=\"" ++ Result ++
    "\"".

bs() ->
  list_to_binary(" ").

bn() ->
  list_to_binary("\n").

head_resp_to_pipe(Pipe, Type, Value) ->
  head_pid() ! {self(), Type, Value},
  receive
    {new_task_id, ClientId, TaskId} ->
      port_command(
        Pipe,
        [
          atom_to_binary(new_task_id, utf8),
          bs(),
          list_to_binary(ClientId),
          bs(),
          list_to_binary(integer_to_list(TaskId)),
          bn()
        ]);
    {task_complete, Task} ->
      io:format("L: returning task"),
      port_command(
        Pipe,
        [
          atom_to_binary(task_complete, utf8),
          bs(),
          list_to_binary(task_to_str(Task)),
          bn()
        ]),
      io:format("L: task returned");
    {task_not_complete, TaskId} ->
      port_command(
        Pipe,
        [
          atom_to_binary(task_not_complete, utf8),
          bs(),
          list_to_binary(integer_to_list(TaskId)),
          bn()
        ]
      )
  end.