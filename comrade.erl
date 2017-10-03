-module(comrade). %get it? because it's a functional (classless) program?
-export([
         init/0,
         add_shunt/1,
         del_shunt/1,
         system_info/0,
         alert_all/2,
         send_to_all/1,
         get_queue/0
       ]).
-record(state, {mes, data}).

%% alert_all function

alert_all(Content, [One_Node | Remaining_Nodes], Function) ->
  try
    spawn(One_Node, comrade, Function, [Content, justlocal]),
    alert_all(Content, Remaining_Nodes, Function)
  catch
    exit:Exit -> % at least one node disconnected from the network.
      Actually_Remaining =
        [ Nn || Nn <- Remaining_Nodes, lists:member(Nn, nodes())],
      alert_all(Content, Actually_Remaining, Function)
  end;

alert_all(Task, [], Function) ->
  ok.

alert_all(Content, Function) ->
  spawn(comrade, Function, [Content, justlocal]),
  alert_all(Content, nodes(), Function).

%% end alert_all function

%% shunt functions

add_shunt(Task) ->
  [Worker | _ ] = nodes(connected),
  %TODO: deal with file uploading
  spawn(Worker, comrade, add_or_update_task, [Task]).

del_shunt(Task) ->
  [Worker | _ ] = nodes(connected),
  spawn(Worker, comrade, cancel_task, [Task]).

add_tag(Tag) ->
if
  net_kernel:connect_node(list_to_atom("leoronic@"++Host)) =:= false ->

end.

link_to_leoronic() ->
  {ok, Host} = inet:gethostname(),
  if
    net_kernel:connect_node(list_to_atom("leoronic@"++Host)) =:= false ->
      connect(local_ips()) %only connect to other machines if necessary.
  end.

%% end shunt functions

%% file management functions

send_to_all(Filename) ->
  send_file(Filename, nodes()).

send_file(Filename, [One | Remaining]) ->
  try
    Receiver = spawn(One, comrade, receive_file, self()),
    %TODO
    send_file(Filename, Remaining)
  catch
    exit:Exit -> % at least one node disconnected from the network.
      Actually_Remaining =
        [ Nn || Nn <- Remaining_Nodes, lists:member(Nn, nodes())],
      send_file(Filename, Actually_Remaining)
  end;

receive_file(Filename, Pid_source) ->
  ok. %TODO

%% end file management functions


%% work queue functions

consider_queue() -> %evaluates whether to start a new task.
  ok. %TODO

add_or_update_task(Task) ->
  alert_all(Task, add_or_update_task).

add_or_update_task(Task, justlocal) ->
  to_server(add_or_update, Task).

cancel_task(Task) ->
  alert_all(Task, cancel_task).

cancel_task(Task, justlocal) ->
  to_server(cancel, Task).

to_server(Atm, Tsk) ->
  {ok, H} = inet:gethostname(),
  {leoronic_local_pid, list_to_atom("leoronic@"++H)} ! {Atm, Tsk}.

sync_queue() ->
  ok. %TODO

queue_to_csv() ->
  table_to_csv(local_queue, none).

table_to_csv(Atm, Cond) ->
  case Cond of
    none ->
      ok; %TODO
    complete ->
      ok %TODO
  end.

get_queue() ->
  ok. %TODO

%% end work queue functions


% miscellaneous functions

system_info() ->
  application:start(sasl),
  application:start(os_mon),
  [{_, _}, {_, Current_Memory}, {_, _}] = memsup:get_system_memory_data(),
  [{_, Size, Perc}] = disksup:get_disk_data(),
  application:stop(os_mon),
  application:stop(sasl),

  Cores = erlang:system_info(logical_processors_available),
  if
    Cores =:= unknown -> % MacOS leaves this unknown
      [Current_Memory, erlang:system_info(schedulers_online)]; %default: # cores
    true ->
      [Current_Memory, Cores]
  end.

run(CommandString, justlocal) ->
  spawn(os, cmd, [CommandString]).

run_on(Node, CommandString) ->
  spawn(Node, os, cmd, [CommandString]).

run_on_all(CommandString) ->
  alert_all(run, CommandString).

% end miscellaneous functions


%% node initialization and general behavior

local_ips() ->
%collects all IPV4 address on the network, excluding that of this computer.
%from https://stackoverflow.com/questions/32984215/erlang-finding-my-ip-address
    {ok, Addrs} = inet:getifaddrs(),
    {ok, [{ThisIP, _, _}, _ ]} = inet:getif(),
    [
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         Addr =/= {127,0,0,1},
         Addr =/= ThisIP,
         size(Addr) == 4
    ].

search_for_other_workers() ->
  connect(local_ips()).

connect([IP | T]) ->
  {_, Hostent} = inet:gethostbyaddr(IP),
  [Hostname] = element(2, Hostent),
  net_kernel:connect_node(list_to_atom("leoronic@" ++ Hostname)),
  %^ we make a new atom for each non-local address on the network (constrained).
  connect(T).

connect([]) ->
  ok;

init() ->
  register(leoronic_local_pid, self()),
  ets:new(local_queue, [set, named_table]),
  ets:new(local_resources, [set, named_table]),
  io:format("Searching for other workers on the LAN."),
  spawn(search_for_other_workers),
  io:format("Initialization complete. Ready for tasks."),
  loop(#state{mes = orddict:new(),
              data = orddict:new()}).

loop(S=#state{}) ->
  receive

    {add_or_update, Task} ->
      ets:insert(local_queue, Task),
      spawn(consider_queue),
      loop(S);

    {cancel, Task} ->
      ets:delete(local_queue, Task),
      spawn(consider_queue),
      loop(S);

    {has, Local_Resource, pID} ->
      case ets:lookup(local_resources, Local_Resource) of
          [] ->
            ok,
          [in_progress] ->
            ok,
          Found ->
            pID ! {requested_resource_at, node(), Local_Resource}
      end,
      loop(S);

    {requested_resource_at, Node, Local_Resource} ->
      if ets:lookup(local_resources, Local_Resource) =:= [] ->
        spawn(Node, comrade, send_resource, node()),
        ets:insert(local_resources, {Local_Resource, in_progress})
      end,
      loop(S);

    {shutdown} ->
      io:format("saving local queue as csv.~n"),
      table_to_csv(local_queue, none),
      io:format("saving complete. No longer monitoring / starting tasks.~n"),
      io:format("shutting down erlang node.~n"),
      ok

    {write_local_queue} ->
      table_to_csv(local_queue, none),
      loop(S)

    Unknown ->
      io:format("Unknown message: ~p~n",[Unknown]),
      loop(S)

  end.

%% end node initialization and general behavior
