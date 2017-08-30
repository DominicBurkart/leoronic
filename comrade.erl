-module(comrade). %get it? because it's a functional (classless) program?
-export([
         init/0,
         add_shunt/1,
         del_shunt/1,
         system_info/0,
         alert_all/2
       ]).
-record(state, {mes, data}).

%% alert_all function

alert_all(Content, [One_Node | Remaining_Nodes], Function) ->
  try
    spawn(One_Node, comrade, Function, [Content, justlocal]),
    alert_all(Content, Remaining_Nodes, Function)
  catch
    exit:Exit ->
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


%% work queue functions

add_shunt(Task) ->
  [Worker] = nodes(connected),
  %todo: deal with file uploading
  spawn(Worker, comrade, add_or_update_task, [Task]).

cancel_shunt(Task) ->
  [Worker] = nodes(connected),
  spawn(Worker, comrade, cancel_task, [Task]).

link_to_leoronic() ->
  {ok, Host} = inet:gethostname(),
  if net_kernel:connect_node(list_to_atom("leoronic@"++Host)) =:= false:
    io:format("Link failed: Leoronic is not running on local host " ++ Host)
  end.

consider_queue() -> %evaluates whether to start a new task.
  ok. %todo

add_or_update_task(Task) ->
  alert_all(Task, add_or_update_task).

add_or_update_task(Task, justlocal) ->
  os:getenv("leoronic_local_pid") ! {add_or_update, Task}.

cancel_task(Task) ->
  alert_all(Task, cancel_task).

cancel_task(Task, justlocal) ->
  os:getenv("leoronic_local_pid") ! {cancel, Task}.

sync_queue() ->
  ok. %todo

queue_completes_to_csv() ->
  ok. %todo

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

connect([]) ->
  ok;

connect([IP | T]) ->
  {_, Hostent} = inet:gethostbyaddr(IP),
  [Hostname] = element(2, Hostent),
  net_kernel:connect_node(list_to_atom("leoronic@" ++ Hostname)),
  %^ we make a new atom for each non-local address on the network (constrained).
  connect(T).

init() ->
  os:putenv("leoronic_local_pid", os:getpid()),
  ets:new(local_queue, [set, named_table]),
  ets:new(local_resources, [set, named_table]),
  spawn(search_for_other_workers),
  loop(#state{mes = orddict:new(),
              data = orddict:new()}).

loop(S=#state{}) ->
  receive

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

    {add_or_update, Task} ->
      ets:insert(local_queue, Task),
      consider_queue(),
      loop(S);

    Unknown ->
      io:format("Unknown message: ~p~n",[Unknown]),
      loop(S)
  end.

%% end node initialization and general behavior
