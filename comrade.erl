-module(comrade). %get it? because it's a functional (classless) program?
-export([
         init/0,
         add_tag/1,
         del_tag/1,
         has_tag/1,
         has_tag/2,
         system_info/0,
         alert_all/2,
         send_file/2,
         send_to_all/1,
         send_to_all_with_tag/2,
         get_queue/0,
         in_leoronic/1
       ]).
-record(state, {mes, data}).

%% node initialization and general behavior

init() ->
  register(leoronic_local_pid, self()),
  ets:new(local_queue, [set, named_table]),
  ets:new(local_resources, [set, named_table]),
  ets:new(node_tags, [set, named_table]),
  io:format("Beginning search for other workers on the LAN."),
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
          [] -> ok;
          [in_progress] -> ok;
          Found -> pID ! {requested_resource_at, node(), Local_Resource}
      end,
      loop(S);

    {requested_resource_at, Node, Local_Resource} ->
      case ets:lookup(local_resources, Local_Resource) of  [] ->
        spawn(Node, comrade, send_resource, node()),
        ets:insert(local_resources, {Local_Resource, in_progress})
      end,
      loop(S);

    {shutdown} ->
      io:format("saving local queue as csv.~n"),
      table_to_csv(local_queue, none),
      io:format("saving complete. No longer monitoring / starting tasks.~n"),
      io:format("shutting down erlang node.~n"),
      ok;

    {write_local_queue} ->
      table_to_csv(local_queue, none),
      loop(S);

    Unknown ->
      io:format("Unknown message: ~p~n",[Unknown]),
      loop(S)

  end.

%% end node initialization and general behavior

%% alert_all function

to_server(Atm, Tsk) ->
  {ok, H} = inet:gethostname(),
  {leoronic_local_pid, list_to_atom("leoronic@"++H)} ! {Atm, Tsk}.

alert_all(Content, [One_Node | Remaining_Nodes], Function) ->
  try
    spawn(comrade, alert_all, [Content, Remaining_Nodes, Function]),
    try_alert(One_Node, Function, Content, 1) %throws error if node DNE
  catch
    exit:Exit -> % at least one node disconnected from the network.
      Actually_Remaining =
        [ Nn || Nn <- Remaining_Nodes, lists:member(Nn, nodes())],
      alert_all(Content, Actually_Remaining, Function)
  end;

alert_all(Task, [], Function) ->
  ok.

alert_all(Content, Function) ->
  spawn(comrade, Function, [Content, self()]),
  alert_all(Content, nodes(), Function).

try_alert(Node, Function, Content, NumTries) ->
  spawn(Node, comrade, Function, [Content, self()]),
  receive
    confirmed ->
      ok
    after 30000 ->
      if NumTries < 10 ->
        io:format("~nUnresponsive node: "),
        io:format(Node),
        try_alert(Node, Function, Content, NumTries + 1)
      end,
      if true ->
        io:format("~nNode did not respond after 10 retries over 5 minutes: "),
        io:format(Node),
        ok
      end
  end.

%% end alert_all function

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

%% file management functions

send_to_all(Filename) ->
  send_file(Filename, nodes()).

send_to_all_with_tag(Filename, Tag) ->
  send_file(Filename, [ N || N <- nodes(), has_tag(Tag, N)]).

send_file(Filename, [One | Remaining]) ->
  try
    Receiver = spawn(One, comrade, receive_file, self()),
    %TODO
    send_file(Filename, Remaining)
  catch
    exit:Exit -> % lost connection to at least one node.
      Actually_Remaining =
        [ Nn || Nn <- Remaining, lists:member(Nn, nodes())],
      send_file(Filename, Actually_Remaining)
  end.

receive_file(Filename, Pid_source) ->
  ok. %TODO

%% end file management functions

%% misc functions

run_on(Node, CommandString) ->
  spawn(Node, os, cmd, [CommandString]).

run_on_all(CommandString) ->
  alert_all(CommandString, run).

has_tag(Tag, Node) ->
  ok. %TODO

add_tag(Tag) -> shared:add_tag(Tag). %add tag to node
del_tag(Tag) -> shared:add_tag(Tag). %delete tag from node
has_tag(Tag) -> shared:add_tag(Tag). %check if node has tag (returns True or False)
tags() -> shared:tags(). %return all tags for a node.
system_info() -> shared:system_info(). %returns [Current_Memory (bytes), Cores (int)]
run(CommandString) -> shared:run(CommandString).
in_leoronic(CommandString) -> shared:in_leoronic(CommandString). %run erlang in leoronic
%% end misc functions
