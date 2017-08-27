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

add_shunt(alskdjf) ->
  ok. %todo

del_shunt(dsflgkj) ->
  ok. %todo

consider_queue() -> %evaluates whether to start a new task.
  ok. %todo

add_or_update_task(Task) ->
  alert_all(Task, add_or_update_task).

add_or_update_task(Task, justlocal) ->
  ets:insert(local_queue, Task).

sync_queue() ->
  ok. %todo

queue_completes_to_csv() ->
  ok. %todo

%% end work queue functions


% miscellaneous functions

system_info() ->
  ok. %todo

run(CommandString, justlocal) ->
  spawn(os, cmd, [CommandString]).

run_on(Node, CommandString) ->
  spawn(Node, os, cmd, [CommandString]).

run_on_all(CommandString) ->
  alert_all(run, CommandString).

% end miscellaneous functions


%% node initialization and general behavior

init() ->
  os:putenv("leoronic_local_pid", os:getpid()),
  ets:new(local_queue, [set, local_queue]), %not right yet
  loop(#state{mes = orddict:new()
              data = orddict:new()}).

loop(S=#state{}) -> %actually runs the
  receive
    {add, Task} -> %now redundant
      ets:insert(local_queue, Task),
      consider_queue(),
      loop(S);


    Unknown ->
      io:format("Unknown message: ~p~n",[Unknown]),
      loop(S)
  end.

%% end node initialization and general behavior
