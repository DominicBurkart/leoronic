%%%-------------------------------------------------------------------
%%% @author dominic burkart
%%% @doc
%%% Leoronic: cluster management.
%%% @end
%%% Created : 25. avr. 2019 19:22
%%%-------------------------------------------------------------------
-module(leoronic).
-author("dominicburkart").

%% API
-import(
link_to_leoronic, [
link_to_leoronic/0
]).
-import(run_container, [
run_container/2
]).
-import(head, [
head_pid/0, should_be_head/1, head/0
]).
-import(utils, [select/2, sub/2]).
-export([
  perform_task/1,
  send_system_info/1
]).
-export([start/0, stop/0, init/0]).


start() ->
  spawn(leoronic, init, []).


stop() ->
  leoronic ! stop.


init() ->
  register(leoronic, self()),
  process_flag(trap_exit, true),
  spawn(node(), leoronic, alert_new_node, []),
  ets:new(kids, [set, named_table]),
  case check_should_be_head(node()) of
    true ->
      spawn(node(), head, head, []);
    false ->
      register(loop_head_check, spawn(node(), leoronic, loop_check_should_be_head, [])),
      ets:insert(kids, {loop_head_check})
  end,
  loop().


loop() ->
  receive
    {new_node_initialized} ->
      spawn(node(), leoronic, check_should_be_head, []),
      loop();
    stop ->
      [Kid ! stop || {Kid} <- ets:tab2list(kids)],
      exit(normal)
  end.


check_should_be_head() ->
  case should_be_head(node()) of
    true ->
      case whereis(head) of
        undefined ->
          spawn(node(), head, head, []),
          is_head;
        _ ->
          not_head
      end;
    false ->
      not_head
  end.


loop_check_should_be_head() ->
  receive
    stop ->
      ok
  after 1000 * 60 * 2 ->
    case check_should_be_head() of
      is_head ->
        ok;
      not_head ->
        loop_check_should_be_head()
    end
  end.


alert_new_node() ->
  [{leoronic, N} ! new_node_initialized || N <- nodes()].


impute_task_values_helper(Old, []) ->
  Old;


impute_task_values_helper(Old, [[UpdatingKey, UpdatingValue] | New]) ->
  impute_task_values_helper(lists:keyreplace(UpdatingKey, 1, Old, UpdatingValue), New).


impute_task_values(Task, Values) ->
  impute_task_values_helper(Task, [Values | {has_run, true}]).


perform_task(Task) ->
  head_pid() !
    impute_task_values(
      Task,
      run_container(
        select(container, Task),
        sub([memory, storage, cpus], Task)
      )
    ).

send_system_info(HeadPid) -> % yields memory in MB
  application:start(sasl),
  application:start(os_mon),
  [{_, TotalMemory}, {_, CurrentMemory}, {_, _}] = memsup:get_system_memory_data(),
  application:stop(os_mon),
  application:stop(sasl),

  Cores = erlang:system_info(logical_processors_available),
  CoreReport =
    case Cores of
      unknown -> % MacOS leaves this unknown
        {cores, erlang:system_info(schedulers_online)};
      _ ->
        {cores, Cores}
    end,
  HeadPid ! {
    system_info,
    {[
      {node, node()},
      {total_memory, TotalMemory / 1000000},
      {free_memory, CurrentMemory / 1000000},
      CoreReport
    ]}
  }.
