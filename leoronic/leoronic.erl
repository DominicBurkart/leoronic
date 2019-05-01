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
  case Cores of
    unknown -> % MacOS leaves this unknown
      HeadPid ! {system_info,
        {node(), [
          {total_memory, TotalMemory / 1000000},
          {free_memory, CurrentMemory / 1000000},
          {cores, erlang:system_info(schedulers_online)}
        ]}};
    _ ->
      HeadPid ! {system_info,
        [ {node, node()},
          {total_memory, TotalMemory / 1000000},
          {free_memory, CurrentMemory / 1000000},
          {cores, Cores}
        ]}
  end.