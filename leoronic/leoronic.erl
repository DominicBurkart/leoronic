%%%-------------------------------------------------------------------
%%% @author dominic burkart
%%% @doc
%%% Leoronic: cluster management.
%%% @end
%%% Created : 25. avr. 2019 19:22
%%%-------------------------------------------------------------------
-module(leoronic).
-behavior(gen_server).
-author("dominicburkart").

%% API
-import(link_to_leoronic, [
link_to_leoronic/0
]).
-import(run_container, [
run_container/2
]).
-import(utils, [select/2, sub/2]).
-export([
  start/0,
  start_link/0,
  init/1,
  perform_task/1,
  add_child_process/3,
  housekeeping/2,
  perform_task/2,
  stop/1,
  alert_new_node/0
  ]).
-record(state, {kids=none}).
-type state() :: #state{}.


-dialyzer({nowarn_function, perform_task/1}).

%%% API

start() ->
  gen_server:start(?MODULE, make_state(), []).

start_link() ->
  gen_server:start_link(?MODULE, make_state(), []).

add_child_process(Mod, Fun, Args) ->
  gen_server:call(self(), {add_child, {Mod, Fun, Args}}).

housekeeping(Pid, Cmd) ->
  gen_server:call(Pid, Cmd).

perform_task(Pid, Task) ->
  gen_server:cast(Pid, {perform_task, Task}).

stop(Pid) ->
  gen_server:call(Pid, stop).

%%% Server functions

make_state() ->
  #state { kids=ets:new(kids, [set, public, named_table]) }.

init(State) -> % todo where to declare process_flag(trap_exit, true),
  spawn_kid(leoronic, alert_new_node, []),
  case check_should_be_head() of
    is_head ->
      spawn_kid(head, head, []);
    not_head ->
      spawn_kid(leoronic, loop_check_should_be_head, [])
  end,
  {ok, State}.

handle_cast({perform_task, Task}, _State) ->
  CompletedTaskInfo = perform_task(Task),
  head:head_pid() ! CompletedTaskInfo,
  % todo is there a better way to do this? sending head might not be receiving head
  {noreply}.

handle_call(send_system_info, _From, State) ->
  {reply, send_system_info(), State};

handle_call(new_node_initialized, _From, _State) ->
  spawn_kid(leoronic, check_should_be_head, []),
  {no_reply};

handle_call({add_child, {Mod, Fun, Args}}, _From, _State) ->
  spawn_kid(Mod, Fun, Args),
  {no_reply};

handle_call(prune_kids, _From, _State) ->
  ets:delete(kids, [{Kid} || {Kid} <- ets:tab2list(kids), whereis(Kid) =:= undefined]),
  {no_reply};

handle_call(stop, _From, _State) ->
  [Kid ! stop || {Kid} <- ets:tab2list(kids)],
  exit(normal),
  {no_reply}.

code_change(_, State, _) ->
  {ok, State}.

%%% Internals


perform_task(Task) ->
  [TaskId] = sub([id], Task),
  impute_task_values(
    Task,
    run_container(
      select(container, Task),
      sub([memory, storage, cpus], Task),
      TaskId
    )
  ).


send_system_info() -> % yields memory in MB
  application:start(sasl),
  application:start(os_mon),
  [{_, TotalMemory}, {_, CurrentMemory}, {_, _}] = memsup:get_system_memory_data(),
  application:stop(os_mon),
  application:stop(sasl),

  Cores = erlang:system_info(logical_processors_available),

  {
    system_info,
    {[
      {node, node()},
      {total_memory, TotalMemory / 1000000},
      {free_memory, CurrentMemory / 1000000},
      {cores,
        case Cores of
          unknown -> % MacOS leaves this unknown
            erlang:system_info(schedulers_online);
          _ ->
            Cores
        end
      }
    ]}
  }.


spawn_kid(Mod, Fun, Args) ->
  register(Fun, spawn(Mod, Fun, Args)),
  ets:insert(kids, {Fun}).


check_should_be_head() ->
  case head:should_be_head(node()) of
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
  impute_task_values_helper(Task, [{has_run, true} | Values]).