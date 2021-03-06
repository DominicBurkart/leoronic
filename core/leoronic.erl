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
run_container/3
]).
-import(utils, [select/2, sub/2]).
-export([
  start/0,
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  code_change/3,
  perform_task/1,
  add_child_process/3,
%%  housekeeping/2,
  stop/1,
  alert_new_node/0,
  spawn_kid/3,
  loop_check_should_be_head/0,
  system_info/0,
  send_system_info/1
  ]).
-record(state, {kids=none}).
-type state() :: #state{}.


-dialyzer({nowarn_function, perform_task_internal/1}).
-dialyzer({nowarn_function, handle_cast/2}).

%%% API

start() ->
  gen_server:start(?MODULE, make_state(), []).

start_link() ->
  gen_server:start_link(?MODULE, make_state(), []).

add_child_process(Mod, Fun, Args) ->
  gen_server:call(?MODULE, {add_child, Mod, Fun, Args}).

%%housekeeping(Pid, Cmd) ->
%%  gen_server:call(Pid, Cmd).

send_system_info(Pid) ->
  Info = system_info(),
  Pid ! Info.

system_info() ->
  gen_server:call(?MODULE, send_system_info).

perform_task(Task) ->
  gen_server:cast(?MODULE, {perform_task, Task}).

stop(Pid) ->
  gen_server:call(Pid, stop).

%%% Server functions

make_state() ->
  #state { kids=ets:new(kids, [set, public, named_table]) }.

init(State) -> % todo where to declare process_flag(trap_exit, true),
  application:start(sasl),
  application:start(os_mon),
  register(leoronic, self()),
  spawn_kid(leoronic, alert_new_node, []),
  spawn_kid(port, start, []),
  case check_should_be_head() of
    is_head ->
      ok;
    not_head ->
      spawn_kid(leoronic, loop_check_should_be_head, [])
  end,
  {ok, State}.

handle_call({add_child, Mod, Fun, Args}, _From, State) ->
  spawn_kid(Mod, Fun, Args),
  {reply, ok, State};

handle_call(send_system_info, _From, State) ->
  {reply, send_system_info(), State};

handle_call(new_node_initialized, _From, _State) ->
  spawn_kid(leoronic, check_should_be_head, []),
  {no_reply};

handle_call(prune_kids, _From, _State) ->
  ets:delete(kids, [{Kid} || {Kid} <- ets:tab2list(kids), whereis(Kid) =:= undefined]),
  {no_reply};

handle_call(stop, _From, _State) ->
  [Kid ! stop || {Kid} <- ets:tab2list(kids)],
  application:stop(os_mon),
  application:stop(sasl),
  io:format("leoronic main exiting"),
  exit(normal),
  {no_reply}.

handle_cast({perform_task, Task}, State) ->
  CompletedTaskInfo = perform_task_internal(Task),
  io:format("task performed"),
  head:head_pid() ! {self(), task_complete, CompletedTaskInfo},
  {noreply, State}.

code_change(_, State, _) ->
  {ok, State}.

%%% Internals


perform_task_internal(Task) ->
  [{id, TaskId}] = sub([id], Task),
  Ran = run_container(
    select(container, Task),
    sub([memory, storage, cpus], Task),
    integer_to_list(TaskId)
  ),
  impute_task_values(Task, Ran).


send_system_info() -> % yields memory in MB
  MemoryData = memsup:get_system_memory_data(),
  TotalMemory = utils:select(total_memory, MemoryData),
  CurrentMemory = utils:select(free_memory, MemoryData),

  Cores = erlang:system_info(logical_processors_available),

  {
    system_info,
    [
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
    ]
  }.


spawn_kid(Mod, Fun, Args) ->
  case Fun of
    start ->
      register(Mod, spawn(Mod, Fun, Args));
    init ->
      register(Mod, spawn(Mod, Fun, Args));
    _ ->
      register(Fun, spawn(Mod, Fun, Args))
  end.
%%  case ets:lookup(kids, Fun) of
%%    [] ->
%%      io:format("Function "++atom_to_list(Fun)++" being added to kids table."),
%%      ets:insert(kids, {Fun});
%%    _ ->
%%      io:format("Duplicate version of function "++atom_to_list(Fun)++" running...")
%%  end.


check_should_be_head() ->
  case head:should_be_head() of
    true ->
      case whereis(head) of
        undefined ->
          spawn_kid(head, head, []),
          is_head;
        _ ->
          is_head
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

impute_task_values_helper(Old, [{UpdatingKey, UpdatingValue}]) ->
  lists:keyreplace(UpdatingKey, 1, Old, {UpdatingKey, UpdatingValue});

impute_task_values_helper(Old, [{UpdatingKey, UpdatingValue} | New]) ->
  impute_task_values_helper(
   lists:keyreplace(UpdatingKey, 1, Old, {UpdatingKey, UpdatingValue}),
    New
  ).

impute_task_values(Task, Values) ->
  impute_task_values_helper(Task, [{has_run, true} | Values]).
