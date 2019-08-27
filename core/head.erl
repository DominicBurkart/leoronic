%%%-------------------------------------------------------------------
%%% @author dominicburkart
%%% @doc
%%%
%%% @end
%%% Created : 27. avr. 2019 08:21
%%%-------------------------------------------------------------------
-module(head).
-author("dominicburkart").

%% API
-import(utils, [select/2, indexed/1, not_in_match_specification/2]).

-export([heads/0, should_be_head/0, head/0, head_pid/0, job_checker/0, job_checker_scheduler/2, evaluate_worker_responses/3, evaluate_worker_responses/5, ask_for_params/1]).


heads() -> [{head, Node} || Node <- nodes(), lists:member(Node, lists:sublist(sorted_nodes(), number_of_heads()))].


number_of_heads() ->
  case length(sorted_nodes()) of
    1 ->
      1;
    N when N >= 2; N < 10 ->
      2;
    N when N >= 10; N < 100 ->
      math:ceil(N * 0.2);
    N when N >= 100 ->
      math:ceil(N * 0.1)
  end.


sorted_nodes() ->
  Sorted = lists:sort(nodes()),
  case Sorted of
    [] ->
      [node()];
    _ ->
      Sorted
  end.


should_be_head() ->
  whereis(leoronic_port) /= undefined orelse
    lists:member(node(), lists:sublist(sorted_nodes(), number_of_heads())).


head_processes(Nodes) ->
  IndexedNodes = indexed(Nodes),
  NHeads = number_of_heads(),
  HeadIndexForEachNode =
    lists:seq(1, NHeads) ++ % each head node is its own head.
    [I rem NHeads + 1 || I <- lists:seq(NHeads + 1, length(Nodes))], %% non-heads are assigned a head
  [{head, element(2, lists:keyfind(I, 1, IndexedNodes))} || I <- HeadIndexForEachNode].


worker_head_tuples() ->
  Nodes = sorted_nodes(),
  lists:zip(head_processes(Nodes), Nodes).


worker_pids(Head) ->
  [Worker || {Head1, Worker} <- worker_head_tuples(), Head == Head1].


head_pid() ->
  element(1, lists:keyfind(node(), 2, worker_head_tuples())).


% todo refactor so that the dockerfile, stdout, stderr, and result all have their own tables.
head() ->
  ets:new(task_instructions, [set, named_table, public]),
  ets:new(running_tasks, [set, named_table, public]),
  ets:new(completed_tasks, [set, named_table, public]),
  ets:new(params, [set, named_table, public]),
  Now = os:system_time(1),
  leoronic:add_child_process(
    head,
    job_checker_scheduler,
    [Now, Now]
  ),
  leoronic:add_child_process(
    head,
    ask_for_params,
    [self()]
  ),
  loop().


ask_for_params(ReturnPid) ->
  case [Node || Node <- sorted_nodes(), Node /= node()] of
    [FirstOtherNode | _] ->
      case lists:member(FirstOtherNode, lists:sublist(sorted_nodes(), number_of_heads())) of
        false ->
          no_other_head;
        true ->
          {head, FirstOtherNode} ! {ReturnPid, get_params, undefined}
      end;
    [] ->
      no_other_nodes
  end.


ask_for_worker_info([], _ReturnPid) ->
  ok;

ask_for_worker_info([Worker | Workers], ReturnPid) ->
  spawn(Worker, leoronic, send_system_info, [ReturnPid]), % todo ideally would be in the appropriate kids table.
  ask_for_worker_info(Workers, ReturnPid).


match_available_to_requested_helper(Available, Requested, FoundMatches) ->
  [SmallestA | RestAvailable] = Available,
  [BiggestR | RestRequested] = Requested,
  IsMatch = (element(1, SmallestA) > element(1, BiggestR))
    and (element(2, SmallestA) > element(2, BiggestR)),
  case IsMatch of % todo this should be a binary search
    true ->
      match_available_to_requested(
        RestAvailable,
        RestRequested,
        [{element(3, SmallestA), element(3, BiggestR)} | FoundMatches]
      );
    false ->
      match_available_to_requested(RestAvailable, Requested, FoundMatches)
  end.


match_available_to_requested(Available, Requested, FoundMatches) ->
  case {Available, Requested} of
    {[], _} ->
      FoundMatches;
    {_, []} ->
      FoundMatches;
    _ ->
      match_available_to_requested_helper(Available, Requested, FoundMatches)
  end.


reformat_response(Response) ->
  [
    {node, N},
    {total_memory, TM},
    {free_memory, FM},
    {cores, Cores}
  ] = Response,
  Buffer =
    case FM of
      FM when FM >= 4 * 1024 ->
        TM * 0.25;
      FM when FM < 4 * 1024 ->
        FM * 0.5 % only use half of memory for low memory systems.
    end,
  {FM - Buffer, Cores, N}.

best_viable(Responses, Tasks) ->
  Available = [reformat_response(Response) || Response <- Responses],
  Requested = lists:reverse(
    lists:sort(
      [{select(memory, Task), select(cpus, Task), Task} || {_taskId, Task} <- Tasks]
    )
  ), % biggest first
  case match_available_to_requested(Available, Requested, []) of
    [] -> undefined;
    Matches -> Matches
  end.

evaluate_worker_responses(ReturnPid, Tasks, NumWorkers) ->
  evaluate_worker_responses([], 0, NumWorkers, ReturnPid, Tasks).

evaluate_worker_responses(Responses, Received, Total, ReturnPid, Tasks) ->
  receive
    {system_info, Response} ->
      NewResponses = [Response | Responses],
      case Received + 1 of
        Total ->
          ReturnPid ! {final, best_viable(NewResponses, Tasks)};
        _ ->
          evaluate_worker_responses(NewResponses, Received + 1, Total, ReturnPid, Tasks)
      end;
    {eager_eval} ->
      ReturnPid ! {eager_eval, best_viable(Responses, Tasks)}
  end.

get_runnable_tasks() ->
  Tasks = n_next_tasks(length(sorted_nodes()) * 3), % todo the const int on this line should be exposed as a parameter
  case Tasks of
    [] ->
      undefined;
    _ ->
      Workers = worker_pids(head_pid()),
      Evaluator = spawn(node(), head, evaluate_worker_responses, [self(), Tasks, length(Workers)]), % todo set as child
      ask_for_worker_info(Workers, Evaluator),
      receive
        {final, FinalResponse} -> FinalResponse
      after 2 * 1000 ->
        Evaluator ! {eager_eval},
        receive
          {final, Response} -> Response;
          {eager_eval, Response} -> Response
        end
      end
  end.

start_jobs([]) ->
  ok;

start_jobs([{Node, Task} | OtherJobs]) ->
  Id = select(id, Task),
  ets:delete(task_instructions, Id),
  ets:insert(running_tasks, {Id, Task}),
  head_pid() ! {spawn(Node, leoronic, perform_task, [Task]), running_task, Task},
  start_jobs(OtherJobs).


job_checker() ->
  job_checker(-1, -1),
  {job_checker_scheduler, node()} ! {ran}.

prune_running_task_record() ->
  ok. % todo
%%  RunningTasks = ets:tab2list(running_tasks),
%%  Infos = [
%%    {Task, erlang:process_info(utils:select(pid, Task), status)} ||
%%    {_TaskId, Task} <- RunningTasks
%%  ],
%%  RanOrErroredTasks = [
%%    Task || {Task, _} <- lists:filter(
%%      fun(E) ->
%%        element(2, E) =:= undefined
%%      end,
%%      Infos
%%    )
%%  ],
%%  [head ! {select(id, Task), task_complete, Task} || Task <- RanOrErroredTasks].

job_checker(LastRan, LastIdle) ->
  prune_running_task_record(),
  case {LastRan, LastIdle} of
    {-1, -1} ->
      job_checker_scheduler ! {self(), get_times},
      receive
        {times, RealLastRan, RealLastIdle} ->
          job_checker(RealLastRan, RealLastIdle)
      end;
    _ ->  % todo don't run this more than once a second or so
      % todo check for timed-out tasks / idling here
      case get_runnable_tasks() of
        undefined ->
          ok;
        Pairs ->
          start_jobs(Pairs)
      end
  end.


job_checker_scheduler(LastRan, LastIdle) -> % todo why do we need the last idle
  receive
    {idle} -> ok; % todo
    {ran} ->
      job_checker_scheduler(os:system_time(1), LastIdle);
    {ReturnPid, get_times} ->
      ReturnPid ! {times, LastRan, LastIdle},
      job_checker_scheduler(LastRan, LastIdle)
  after 1000 * 60 * 2 ->
    job_checker(LastRan, LastIdle),
    Now = os:system_time(1),
    job_checker_scheduler(Now, LastIdle)
  end.


make_id() ->
  (os:system_time(1000) * 10000) + rand:uniform(10000). % todo what is a better way to generate these?

n_next_tasks(Found, Last, N) ->
  case N of
    0 ->
      Found;
    _ ->
      case ets:next(task_instructions, Last) of
        '$end_of_table' ->
          Found;
        Key ->
          Task = ets:lookup(task_instructions, Key),
          n_next_tasks(Task ++ Found, Key, N - 1)
      end
  end.

n_next_tasks(N) ->
  case ets:first(task_instructions) of
    '$end_of_table' ->
      [];
    Key ->
      Task = ets:lookup(task_instructions, Key),
      n_next_tasks(Task, Key, N - 1)
  end.


loop() ->
  spawn(head, job_checker, []),
  receive
    {ReturnPid, add_task, PartialTask} ->
      Id = make_id(),
      [
        {client_id, ClientId},
        {port_pid, _PortPid},
        {await, Await},
        {cpus, CPUS},
        {memory, Memory},
        {storage, Storage},
        {dockerless, Dockerless},
        {container, Container}
      ] = PartialTask,
      RespondTo =
        case Await of
          false -> undefined;
          true -> ReturnPid
        end,
      Task = [
        {id, Id},
        {has_run, false},
        {respond_to, RespondTo},
        {created_at, os:system_time(1)},
        {started_at, undefined},
        {finished_at, undefined},
        {dockerless, Dockerless},
        {memory, Memory},
        {storage, Storage},
        {cpus, CPUS},
        {stdout, undefined},
        {stderr, undefined},
        {result, undefined},
        {container, Container}
      ],
      ets:insert(task_instructions, {Id, Task}),
      ReturnPid ! {new_task_id, ClientId, Id},
      loop();

    {TaskPid, running_task, Task} ->
      ets:insert(running_tasks, {select(id, Task), Task ++ [{pid, TaskPid}]}),
      loop();

    {TaskPid, task_complete, Task} ->
      ets:delete(running_tasks, select(id, Task)),
      case select(respond_to, Task) of
        undefined ->
          ets:insert(completed_tasks, lists:keydelete(pid, 1, Task));
        ReturnPid ->
          case erlang:process_info(ReturnPid) of
            undefined ->
              ets:insert(completed_tasks, lists:keydelete(pid, 1, Task));
            _ ->
              ReturnPid ! {task_complete, lists:keydelete(pid, 1, Task)}
          end
      end,
      loop();

    {ReturnPid, retrieve_task, TaskId} ->
      case ets:lookup(completed_tasks, TaskId) of
        [] ->
          ReturnPid ! {task_not_complete, TaskId};
        Task ->
          io:format("Task id in completed task table ~p~n", [TaskId]),
          ReturnPid ! {task_complete, Task},
          ets:delete(completed_tasks, TaskId)
      end,
      loop();

    {ReturnPid, get_params} ->
      ReturnPid ! {self(), update_params, ets:tab2list(params)},
      loop();

    {ReturnPid, update_params, Params} ->
      ets:insert(params, Params),
      loop();

    stop ->
      leoronic:add_child_process(leoronic, loop_check_should_be_head, []),
      ok;

    {'EXIT', Pipe, Reason} ->
      exit(head_terminated)
  after
    30 * 1000 ->
      loop()
  end.