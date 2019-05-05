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
-export([heads/0, should_be_head/1, head/0, head_pid/0]).


heads() -> [{head, Node} || Node <- nodes(), should_be_head(Node)].


number_of_heads() ->
  case length(nodes()) + 1 of
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
  lists:sort(nodes() ++ [node()]).


should_be_head(Node) when is_atom(Node) ->
  HeadNodes = lists:sublist(sorted_nodes(), number_of_heads()),
  lists:member(Node, HeadNodes).


worker_head_tuples() ->
  Nodes = sorted_nodes(),
  IndexedNodes = indexed(Nodes),
  NHeads = number_of_heads(),
  HeadIndexForEachNode =
    lists:seq(1, NHeads) ++ % each head node is its own head.
    [I rem NHeads + 1 || I <- lists:seq(NHeads, length(IndexedNodes))],
  HeadProcesses =
    [{head, element(2, lists:keyfind(I, 1, IndexedNodes))} || I <- HeadIndexForEachNode],
  lists:zip(HeadProcesses, Nodes).


worker_pids(Head) ->
  [Worker || {Head1, Worker} <- worker_head_tuples(), Head == Head1].


head_pid() ->
  case should_be_head(node()) of
    true ->
      lists:keysearch(node(), 2, heads());
    false ->
      element(1, lists:keyfind(node(), 2, worker_head_tuples()))
  end.


% todo refactor so that the dockerfile, stdout, stderr, and result all have their own tables.
head() ->
  register(head, self()),
  register(scheduler, spawn(head, job_checker_scheduler, [os:system_time(), os:system_time()])),
  ets:new(tasks, [set, named_table]), % ets tables are released when this process terminates.
  ets:new(params, [set, named_table]),
  ets:new(running_tasks, [set, named_table]),
  spawn(head, ask_for_params, [self()]),
  loop().


ask_for_params(ReturnPid) ->
  [FirstOtherNode | _] = [Node || Node <- sorted_nodes(), Node /= node()],
  case should_be_head(FirstOtherNode) of
    false ->
      no_other_head;
    true ->
      {head, FirstOtherNode} ! {ReturnPid, get_params, undefined}
  end.

ask_for_worker_info([Worker | Workers], ReturnPid) ->
  spawn(Worker, leoronic, send_system_info, [ReturnPid]),
  ask_for_worker_info(Workers, ReturnPid).


match_available_to_requested_helper(Available, Requested, FoundMatches) ->
  [SmallestA | RestAvailable] = Available,
  [BiggestR | RestRequested] = Requested,
  IsMatch = (element(1, SmallestA) > element(1, BiggestR))
    and (element(2, SmallestA) > element(2, BiggestR)),
  case IsMatch of
    true ->
      match_available_to_requested(
        RestAvailable,
        RestRequested,
        [FoundMatches | {element(3, SmallestA), element(3, BiggestR)}]);
    false ->
      case match_available_to_requested(RestAvailable, Requested, FoundMatches) of
        FoundMatches -> % no new found matches
          match_available_to_requested(Available, RestRequested, FoundMatches);
        WithNewMatches ->
          WithNewMatches
      end
  end.

match_available_to_requested(Available, Requested, FoundMatches) ->
% todo this should be a binary search.
  case {Available, Requested} of
    {[], _} ->
      [];
    {_, []} ->
      [];
    _ ->
      match_available_to_requested_helper(Available, Requested, FoundMatches)
  end.


reformat_response(Response) ->
  {system_info,
    [
      {node, N},
      {total_memory, TM},
      {free_memory, FM},
      {cores, Cores}
    ]
  } = Response,
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
      [{select(memory, Task), select(cpus, Task), Task} || Task <- Tasks]
    )
  ), % biggest first
  case match_available_to_requested(Available, Requested, []) of
    [] -> undefined;
    Matches -> Matches
  end.

evaluate_worker_responses(Responses, Recieved, Total, ReturnPid, Tasks) ->
  receive
    {system_info, Response} ->
      NewResponses = [Responses | Response],
      case Recieved + 1 of
        Total ->
          ReturnPid ! {final, best_viable(NewResponses, Tasks)};
        _ ->
          evaluate_worker_responses(NewResponses, Recieved + 1, Total, ReturnPid, Tasks)
      end;
    {eager_eval} ->
      ReturnPid ! {eager_eval, best_viable(Responses, Tasks)}
  end.

get_runnable_tasks() ->
  head_pid() ! {self(), get_next_tasks},
  Workers = worker_pids(head_pid()),
  Tasks =
    receive
      {tasks, T} -> T
    end,
  Evaluator = spawn(node(), head, evaluate_worker_responses, [self(), Tasks]),
  ask_for_worker_info(Workers, Evaluator),
  receive
    {final, FinalResponse} -> FinalResponse
  after 2000 ->
    Evaluator ! {eager_eval},
    receive
      {final, Response} -> Response;
      {eager_eval, Response} -> Response
    end
  end.


start_jobs([{Node, Task} | OtherJobs], Acc) ->
  start_jobs(OtherJobs, [Acc |
    head_pid() ! {spawn(Node, leoronic, perform_task, [Task]), running_task, Task}
  ]).


job_checker(LastRan, LastIdle) ->
  head_pid() ! {self(), prune_running_task_record},
  case {LastRan, LastIdle} of
    {-1, -1} ->
      {scheduler, node()} ! {self(), get_times},
      receive
        {times, LastRan, LastIdle} -> job_checker(LastRan, LastIdle)
      end;
    _ ->
      % todo check for timed-out tasks / idling here
      case get_runnable_tasks() of
        undefined -> ok;
        Pairs ->
          start_jobs(Pairs, [])
      end
  end.


job_checker_scheduler(LastRan, LastIdle) ->
  receive
    {idle} -> ok; % todo
    {ran} ->
      job_checker_scheduler(os:system_time(), LastIdle);
    {ReturnPid, get_times} ->
      ReturnPid ! {times, LastRan, LastIdle},
      job_checker_scheduler(LastRan, LastIdle)
  after 1000 * 60 * 2 ->
    job_checker(LastRan, LastIdle),
    job_checker_scheduler(LastRan, LastIdle)
  end.


make_id() ->
  list_to_integer(
    integer_to_list(os:system_time()) ++
    [C || C <- pid_to_list(self()), C >= $0 andalso C =< $9]
  ).

loop() ->
  receive
    {ReturnPid, add_task, PartialTask} ->
      Id = make_id(),
      [
        {port_pid, PortPid},
        {await, Await},
        {cpus, CPUS},
        {memory, Memory},
        {storage, Storage},
        {dockerless, Dockerless},
        {container, Container}
      ] = PartialTask,
      RespondTo = case Await of
                    false -> undefined;
                    true -> PortPid
                  end,
      Task = [
        {has_run, false},
        {respond_to, RespondTo},
        {created_at, os:system_time()},
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
      ets:insert(tasks, {Id, Task}),
      ReturnPid ! {new_task_id, Id},
      loop();

    {TaskPid, running_task, TaskId} ->
      ets:insert(running_tasks, {TaskId, TaskPid}),
      loop();

    {TaskPid, task_complete, Task} ->
      ets:delete(running_tasks, select(id, Task)),
      case select(respond_to, Task) of
        undefined ->
          ets:insert(tasks, Task);
        ReturnPid ->
          ReturnPid ! {task_complete, Task},
          ets:delete(tasks, select(id, Task))
      end,
      loop();

    {ReturnPid, retrieve_task, TaskId} ->
      Task = [{id, TaskId} | ets:lookup(tasks, TaskId)],
      case select(has_run, Task) of
        true ->
          ReturnPid ! {task_complete, Task},
          ets:delete(tasks, TaskId);
        false ->
          ReturnPid ! {task_not_complete}
      end,
      loop();

    {ReturnPid, get_params} ->
      ReturnPid ! {self(), update_params, ets:tab2list(params)},
      loop();

    {ReturnPid, update_params, Params} ->
      ets:insert(params, Params),
      loop();

    {ReturnPid, get_next_tasks} ->
      RunningTaskIds = [element(1, Task) || Task <- ets:tab2list(running_tasks)],
      UnformattedTaskMatch =
        ets:select(tasks,
          [{
            {'$1',
              [
                {has_run, false},
                {respond_to, '$_'},
                {created_at, '$_'},
                {started_at, '$_'},
                {finished_at, '$_'},
                {dockerless, '$_'},
                {memory, '$_'},
                {storage, '$_'},
                {cpus, '$_'},
                {stdout, '$_'},
                {stderr, '$_'},
                {result, '$_'},
                {container, '$_'}
              ]
            },
            [not_in_match_specification('$1', RunningTaskIds)],
            []
          }], 5),
      ReturnPid !
        case UnformattedTaskMatch of
          '$end_of_table' ->
            {tasks, []};
          {UnformattedTasks, _Continuation} ->
            {tasks, [[{id, Id} | Task] || {Id, Task} <- UnformattedTasks]}
        end,
      loop();

    {ReturnPid, prune_running_task_record} ->
      RunningTasks = ets:tab2list(running_tasks),
      Infos = [{TaskId, erlang:process_info(Pid, status)} || {TaskId, Pid} <- RunningTasks],
      BadTaskIds = [
        BadId || {BadId, _} <- lists:filter(
          fun(E) ->
            element(2, E) =:= undefined
          end,
          Infos
        )
      ],
      [ets:delete(running_tasks, BadTaskId) || BadTaskId <- BadTaskIds],
      loop();

    stop ->
      register(loop_head_check,
        spawn(node(), leoronic, loop_check_should_be_head, [])),
      ok;

    {'EXIT', Pipe, Reason} ->
      os:cmd("rm leoronic.pipe"),
      exit(port_terminated)
  end.