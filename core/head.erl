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

-export([heads/0, should_be_head/0, head/0, head_pid/0, job_checker_scheduler/2, evaluate_worker_responses/5, ask_for_params/1]).


heads() -> [{head, Node} || Node <- nodes(), lists:member(Node, lists:sublist(sorted_nodes(), number_of_heads()))].


number_of_heads() ->
  case length(nodes()) of
    0 ->
      1;
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
    [I rem NHeads + 1 || I <- lists:seq(NHeads, length(IndexedNodes))],
  [{head, element(2, lists:keyfind(I, 1, IndexedNodes))} || I <- HeadIndexForEachNode].


worker_head_tuples() ->
  Nodes = sorted_nodes(),
  io:format("Nodes from sorted_nodes: ~p ~n", [Nodes]),
  io:format("Head processes from sorted_nodes: ~p ~n", [head_processes(Nodes)]),
  lists:zip(head_processes(Nodes), Nodes).


worker_pids(Head) ->
  [Worker || {Head1, Worker} <- worker_head_tuples(), Head == Head1].


head_pid() ->
  case should_be_head() of
    true ->
      io:format("this node should be a head: " ++ atom_to_list(node()) ++ "~n"),
      {head, node()};
    false ->
      io:format("this node is not a head. Finding correct head...~n"),
      element(1, lists:keyfind(node(), 2, worker_head_tuples()))
  end.


% todo refactor so that the dockerfile, stdout, stderr, and result all have their own tables.
head() ->
  io:format("adding head ets tables & job_checker_scheduler ~n"),
  ets:new(task_instructions, [set, named_table]),
  ets:new(running_tasks, [set, named_table]),
  ets:new(completed_tasks, [set, named_table]),
  ets:new(params, [set, named_table]),
  io:format("tables added in head, starting children processes... ~n"),
  Now = os:system_time(second),
  leoronic:add_child_process(
    head,
    job_checker_scheduler,
    [Now, Now]
  ),
  io:format("adding ask for params thread in head...~n"),
  leoronic:add_child_process(
    head,
    ask_for_params,
    [self()]
  ),
  io:format("starting head loop...~n"),
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



ask_for_worker_info([Worker | Workers], ReturnPid) ->
  spawn(Worker, leoronic, send_system_info, [ReturnPid]),
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
  Evaluator = leoronic:add_child_process(head, evaluate_worker_responses, [self(), Tasks]),
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
      job_checker_scheduler(os:system_time(second), LastIdle);
    {ReturnPid, get_times} ->
      ReturnPid ! {times, LastRan, LastIdle},
      job_checker_scheduler(LastRan, LastIdle)
  after 1000 * 60 * 2 ->
    job_checker(LastRan, LastIdle),
    job_checker_scheduler(LastRan, LastIdle)
  end.


make_id() ->
  list_to_integer(
    integer_to_list(os:system_time(second)) ++
    [C || C <- pid_to_list(self()), C >= $0 andalso C =< $9]
  ).

n_next_tasks(Found, Last, N) ->
  case ets:next(task_instructions, Last) of
    '$end_of_table' ->
      Found;
    Key ->
      Task = ets:lookup(task_instructions, Key),
      n_next_tasks(Task ++ Found, Key, N - 1)
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
  io:format("head waiting for input..."),
  receive
    {ReturnPid, add_task, PartialTask} ->
      Id = make_id(),
      [
        {client_id, ClientId},
        {port_pid, PortPid},
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
          true -> PortPid
        end,
      Task = [
        {has_run, false},
        {respond_to, RespondTo},
        {created_at, os:system_time(second)},
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
      ets:insert(running_tasks, {select(id, Task),  Task ++ [{pid, TaskPid}]}),
      loop();

    {TaskPid, task_complete, Task} ->
      ets:delete(running_tasks, select(id, Task)),
      case select(respond_to, Task) of
        undefined ->
          ets:insert(completed_tasks, Task);
        ReturnPid ->
          ReturnPid ! {task_complete, Task}
      end,
      loop();

    {ReturnPid, retrieve_task, TaskId} ->
      case ets:lookup(completed_tasks, TaskId) of
        [] ->
          ReturnPid ! {task_not_complete, TaskId};
        Task ->
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

    {ReturnPid, get_next_tasks} ->
      ReturnPid ! {tasks, n_next_tasks(5)},
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
      leoronic:add_child_process(leoronic, loop_check_should_be_head, []),
      ok;

    {'EXIT', Pipe, Reason} ->
      os:cmd("rm leoronic.pipe"),
      exit(port_terminated)
  end.