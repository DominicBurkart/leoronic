-module(leoronic_shunts).
-export([
  link_to_leoronic/0,
  add/1,
  del/1,
  add_tag/1,
  del_tag/1
]).

%% shunt functions

connect([IP | T]) ->
  {_, Hostent} = inet:gethostbyaddr(IP),
  [Hostname] = element(2, Hostent),
  net_kernel:connect_node(list_to_atom("leoronic@" ++ Hostname)),
  %^ we make a new atom for each non-local address on the network (constrained).
  connect(T).

add(Task) ->
  [Worker | _] = nodes(connected),
  %TODO: deal with file uploading
  if
    length(Task) == 8 ->
      % give it an ID (current time and hostname)
      {ok, LocalHost} = inet:gethostname(),
      spawn(Worker, comrade, add_or_update_task, Task ++ [[erlang:system_time(), LocalHost]]);

    length(Task) == 9 ->
      spawn(Worker, comrade, add_or_update_task, Task) end.

del(Task) ->
  [Worker | _] = nodes(connected),
  spawn(Worker, comrade, cancel_task, [Task]).

add_tag(Tag) ->
  {ok, LocalHost} = inet:gethostname(),
  Is_Connected = net_kernel:connect_node(list_to_atom("leoronic@" ++ LocalHost)),
  if
    Is_Connected == false -> ok; %todo yell at the user
    Is_Connected == true -> ok % TODO
  end.




del_tag(Tag) ->
  ok. %TODO

link_to_leoronic() -> shared:link_to_leoronic().
%% end shunt functions
