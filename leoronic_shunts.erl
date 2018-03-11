-module(leoronic_shunts).
-export([
  link_to_leoronic/0,
  add_shunt/1,
  del_shunt/1,
  add_tag_shunt/1,
  del_tag_shunt/1
]).

%% shunt functions

connect([IP | T]) ->
  {_, Hostent} = inet:gethostbyaddr(IP),
  [Hostname] = element(2, Hostent),
  net_kernel:connect_node(list_to_atom("leoronic@" ++ Hostname)),
  %^ we make a new atom for each non-local address on the network (constrained).
  connect(T).

add(TaskString) ->
  [Worker | _] = nodes(connected),
  String_split = "|||",
  %TODO: deal with file uploading
  Task = string:tokens(TaskString, String_split),
  spawn(Worker, comrade, add_or_update_task, Task).

del(Task) ->
  [Worker | _] = nodes(connected),
  spawn(Worker, comrade, cancel_task, [Task]).

add_tag(Tag) ->
  if
    net_kernel:connect_node(list_to_atom("leoronic@" ++ Host)) =:= false ->
      ok %make this fail
  %TODO
  end.

del_tag(Tag) ->
  ok. %TODO

link_to_leoronic() -> shared:link_to_leoronic().
%% end shunt functions
