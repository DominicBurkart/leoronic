-module(leoronic_shunts).
-export([
          link_to_leoronic,
          add_shunt/1,
          del_shunt/1,
          add_tag_shunt/1,
          del_tag_shunt/1
        ]).

%% shunt functions

link_to_leoronic() ->
  {ok, Host} = inet:gethostname(),
  if
    net_kernel:connect_node(list_to_atom("leoronic@"++Host)) =:= false ->
      connect(local_ips()) %only connect to other machines if necessary.
  end.

add(Task) ->
  [Worker | _ ] = nodes(connected),
  %TODO: deal with file uploading
  spawn(Worker, comrade, add_or_update_task, [Task]).

del(Task) ->
  [Worker | _ ] = nodes(connected),
  spawn(Worker, comrade, cancel_task, [Task]).

add_tag(Tag) ->
if
  net_kernel:connect_node(list_to_atom("leoronic@"++Host)) =:= false ->
    %make this fail
  %TODO
end.

del_tag(Tag) ->
  ok. %TODO

%% end shunt functions
