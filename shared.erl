-module(shared). %utilities used by both comrades and ghosts.
-export([
         add_tag/1,
         del_tag/1,
         has_tag/1,
         tags/0,
         system_info/0,
         run/1,
         in_leoronic/1,
         connect/1,
         local_ips/0,
         search_for_other_workers/0
       ]).

% miscellaneous functions

has_tag(Tag) ->
  ok. %TODO

tags() ->
  ok. %TODO

add_tag(Tag) ->
  ok. %TODO

del_tag(Tag) ->
  ok. %TODO

local_ips() ->
%collects all IPV4 address on the network, excluding that of this computer.
%from https://stackoverflow.com/questions/32984215/erlang-finding-my-ip-address
    {ok, Addrs} = inet:getifaddrs(),
    {ok, [{ThisIP, _, _}, _ ]} = inet:getif(),
    [
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         Addr =/= {127,0,0,1},
         Addr =/= ThisIP,
         size(Addr) == 4
    ].

search_for_other_workers() -> connect(local_ips()).

connect([IP | T]) ->
  {_, Hostent} = inet:gethostbyaddr(IP),
  [Hostname] = element(2, Hostent),
  net_kernel:connect_node(list_to_atom("leoronic@" ++ Hostname)),
  %^ we make a new atom for each non-local address on the network (constrained).
  connect(T);

connect([]) -> ok. %all done!

system_info() ->
  application:start(sasl),
  application:start(os_mon),
  [{_, Total_Memory}, {_, Current_Memory}, {_, _}] = memsup:get_system_memory_data(),
  application:stop(os_mon),
  application:stop(sasl),

  Cores = erlang:system_info(logical_processors_available),
  if
    Cores =:= unknown -> % MacOS leaves this unknown
      [{total_memory, Total_Memory}, {free_memory, Current_Memory}, {cores, erlang:system_info(schedulers_online)}]; %default: # cores
    true ->
      [{total_memory, Total_Memory}, {free_memory, Current_Memory}, {cores, Cores}]
  end.

run(CommandString) ->
  spawn(os, cmd, [CommandString]).

in_leoronic(CommandString) ->
  {ok, Tokes, _} = erl_scan:string(CommandString),
  case erl_parse:parse_exprs(Tokes) of
    {ok, Parsed} ->
      {value, Result, _} = erl_eval:exprs(Parsed, []),
      Result;
    {error, Error} ->
      error
  end.

% end miscellaneous functions
