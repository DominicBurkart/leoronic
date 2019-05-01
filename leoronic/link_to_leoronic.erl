%%%-------------------------------------------------------------------
%%% @author dominicburkart
%%% @doc
%%%
%%% @end
%%% Created : 25. avr. 2019 20:20
%%%-------------------------------------------------------------------
-module(link_to_leoronic).
-author("dominicburkart").

%% API
-export([link_to_leoronic/0]).

local_ips() ->
%collects all IPV4 address on the network, excluding that of this computer.
%from https://stackoverflow.com/questions/32984215/erlang-finding-my-ip-address
  {ok, Addrs} = inet:getifaddrs(),
  {ok, [{ThisIP, _, _}, _]} = inet:getif(),
  [
    Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
    Addr =/= {127, 0, 0, 1},
    Addr =/= ThisIP,
    size(Addr) == 4
  ].

connect([IP | T]) ->
  {_, Hostent} = inet:gethostbyaddr(IP),
  [Hostname] = element(2, Hostent),
  net_kernel:connect_node(list_to_atom("leoronic@" ++ Hostname)),
  %^ note: we make a new atom for each non-local address on the network.
  connect(T);

connect([]) -> ok.

link_to_leoronic() ->
  {ok, Host} = inet:gethostname(),
  case net_kernel:connect_node(list_to_atom("leoronic@" ++ Host)) of
    false ->
      connect(local_ips())
  end.
