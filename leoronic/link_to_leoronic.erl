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
-include_lib("kernel/include/inet.hrl").
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

connect([]) -> ok;

connect([IP | T]) ->
  case inet:gethostbyaddr(IP) of
    {ok , Hostent} when is_record(Hostent, hostent) ->
      HostName = Hostent#hostent.h_name,
      net_kernel:connect_node(list_to_atom("leoronic@" ++ HostName)),
      connect(T);
    {error, _} ->
      connect(T)
  end.

link_to_leoronic() ->
  {ok, Host} = inet:gethostname(),
  case net_kernel:connect_node(list_to_atom("leoronic@" ++ Host)) of
    false ->
      connect(local_ips())
  end.
