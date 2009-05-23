-module(yawsapp_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-include("../include/yawsapp.hrl").

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    Flags = {one_for_one, 2, 10},
    Children = [worker_spec(yawsapp_yaws1, yawsapp_server,  [Args])],
    {ok, {Flags, Children}}.

worker_spec(Id, Module, Args) ->
    StartFunc = {Module, start_link, Args},
    {Id, StartFunc, permanent, ?SHUTDOWN_WAITING_TIME, worker, [Module]}.
