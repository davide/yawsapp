-module(yawsapp).
-behaviour(application).

-export([start/0, start/2, stop/1]).

-include("../include/yawsapp.hrl").

start() ->
	application:start(yawsapp).

start(_Type, _Args) ->
    mnesia:start(),
    application:start(inets),
    Args = lists:map(
        fun (Var) -> {ok, Value} = application:get_env(?MODULE, Var), Value end,
        [port, working_dir, docroot]
    ),
    yawsapp_sup:start_link(Args).

stop(_State) -> ok.
