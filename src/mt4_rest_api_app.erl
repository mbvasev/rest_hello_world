%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(mt4_rest_api_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	ok = application:start(crypto),
	ok = application:start(ranch),
  ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(mt4_utils),

	Dispatch = cowboy_router:compile([
		{'_', [
			{'_', toppage_handler, []}
		]}
	]),
	{ok,Port} = application:get_env(cowboy_port),
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
		{env, [{dispatch, Dispatch}]}
	]),
	mt4_rest_api_sup:start_link().

stop(_State) ->
	ok.
