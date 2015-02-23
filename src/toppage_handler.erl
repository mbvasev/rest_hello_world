%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).
-include("../../mt4_utils/include/managerapiwrapper.hrl").

-define(MAX_TOKENS,3).
-define(ACCOUNTS_RESOURCE,"accounts").
-define(BALANCEOPS_RESOURCE,"balanceops").
-define(RESOURCES,[?ACCOUNTS_RESOURCE,?BALANCEOPS_RESOURCE]).

-export([init/3, content_types_accepted/2, known_methods/2, allowed_methods/2, options/2, process_json_request/2, prepare_json_response/2]).
%% -export([service_available/2]).
-export([content_types_provided/2]).

init(_Transport, _Req, []) ->
	io:format("init ~n"),
	{upgrade, protocol, cowboy_rest}.

%% service_available(Req, State)->
%% 	io:format("service_available ~n"),
%% 	case mt4_direct_connection:ping() of
%% 		ok ->
%% 			{true, Req, State};
%% 		{error,_}->
%% 			{false, Req, State}
%% 	end.


options(Req, State)->
	io:format("options ~n"),
 	{Origin, _} = cowboy_req:header(<<"origin">>,Req),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET,POST,OPTIONS">>, Req),
	Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, Req1),
	Req4 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>,<<"Origin, Content-Type, Accept">>,Req2),
	Req5 = cowboy_req:set_resp_header(<<"access-control-max-age">>,<<"60">>,Req4),
	{ok, Req5, State}.

known_methods(Req, State)->
	{Method,_} = cowboy_req:method(Req),
	io:format("known_methods ~p ~n",[Method]),
	{[<<"GET">>, <<"POST">>,<<"OPTIONS">>], Req, State}.

%% uri_too_long(Req, State)->
%% 	io:format("uri_too_long  ~n"),
%% 	{Path,_} = cowboy_req:path(Req),
%% 	PathTokens = string:tokens(binary_to_list(Path),"/"),
%% 	if length(PathTokens)>?MAX_TOKENS ->
%% 		{true, Req, State};
%% 	true ->
%% 		{false, Req, State}
%% 	end.

allowed_methods(Req, State)->
	io:format("allowed_methods ~n"),
%% 	{Path,_} = cowboy_req:path(Req),
%% 	PathTokens = string:tokens(binary_to_list(Path),"/"),
	Resource = get_quered_resource(Req),
	case Resource of
		{?ACCOUNTS_RESOURCE}->
			{[<<"POST">>,<<"OPTIONS">>], Req, State};
		{?ACCOUNTS_RESOURCE,_AccountId}->
			{[<<"GET">>,<<"OPTIONS">>], Req, State};
		{?ACCOUNTS_RESOURCE,_AccountId,?BALANCEOPS_RESOURCE} ->
			{[<<"POST">>,<<"OPTIONS">>], Req, State}
	end.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, process_json_request}], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, prepare_json_response}], Req, State}.

get_quered_resource(Req)->
	{Path,_} = cowboy_req:path(Req),
	PathTokens = string:tokens(binary_to_list(Path),"/"),
	io:format("get_quered_resource ~p ~n",[PathTokens]),
	case PathTokens of
		["restapi",?ACCOUNTS_RESOURCE]->
			{?ACCOUNTS_RESOURCE};
		["restapi",?ACCOUNTS_RESOURCE,_]->
			{?ACCOUNTS_RESOURCE,lists:nth(3,PathTokens)};
		["restapi",?ACCOUNTS_RESOURCE,_,?BALANCEOPS_RESOURCE] ->
			{?ACCOUNTS_RESOURCE,lists:nth(3,PathTokens),?BALANCEOPS_RESOURCE};
		_->
			undefined
	end.

process_json_request(Req, State) ->
	io:format("process_json_request ~n"),
	{Origin, _} = cowboy_req:header(<<"origin">>,Req),
	Reply = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, Req),
	Resource = get_quered_resource(Req),
	case Resource of
		{?ACCOUNTS_RESOURCE}->
			create_account(Reply,State);
		{?ACCOUNTS_RESOURCE,AccountId,?BALANCEOPS_RESOURCE} ->
			create_balance_op(AccountId,Reply, State);
		_ ->
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State}
	end.

create_balance_op(AccountId, Req, State)->
	io:format("create_balance_op enter~n"),
	{ok, Body, _} = cowboy_req:body(Req),
	{struct, JsonData} = mochijson2:decode(Body),
	Amount = proplists:get_value(<<"amount">>, JsonData),
	Comment = proplists:get_value(<<"comment">>, JsonData),
	Amm =  bin_to_num(Amount),
	Login = list_to_integer(AccountId),
	ValResult =  validate_ballop_params(Login,Amm,Comment),
	if
		ValResult =:= false->
		cowboy_req:reply(404, [], [], Req),
		{halt, Req, State}
	end,

	io:format("going to call create_balance_operation ~p ~n",[JsonData]),
	case mt4_direct_connection:create_balance_operation(Login,Amm,binary_to_list(Comment)) of
		{ok,Ticket}->
 			ReplyBody = mochijson2:encode({struct, [{ticket, Ticket}]}),
 			{ok, _Reply} = cowboy_req:reply(201, [], ReplyBody, Req),
			{true,Req,State};
		{error,_}->
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State};
		{badarg,_}->
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State};
		badarg->
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State}
	end.
validate_ballop_params(Login,Ammount,Comment)->
	is_integer(Login) andalso is_float(Ammount) andalso is_list(Comment).

bin_to_num(Bin)->
	try
	    binary_to_float(Bin)
	catch
	    _:_ -> binary_to_integer(Bin)+0.00
	end.

create_account(Req, State)->
	{ok, Body, _} = cowboy_req:body(Req),
	{struct, JsonData} = mochijson2:decode(Body),
	Name = proplists:get_value(<<"name">>, JsonData),
	Address = proplists:get_value(<<"address">>, JsonData),
	Email = proplists:get_value(<<"email">>, JsonData),
	City = proplists:get_value(<<"city">>, JsonData),
	Id = proplists:get_value(<<"id">>, JsonData),
	Phone = proplists:get_value(<<"phone">>, JsonData),
	Country = proplists:get_value(<<"country">>, JsonData),
	Zip = proplists:get_value(<<"zip">>, JsonData),
	Comment = "Created by REST API",
	io:format("Going to call create account ~n"),
	case mt4_direct_connection:create_account(binary_to_list(Name),binary_to_list(Address),binary_to_list(Email),binary_to_list(City),Comment,binary_to_list(Id),binary_to_list(Phone),binary_to_list(Country),binary_to_list(Zip),"GMART-USD2P",100) of
		{ok,Login} ->
			ReplyBody = mochijson2:encode({struct, [{login, Login}]}),
			{ok, _Reply} = cowboy_req:reply(201, [], ReplyBody, Req),
			{true, Req, State};
		{error,_}->
			{ok, Reply} = cowboy_req:reply(204, [], [], Req),
			{halt, Reply, State};
		{badarg,_}->
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State};
		badarg->
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State}
	end.



prepare_json_response(Req, State) ->
	io:format("get_json ~n"),
	{Origin, _} = cowboy_req:header(<<"origin">>,Req),
	Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, Req),
	{Path,_} = cowboy_req:path(Req),
	PathTokens = string:tokens(binary_to_list(Path),"/"),
	LastToken = lists:last(PathTokens),
	Account =list_to_integer(LastToken),
	case mt4_direct_connection:get_account(Account) of
		not_found->
			{ok, Reply} = cowboy_req:reply(404, [], [], Req1),
			{halt, Reply, State};
		{ok,UserInfo} ->
			Answer = {struct, [{balance, UserInfo#mt4_user.balance}]},
			RespBody = mochijson2:encode(Answer),
			{RespBody, Req1, State}
	end.


