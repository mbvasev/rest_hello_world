%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).
-include("../../mt4_utils/include/managerapiwrapper.hrl").

-define(MAX_TOKENS,3).
-define(TARDING_SERVERS_RESOURCE,"tradingservers").
-define(ACCOUNTS_RESOURCE,"accounts").
-define(BALANCEOPS_RESOURCE,"balanceops").
-define(RESOURCES,[?ACCOUNTS_RESOURCE,?BALANCEOPS_RESOURCE]).

-export([init/3, content_types_accepted/2, known_methods/2, allowed_methods/2, options/2, process_json_request/2, prepare_json_response/2, get_ballop_params/2]).
%% -export([service_available/2]).
-export([content_types_provided/2]).
-compile([{parse_transform, lager_transform}]).
init(_Transport, Req, []) ->
	{Origin, _} = cowboy_req:header(<<"origin">>,Req),
	lager:info("Origin ~p",[Origin]),
	lager:info("~p",[Req]),

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
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET,POST,OPTIONS">>, Req),
	Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,  <<"*">>, Req1),
	Req4 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>,<<"Origin, Content-Type, Accept">>,Req2),
	Req5 = cowboy_req:set_resp_header(<<"access-control-max-age">>,<<"60">>,Req4),
	{ok, Req5, State}.

known_methods(Req, State)->
%% 	{{Ip,_},_} = cowboy_req:peer(Req),
%% 	lager:info("peer ~p ~n",[Ip]),
%%
%% 	{Method,_} = cowboy_req:method(Req),
%% 	io:format("known_methods ~p ~n",[Method]),
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
%% 	{Path,_} = cowboy_req:path(Req),
%% 	PathTokens = string:tokens(binary_to_list(Path),"/"),
	Resource = get_quered_resource(Req),
	case Resource of
		{?ACCOUNTS_RESOURCE}->
			{[<<"POST">>,<<"OPTIONS">>], Req, State};
		{?ACCOUNTS_RESOURCE,_AccountId}->
			{[<<"GET">>,<<"OPTIONS">>], Req, State};
		{?ACCOUNTS_RESOURCE,_AccountId,?BALANCEOPS_RESOURCE} ->
			{[<<"POST">>,<<"OPTIONS">>], Req, State};
		_->
			{halt, Req, State}
	end.

%% charsets_provided(Req, State)->
%% 	io:format("charsets_provided \n"),
%% 	{[ <<"UTF-8">>], Req, State}.

content_types_accepted(Req, State) ->
	{ok,Res,_Re1} = cowboy_req:parse_header(<<"content-type">>, Req),
	lager:info("parsed content-type header is: ~p ~n ",[Res]),
	{[{{<<"application">>, <<"json">>,[{<<"charset">>,<<"utf-8">>}]}, process_json_request}], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, prepare_json_response}], Req, State}.

get_quered_resource(Req)->
	{Path,_} = cowboy_req:path(Req),
	PathTokens = string:tokens(binary_to_list(Path),"/"),
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
	Reply = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,  <<"*">>, Req),
	Resource = get_quered_resource(Req),
	case Resource of
		{?ACCOUNTS_RESOURCE}->
			create_account(Reply,State);
		{?ACCOUNTS_RESOURCE,AccountId,?BALANCEOPS_RESOURCE} ->
			create_balance_op(AccountId,Reply, State);
		_ ->
			lager:error("unknown resource: ~p",[Resource]),
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State}
	end.

create_balance_op(AccountId, Req, State)->
	{ok, Body, _} = cowboy_req:body(Req),
%% 	{ok,{Account,Amount,Comment}) = get_ballop_params(Body),
	{struct, JsonData} = mochijson2:decode(Body),
	Amount = proplists:get_value(<<"amount">>, JsonData),
	Comment = proplists:get_value(<<"comment">>, JsonData),
	Login = list_to_integer(AccountId),

	lager:info("going to call create_balance_operation Login: ~p ~p ~n",[Login,JsonData]),
	case catch(mt4_direct_connection:create_balance_operation(Login,Amount,binary_to_list(Comment))) of
		{ok,Ticket}->
			lager:info("balop created ticket: ~p",[Ticket]),
 			ReplyBody = mochijson2:encode({struct, [{ticket, Ticket}]}),
 			{ok, _Reply} = cowboy_req:reply(201, [], ReplyBody, Req),
			{true,Req,State};
		{'EXIT',Reason}->
			lager:error("'EXIT': ~p",[Reason]),
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State};
		{error,ErrMessage}->
			lager:error("error: ~p",[ErrMessage]),
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State};
		{badarg,Arg}->
			lager:error("badarg: ~p",[Arg]),
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State};
		badarg->
			lager:error("badarg:"),
			{ok, Reply} = cowboy_req:reply(404, [], [], Req),
			{halt, Reply, State}
	end.

get_ballop_params(AccountId,ReqBody)->
	{struct, JsonData} = mochijson2:decode(ReqBody),
	Amount = proplists:get_value(<<"amount">>, JsonData),
	Comment = proplists:get_value(<<"comment">>, JsonData),
	Login = list_to_integer(AccountId),
  res = is_integer(Login) andalso is_float(Amount) andalso is_list(binary_to_list(Comment)),

	if(res =:= true)->
		{ok,{Login,Amount,Comment}};
		true->
			badarg
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
	Group = proplists:get_value(<<"group">>, JsonData),
	EnableAccount = proplists:get_value(<<"enabled">>, JsonData),
	Leverage = proplists:get_value(<<"leverage">>, JsonData),
	Password = proplists:get_value(<<"password">>, JsonData),
%% 	Comment = "dd",
	lager:info("Going to call create account with params ~p",[JsonData]),
	AcountInfo = #mt4_user{name = binary_to_list(Name),address =  binary_to_list(Address),email =binary_to_list(Email), city = binary_to_list(City),comment =  "",id =binary_to_list(Id),
		phone = binary_to_list(Phone),country =  binary_to_list(Country),zipcode =  binary_to_list(Zip),group =  binary_to_list(Group),leverage =  Leverage,enable = EnableAccount,password = binary_to_list(Password)},

	case catch(mt4_direct_connection:create_account(AcountInfo)) of
		{ok,Login} ->
			lager:info("account create login is ~p",[Login]),
			ReplyBody = mochijson2:encode({struct, [{login, Login}]}),
			{ok, _Reply} = cowboy_req:reply(201, [], ReplyBody, Req),
			{true, Req, State};
		{error,ErrMsg}->
			lager:error("error ~p",[ErrMsg]),
			{ok, Reply} = cowboy_req:reply(204, [], mochijson2:encode({struct, [{error, list_to_binary(ErrMsg)}]}), Req),
			{halt, Reply, State};
		{'EXIT',Reason}->
			lager:error("EXIT ~p",[Reason]),
			ReplyBody = mochijson2:encode({struct, [{error, Reason}]}),
			{ok, Reply} = cowboy_req:reply(404, [], ReplyBody, Req),
			{halt, Reply, State};
		{badarg,Arg}->
			lager:error("badarg ~p",[Arg]),
			{ok, Reply} = cowboy_req:reply(404, [],mochijson2:encode({struct, [{error, <<"badarg_">>}]}),Req),
			{halt, Reply, State};
		badarg->
			lager:error("badarg"),
			{ok, Reply} = cowboy_req:reply(404, [],mochijson2:encode({struct, [{error, <<"badarg">>}]}),Req),
			{halt, Reply, State}
	end.



prepare_json_response(Req, State) ->
	Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,  <<"*">>, Req),
	{Path,_} = cowboy_req:path(Req),
	PathTokens = string:tokens(binary_to_list(Path),"/"),
	LastToken = lists:last(PathTokens),
	Account =list_to_integer(LastToken),
	case catch(mt4_direct_connection:get_account(Account)) of
		{'EXIT',Reason}->
			lager:error("EXIT : ~p",[Reason]),
			{ok, Reply} = cowboy_req:reply(404, [], [], Req1),
			{halt, Reply, State};
		not_found->
			lager:warning("account ~p not found",[Account]),
			{ok, Reply} = cowboy_req:reply(404, [], [], Req1),
			{halt, Reply, State};
		{ok,UserInfo} ->
			lager:info("account ~p found",[Account]),
			Answer = {struct, [{balance, UserInfo#mt4_user.balance}]},
			RespBody = mochijson2:encode(Answer),
			{RespBody, Req1, State}
	end.


