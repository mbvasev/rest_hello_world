%%%-------------------------------------------------------------------
%%% @author Martin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. II 2015 14:25 ч.
%%%-------------------------------------------------------------------
-module(toppage_handler_tests).
-author("Martin").

-include_lib("eunit/include/eunit.hrl").

get_ballop_params_test_() ->
%%   Req = cowboy_req:new(),
  [?_assertEqual({ok,{1122,500.78,"some comment"}},toppage_handler:get_ballop_params("1122",<<"{\"amount\":500.78,\"comment\":\"some comment\"}">>)),
   ?_assertEqual(badarg,toppage_handler:get_ballop_params("1122",<<"{\"amount\":500.78,\"commen\":\"some comment\"}">>))].

