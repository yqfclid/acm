%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-12-19 00:43:03
%%%-------------------------------------------------------------------
-module(acm_tests).

-include_lib("eunit/include/eunit.hrl").
-include("acm.hrl").
%%%===================================================================
%%% test
%%%===================================================================
acm_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(ok) ->
         {inparallel, all_tests()}
    end}.

start() ->
    error_logger:tty(false),
    application:set_env(acm, access_key, <<"LTAIN75ax2MpED9f">>),
    application:set_env(acm, secret_key, <<"VQXuWqDk4uBX1vwAV3lGvVZC0Vxhoh">>),
    application:set_env(acm, namespace, <<"d764ae3a-dad0-4932-8946-875e89978817">>),
    application:set_env(acm, group, <<"TEST_GROUP">>),
    application:set_env(acm, data_id, <<"test1">>),
    {ok, _} = application:ensure_all_started(acm),
    ok.    

stop(ok) -> 
    application:stop(acm),
    application:stop(hackney),
    ok.

all_tests() ->
   [
    get_server_list(),
    publish_and_remove(),
    listen()
   ].

get_server_list() ->
    {ok, Servers} = acm:get_server_list(),
    ExpectServers = [<<"139.196.135.144">>],
    ?_assertEqual(ExpectServers, Servers).

publish_and_remove() ->
    {ok, ok} = acm:publish_config(<<"test2">>),
    {ok, ok} = acm:publish_config(<<"test2">>, <<"test22">>),
    Result0 = acm:get_config(),
    Result1 = acm:get_config(<<"test2">>),
    {ok, ok} = acm:remove_config(<<"test2">>),
    timer:sleep(1000),
    Result2 = acm:get_config(<<"test2">>),
    [?_assertEqual({ok, <<"test2">>}, Result0),
     ?_assertEqual({ok, <<"test22">>}, Result1),
     ?_assertEqual({error, 404}, Result2)].

listen() ->
    {ok, ok} = acm:publish_config(<<"test2">>),
    {ok, Status} = acm:listen_config(<<"test22">>),
    ?_assertEqual(changed, Status).
