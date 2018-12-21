%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-08-07 11:38:27
%%%-------------------------------------------------------------------
-module(acm_listener).

-behaviour(gen_server).

%% API
-export([start_listener/1, stop_listener/1]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("acm.hrl").

-record(state, {request_opt}).
%%%===================================================================
%%% API
%%%===================================================================
start_listener(RequestOpt) ->
    supervisor:start_child(acm_sup, [RequestOpt]).

stop_listener(Listener) ->
    gen_server:cast(Listener, stop).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(#acm_request{listener_name = undefined} = RequestOpt) ->
    gen_server:start_link(?MODULE, [RequestOpt], []);
start_link(#acm_request{listener_name = Name} = RequestOpt) ->
    gen_server:start_link({local, Name}, ?MODULE, [RequestOpt], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the SERVER
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([RequestOpt]) ->
    self() ! get_config,
    {ok, #state{request_opt = RequestOpt}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    lager:warning("Can't handle request: ~p", [_Request]),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    lager:warning("Can't handle msg: ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(listen, #state{request_opt = RequestOpt} = State) ->
    case acm:listen_config(RequestOpt) of
        {ok, same} ->
            self() ! listen;
        {ok, changed} ->
            self() ! get_config;
        {error, Reason} ->
            lager:error("get config with option ~p failed: ~p", [RequestOpt, Reason]),
            erlang:send_after(2000, self(), listen)
    end,
    {noreply, State};

handle_info(get_config, #state{request_opt = RequestOpt} = State) ->
    #acm_request{listen_callbacks = Cbs} = RequestOpt,
    case acm:get_config(RequestOpt) of
        {ok, NContent} ->
            GBkNContent = iconv:convert(<<"utf-8">>, <<"gbk">>, NContent),
            lists:foreach(
                fun({Mod, Fun}) -> Mod:Fun(NContent);
                   (Fun) -> Fun(NContent) 
            end, Cbs),
            self() ! listen,
            {noreply, #state{request_opt = RequestOpt#acm_request{content = GBkNContent}}};
        {error, Reason} ->
            lager:error("get config with option ~p failed: ~p", [RequestOpt, Reason]),
            erlang:send_after(2000, self(), get_config),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    lager:warning("Can't handle info: ~p", [_Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
