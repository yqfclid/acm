%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-08-07 11:21:28
%%%-------------------------------------------------------------------
-module(acm).

-export([start/0,
		 get_server_list/0, get_server_list/1,
		 start_listener/0, start_listener/1, start_listener/2, 
		 start_listener/3, start_listener/4, start_listener/5,
		 stop_listener/1,
		 listen_config/1, listen_config/2, listen_config/3, listen_config/4,
		 namespace_config/0, namespace_config/1, namespace_config/2, namespace_config/3,
		 get_config/0, get_config/1, get_config/2, get_config/3,
		 publish_config/1, publish_config/2, publish_config/3, publish_config/4,
		 remove_config/0, remove_config/1, remove_config/2, remove_config/3]).

-export([default_request/0]).

-define(PUBLISH_CONFIG_URL, <<"/diamond-server/basestone.do?">>).
-define(GET_CONFIG_URL, <<"/diamond-server/config.co?">>).
-define(NAMESPACE_CONFIG_URL, <<"/diamond-server/basestone.do?">>).
-define(LISTEN_CONFIG_URL, <<"/diamond-server/config.co">>).
-define(REMOVE_CONFIG_URL, <<"/diamond-server/datum.do?">>).
-define(SERVER_LIST_URL,  <<"/diamond-server/diamond">>).

-include("acm.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%%===================================================================
%%% API
%%%===================================================================
%%%开启应用
-spec start() -> {ok, Started :: list()} | {error, Reason :: term()}.
start() ->
	application:ensure_all_started(acm).

%%%==========================================================================
%%% @doc
%%% this is an application used for aliyun acm
%%%
%%% 函数在调用时，如果未传参数，则会使用默认的参数
%%%
%%% Params
%%% EndPoint  :: binary()   地址域名            default: <<"acm.aliyun.com">>
%%% DataId    :: binary()   配置Id              default: <<>>
%%% Group     :: binary()   配置所在的组         default: <<"DEFAULT_GROUP">>
%%% NameSpace :: binary()   配置所在的命名空间    default: <<>>
%%% Content   :: binary()   配置内容             default: <<>>
%%% PageNo    :: integer()  分页页号             default: 1
%%% PageSize  :: integer()  分页大小             default: 200
%%% CallBacks :: list()     配置变化时执行的回调函数列表 default: []
%%% CallBack  :: fun()   回调函数皆为一个参数的函数 
%%%                      example: F = fun(NContent) -> ok end. 
%%% @end
%%%==========================================================================

%%% 获取服务器列表IP列表
-spec get_server_list() -> {ok, list()} | {error, term()}.
get_server_list() ->
	get_server_list((default_request())#acm_request.endpoint).

-spec get_server_list(#acm_request{}|binary()) -> {ok, list()} | {error, term()}.
get_server_list(#acm_request{endpoint = EndPoint}) ->
	get_server_list(EndPoint);
get_server_list(EndPoint) ->
	Url = build_url(EndPoint, ?SERVER_LIST_URL),
	case do_request(get, Url, [], <<>>) of
		{ok, RetServers} ->
			Servers = binary:split(RetServers, <<"\n">>, [global]),
			{ok, Servers};
		{error, Reason} ->
			{error, Reason}
	end.


%%% 启动一个进程，监听配置,会在配置发生改变的时候执行callbacks
-spec start_listener() -> {ok, pid()} | {error, term()}.
start_listener() ->
	acm_listener:start_listener(default_request()).

-spec start_listener(#acm_request{}|fun()|list()) -> {ok, pid()} | {error, term()}.	
start_listener(#acm_request{} = AcmRequest) ->
	acm_listener:start_listener(AcmRequest);
start_listener(CallBacks) when is_list(CallBacks) ->
	start_listener((default_request())#acm_request{listen_callbacks = CallBacks});
start_listener(CallBack) when is_function(CallBack, 1) ->
	start_listener([CallBack]);
start_listener({Mod, Fun}) ->
	case erlang:function_exported(Mod, Fun, 1) of
		true ->
			start_listener([{Mod, Fun}]);
		false ->
			{error, bad_arg}
	end;
start_listener(_) ->
	{error, bad_arg}.


-spec start_listener(binary(), list()) -> {ok, pid()} | {error, term()}.
start_listener(DataId, CallBacks) ->
	start_listener((default_request())#acm_request{data_id = DataId,
												   listen_callbacks = CallBacks}).

-spec start_listener(binary(), binary(), list()) -> {ok, pid()} | {error, term()}.
start_listener(DataId, Group, CallBacks) ->
	start_listener((default_request())#acm_request{data_id = DataId,
												   listen_callbacks = CallBacks,
												   group = Group}).

-spec start_listener(binary(), binary(), binary(), list()) -> {ok, pid()} | {error, term()}.
start_listener(DataId, Group, Namespace, CallBacks) ->
	start_listener((default_request())#acm_request{
		namespace = Namespace,
		data_id = DataId,
		listen_callbacks = CallBacks,
		group = Group}).

-spec start_listener(binary(), binary(), binary(), list(), atom()) -> {ok, pid()} | {error, term()}.
start_listener(DataId, Group, Namespace, CallBacks, ListnerName) ->
	start_listener((default_request())#acm_request{
		namespace = Namespace,
		data_id = DataId,
		listen_callbacks = CallBacks,
		group = Group,
		listener_name = ListnerName}).

%%% 取消监听进程
-spec stop_listener(pid()|atom()) -> ok.
stop_listener(Listener) ->
	acm_listener:stop_listener(Listener).

%%% 监听配置, 如果配置有变化,则会返回，否则阻塞30秒之后返回
-spec listen_config(#acm_request{}|binary()) -> {ok, same} | {ok, changed}| {error, term()}.
listen_config(#acm_request{} = AcmRequest) ->
	#acm_request{data_id = DataId,
				 namespace = NameSpace,
				 group = Group,
				 access_key = AccessKey,
				 secret_key = SecretKey,
				 endpoint = EndPoint,
				 content = Content} = AcmRequest,
	ContentMd5 = to_binary(md5(Content)),
	ProbeModReq = <<DataId/binary, "\002", 
				    Group/binary, "\002",
				    ContentMd5/binary, "\002",
				    NameSpace/binary, "\001">>,
	Headers0 = headers(Group, NameSpace, AccessKey, SecretKey),
	Headers = [{<<"longPullingTimeout">>, 30000}|Headers0],
	Body = <<"Probe-Modify-Request=", ProbeModReq/binary>>,
	MatchBody = escape(<<DataId/binary, "\002", 
				    	 Group/binary, "\002",
				   		 NameSpace/binary, "\001">>),
	case request(post, EndPoint, ?LISTEN_CONFIG_URL, Headers, Body, [{timeout, 31000}]) of
		{ok, <<>>} ->
			{ok, same};
		{ok, RepBody} when RepBody =:= MatchBody->
			{ok, changed};
		{ok, RepBody} ->
			{error, {invalid_ret, RepBody}};
		{error, Reason} ->
			{error, Reason}
	end;
listen_config(Content) ->
	ContentB = to_binary(Content),
	listen_config((default_request())#acm_request{content = ContentB}).

-spec listen_config(binary(), binary()) -> {ok, same} | {ok, changed} | {error, term()}.
listen_config(Content, DataId) ->
	listen_config((default_request())#acm_request{content = Content,
												  data_id = DataId}).

-spec listen_config(binary(), binary(), binary()) 
	-> {ok, same} 
	 | {ok, chenged}
	 | {error, term()}.
listen_config(Content, DataId, Group) ->
	listen_config((default_request())#acm_request{content = Content,
												  data_id = DataId,
												  group = Group}).

-spec listen_config(binary(), binary(), binary(), binary()) 
	-> {ok, same}
	| {ok, changed} 
	| {error, term()}.
listen_config(Content, DataId, Group, NameSpace) ->
	listen_config((default_request())#acm_request{content = Content,
												  data_id = DataId,
												  group = Group,
												  namespace = NameSpace}).

%%% 获取命名空间配置
-spec namespace_config() -> {ok, map()} | {error, term()}.
namespace_config() ->
	namespace_config(default_request()).

-spec namespace_config(#acm_request{}) -> {ok, map()} | {error, term()}.
namespace_config(#acm_request{} = AcmRequest) ->
	#acm_request{namespace = NameSpace,
				 access_key = AccessKey,
				 secret_key = SecretKey,
				 endpoint = EndPoint,
				 page_no = PageNo,
				 page_size = PageSize} = AcmRequest,
	Path0 = build_params([{<<"tenant">>, NameSpace},
				 		  {<<"pageNo">>, PageNo},
				 		  {<<"pageSize">>, PageSize},
				 		  {<<"method">>, <<"getAllConfigByTenant">>}]),
	Path = <<?NAMESPACE_CONFIG_URL/binary, Path0/binary>>,
	Headers = headers(undefined, NameSpace, AccessKey, SecretKey),
	case request(get, EndPoint, Path, Headers, <<>>) of
		{ok, RepBody} ->
			try jiffy:decode(RepBody,[return_maps]) of
				Decoded ->
					{ok, Decoded}
			catch _:_Excetption ->
					{error, {invalid_body, RepBody}}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

-spec namespace_config(integer(), integer()) -> {ok, map()} | {error, term()}.
namespace_config(PageNo, PageSize) ->
	namespace_config((default_request())#acm_request{page_no = PageNo,
													 page_size = PageSize}).

-spec namespace_config(integer(), integer(), binary()) -> {ok, list()} | {error, term()}.
namespace_config(PageNo, PageSize, NameSpace) ->
	namespace_config((default_request())#acm_request{page_no = PageNo,
													 page_size = PageSize,
													 namespace = NameSpace}).

%%% 根据Id获取配置
-spec get_config() -> {ok, binary()} | {error, term()}.
get_config() ->
	get_config(default_request()).

-spec get_config(#acm_request{}|binary()) -> {ok, binary()} | {error, term()}.
get_config(#acm_request{} = AcmRequest) ->
	#acm_request{data_id = DataId,
				    namespace = NameSpace,
				    group = Group,
				    access_key = AccessKey,
				    secret_key = SecretKey,
					endpoint = EndPoint} = AcmRequest,
	Path0 = build_params([{<<"tenant">>, NameSpace},
				 		  {<<"dataId">>, DataId},
				 		  {<<"group">>, Group}]),
	Path = <<?GET_CONFIG_URL/binary, Path0/binary>>,
	Headers = headers(Group, NameSpace, AccessKey, SecretKey),
	case request(get, EndPoint, Path, Headers, <<>>, [{is_raw, true}]) of
		{ok, Content} ->
			NContent = iconv:convert(<<"gbk">>, <<"utf-8">>, Content),
			{ok, NContent};
		{error, Reason} ->
			{error, Reason}
	end;
get_config(DataId) ->
	get_config((default_request())#acm_request{data_id = DataId}).

-spec get_config(binary(), binary()) -> {ok, binary()} | {error, term()}.
get_config(DataId, Group) ->
	get_config((default_request())#acm_request{data_id = DataId,
											   group = Group}).
-spec get_config(binary(), binary(), binary()) -> {ok, binary()} | {error, term()}.
get_config(DataId, Group, NameSpace) ->
	get_config((default_request())#acm_request{data_id = DataId,
											  group = Group,
											  namespace = NameSpace}).

%%% 发布配置
-spec publish_config(#acm_request{}|binary()) -> {ok, ok} | {error, term()}.
publish_config(#acm_request{} = AcmRequest) ->
	#acm_request{data_id = DataId,
				 namespace = NameSpace,
				 group = Group,
				 access_key = AccessKey,
				 secret_key = SecretKey,
				 endpoint = EndPoint,
				 content = Content} = AcmRequest,
	Body = build_params([{<<"dataId">>, DataId},
				 		 {<<"group">>, Group},
				 		 {<<"tenant">>, NameSpace},
				 		 {<<"content">>, Content}]),
	Path0 = build_params([{<<"method">>, <<"syncUpdateAll">>}]),
	Path = <<?PUBLISH_CONFIG_URL/binary, Path0/binary>>,
	Headers = headers(Group, NameSpace, AccessKey, SecretKey),
	case request(post, EndPoint, Path, Headers, Body) of
		{ok, <<"OK">>} ->
			{ok, ok};
		{_, Other} ->
			{error, Other}
	end;
publish_config(Content) ->
	publish_config((default_request())#acm_request{content = Content}).

-spec publish_config(binary(), binary()) -> {ok, ok} | {error, term()}.
publish_config(DataId, Content) ->
	publish_config((default_request())#acm_request{data_id = DataId,
												   content = Content}).
-spec publish_config(binary(), binary(), binary()) -> {ok, ok} | {error, term()}.
publish_config(DataId, Group, Content) ->
	publish_config((default_request())#acm_request{data_id = DataId,
												   group = Group,
												   content = Content}).
-spec publish_config(binary(), binary(), binary(), binary()) -> {ok, ok} |{error, term()}.
publish_config(DataId, Group, NameSpace, Content) ->
	publish_config((default_request())#acm_request{data_id = DataId,
												   group = Group,
												   namespace = NameSpace,
												   content = Content}).


%%% 删除配置
-spec remove_config(#acm_request{}|binary()) -> {ok, ok} | {error, term()}.
remove_config() ->
	remove_config(default_request()).
remove_config(#acm_request{} = AcmRequest) ->
	#acm_request{data_id = DataId,
				 namespace = NameSpace,
				 group = Group,
				 access_key = AccessKey,
				 secret_key = SecretKey,
				 endpoint = EndPoint} = AcmRequest,
	Body = build_params([{<<"tenant">>, NameSpace},
				 		 {<<"dataId">>, DataId},
				 		 {<<"group">>, Group}]),
	Path0 = build_params([{<<"method">>, <<"deleteAllDatums">>}]),
	Path = <<?REMOVE_CONFIG_URL/binary, Path0/binary>>,
	Headers = headers(Group, NameSpace, AccessKey, SecretKey),
	case request(post, EndPoint, Path, Headers, Body) of
		{ok, <<"OK">>} ->
			{ok, ok};
		{_, Other} ->
			{error, Other}
	end;
remove_config(DataId) ->
	remove_config((default_request())#acm_request{data_id = DataId}).

-spec remove_config(binary(), binary()) -> {ok, ok} | {error, term()}.
remove_config(DataId, Group) ->
	remove_config((default_request())#acm_request{data_id = DataId,
												  group = Group}).
-spec remove_config(binary(), binary(), binary()) -> {ok, ok} | {error, term()}.
remove_config(DataId, Group, NameSpace) ->
	remove_config((default_request())#acm_request{data_id = DataId,
												  group = Group,
												  namespace = NameSpace}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
request(Method, EndPoint, Path, Headers, Body) ->
	request(Method, EndPoint, Path, Headers, Body, []).

request(Method, EndPoint, Path, Headers, Body, Options) ->
	case get_server_list(EndPoint) of
		{ok, [Server|_]} ->
			Url = build_url(Server, Path),
			do_request(Method, Url, Headers, Body, Options);
		{ok, []} ->
			{error, no_available_server};
		{error, Reason} ->
			{error, Reason}
	end.

do_request(Method, Url, Headers, Body) ->
	do_request(Method, Url, Headers, Body, []).
do_request(Method, Url, Headers, Body, Options) ->
	Timeout = proplists:get_value(timeout, Options, 3000),
	IsRaw = proplists:get_value(is_raw, Options, false),
    case hackney:request(Method, Url, Headers, Body, [{recv_timeout, Timeout}]) of
    	{ok, StatusCode, _RepHeaders, Ref} ->
    		case erlang:trunc(StatusCode/100) of
    			2 ->
    				case hackney:body(Ref) of
    					{ok, <<>>} ->
    						{ok, <<>>};
    					{ok, RepBody} when IsRaw =:= true ->
    						{ok, RepBody};
    					{ok, RepBody} ->
	    				 	case binary:last(RepBody) of
	    						10 ->
	    							{ok, remove_trailing_char(RepBody)};
	    						_ ->
	    							{ok, RepBody}
	    					end;
    					{error, Reason} ->
    						{error, Reason}
    				end;
    			_Other ->
    				hackney:close(Ref),
    				{error, StatusCode}
    		end;
    	{error, Reason} ->
    		{error, Reason}
    end.

build_url(Server, Path) ->
	case binary:match(Server, <<":">>) of
		nomatch ->
			<<"http://", Server/binary, ":8080", Path/binary>>;
		_ ->
			<<"http://", Server/binary, Path/binary>>
	end.

headers(Group, NameSpace, AccessKey, SecretKey) ->
 	{MegaSec, Sec, MicroSec} = os:timestamp(),
 	TimeStamp = (MegaSec * 1000000 + Sec) * 1000 + MicroSec div 1000,
 	AcmSign = acm_sign(Group, NameSpace, SecretKey, TimeStamp),	
	[{<<"Spas-AccessKey">>, AccessKey},
	 {<<"Timestamp">>, TimeStamp},
	 {<<"Spas-Signature">>, AcmSign},
	 {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}].

acm_sign(Group, NameSpace, SecretKey, TimeStamp) ->
 	TimeStampBin = integer_to_binary(TimeStamp),
 	SignBin = 
 		case Group of
 			undefined ->
 				<<NameSpace/binary, "+", TimeStampBin/binary>>;
 			_ ->
 				<<NameSpace/binary, "+", Group/binary, "+", TimeStampBin/binary>>
 		end,
 	Mac = crypto:hmac(sha, SecretKey, SignBin),	
 	base64:encode(Mac).

build_params(Params) ->
	Builds = 
		lists:foldl(
			fun({K, V}, Acc) ->
				NK = to_binary(K),
				NV = to_binary(V),
				<<Acc/binary, NK/binary, "=", NV/binary, "&">>
		end, <<>>, Params),

	remove_trailing_char(Builds).

to_binary(true) ->
	<<"true">>;
to_binary(false) ->
	<<"false">>;
to_binary(S) when is_binary(S) ->
	S;
to_binary(S) when is_list(S) ->
	list_to_binary(S);
to_binary(S) when is_atom(S) ->
	to_binary(atom_to_list(S));
to_binary(S) when is_integer(S) ->
	integer_to_binary(S);
to_binary(S) when is_float(S) ->
	float_to_binary(S);
to_binary(S) ->
	throw({bad_arg, S}).


escape(Bin) ->
	list_to_binary(edoc_lib:escape_uri(binary_to_list(Bin))).

remove_trailing_char(<<>>) ->
	<<>>;
remove_trailing_char(B) ->
	binary:part(B, {0, byte_size(B) - 1}).

default_request() ->
  	DefaultRequest = #acm_request{},
 	DefaultRequest#acm_request{
		endpoint = get_acm_env(endpoint, DefaultRequest#acm_request.endpoint),
		access_key = get_acm_env(access_key, DefaultRequest#acm_request.access_key),
		secret_key = get_acm_env(secret_key, DefaultRequest#acm_request.secret_key),
		namespace = get_acm_env(namespace, DefaultRequest#acm_request.namespace),
		group = get_acm_env(group, DefaultRequest#acm_request.group),
		data_id = get_acm_env(data_id, DefaultRequest#acm_request.data_id),
		content = get_acm_env(content, DefaultRequest#acm_request.content),
		page_no = get_acm_env(page_no, DefaultRequest#acm_request.page_no),
		page_size = get_acm_env(page_size, DefaultRequest#acm_request.page_size),
		listen_callbacks = DefaultRequest#acm_request.listen_callbacks
    }.

get_acm_env(Key, Deault) -> 
	application:get_env(acm, Key, Deault).

md5(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].


hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).


%%%===================================================================
%%% Unit test
%%%===================================================================
-ifdef(TEST).

md5_test() ->
	?assertEqual("c0ecda4b9b7a1aa965c83b9ec3090f5d", md5(<<"dsigoeprgregh">>)).

acm_sign_test() ->
	Sign = acm_sign(<<"TEST_GROUP">>, <<"TEST_NAMESPACE">>, <<"test-secret-key">>, 1545140323),
	?assertEqual(<<"yjsZYupSNJB5K/rqxOPSpokkKV0=">>, Sign).

acm_sign_1_test() ->
	Sign = acm_sign(<<"TEST1_GROUP">>, <<"TEST1_NAMESPACE">>, <<"test1-secret-key">>, 1545140324),
	?assertEqual(<<"VpokyP7s2JYdLIt5xqcNNmi2rd4=">>, Sign).

acm_sign_2_test() ->
	Sign = acm_sign(<<"TEST2_GROUP">>, <<"TEST2_NAMESPACE">>, <<"test2-secret-key">>, 1545140325),
	?assertEqual(<<"Xs4yzElsity4mSpcFMEHBvoJkek=">>, Sign).

escape_test() ->
	Encoded = escape(<<"http://localhost:8086/query?db=test&q = select * from test">>),
	Result = <<"http%3a%2f%2flocalhost%3a8086%2fquery%3fdb%3dtest%26q%20%3d%20select%20%2a%20from%20test">>,
	?assertEqual(Result, Encoded).


-endif.