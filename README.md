Acm
========

This lib is a client for aliyun acm(Application Configuration Management)

Example
=======

Using this lib is quite simple. First, setup something similar to this in your config and these will be your default params:

```erlang
    {acm, [{access_key, <<"*******">>}, 
           {secret_key, <<"*******">>},
           {namespace, <<"d764ae3a-dad0-4932">>},
           {data_id = <<"2">>},
           {group, <<"DEFAULT_GROUP">>},
           {endpoint, <<"acm.aliyun.com">>},
           {page_no, 1},
           {page_size, 200}
    ]}
```
Then, you need start the acm application:

```erlang
    acm:start()
```
Then you can use acm esaily

To get server list
```erlang
> acm:get_server_list().            
{ok,[<<"139.196.135.144">>]}
```
There is some example to use 
To get config by data_id
```erlang
    acm:get_config()
    acm:get_config(DataId)
    acm:get_config(DataId, Group)
    acm:get_config(DataId, Group, NameSpace)
```
To get config by data_id
```erlang
    acm:get_config()
    acm:get_config(DataId)
    acm:get_config(DataId, Group)
    acm:get_config(DataId, Group, NameSpace)
```
To get config by namespace
```erlang
    acm:namespace_config()
    acm:namespace_config(PageNo, PageSize)
    acm:namespace_config(PageNo, PageSize, NameSpace)
```

To publish config by data_id.
```erlang
    acm:publish_config(Content)
    acm:publish_config(DataId, Content)
    acm:publish_config(DataId, Group, Content)
```

To remove config by data_id
```erlang
    acm:remove_config()
    acm:remove_config(DataId)
    acm:remove_config(DataId, Group)
    acm:remove_config(DataId, Group, NameSpace)
```

To listen config
This action will block your process. if the config is not changed, it will return after 30s. And it will return when the config have changed. Content is the content of config.
```erlang
    acm:listen_config(Content)
    acm:listen_config(Content, DataId)
    acm:listen_config(Content, DataId, Group)
    acm:listen_config(Content, DataId, Group, NameSpace)
```

To add a listener to listen config
```erlang
    F1 = fun(NContent) -> io:format("~p", [NContent]) end
    F2 = {Mod, Fun}
    acm:start_listener(F1)
    acm:start_listener([F1, F2...])
    acm:start_listener(DataId, Callbacks)
    acm:start_listener(DataId, Group, Callbacks)
    acm:start_listener(DataId, Group, NameSpace, Callbacks)
    acm:start_listener(DataId, Group, NameSpace, Callbacks, ListenerName)
```
But I suggest using acm:start_listener(#acm_request{}), it can be more flexible
```erlang
    acm:start_listner(#acm_request{}).
```

You can stop listener by pid or name you registered
```erlang
    acm:stop_listener(Pid)
    acm:stop_listener(Name)
```

Besides, you can use the acm_request record

```erlang
    -include_lib("acm/include/acm.hrl").

    acm:namespace_config(#acm_request{access_key = <<"****">>, 
                                      secret_key = <<"****">>, 
                                      namespace = <<"d764ae3a-dad0-4932-8946-875e89978817">>, 
                                      page_no = 1, 
                                      page_size = 200,
                                      endpoint = <<"acm.aliyun.com">>}).
```
you should be careful when use the acm_request record, because it will not use the config you setup in the first step