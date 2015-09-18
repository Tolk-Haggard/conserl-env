-module(conserl_env_http).

-export([
         get_env/0,
         get_env/1,
         index/1
        ]).

-define(HEADERS, [{"Accept", "application/json"}]).

-spec get_env() -> {Index::non_neg_integer, [{Application::atom(), Key::atom(), Value::term()}]}.
get_env() ->
  Uri = build_uri(),
  {ok, _, [{"X-Consul-Index", Index}], Body} = ibrowse:send_req(Uri, ?HEADERS, get),
  ListOfKVs = jiffy:decode(Body, [return_maps]),
  ListOfAppEnvs = [ decode_kv(X) || X <- ListOfKVs ],
  {list_to_integer(Index), ListOfAppEnvs}.

decode_kv(#{<<"Key">> := Key, <<"Value">> := Value}) ->
  Tokens = binary:split(Key, <<"/">>, [global]),
  [<<"conserl_env">>, App, AppKey] = Tokens,
  #{<<"type">> := Type, <<"value">> := AppValue} = jiffy:decode(base64:decode(Value), [return_maps]),
  {binary_to_atom(App, utf8), binary_to_atom(AppKey, utf8), decode_consul_values(Type, AppValue)}.

decode_consul_values(<<"binary">>, Value) ->
  Value;
decode_consul_values(<<"integer">>, Value) ->
  Value;
decode_consul_values(<<"string">>, Value) ->
  binary_to_list(Value);
decode_consul_values(<<"list_of_strings">>, Value) ->
  [ binary_to_list(X) || X <- Value ];
decode_consul_values(<<"list_of_binaries">>, Value) ->
  Value;
decode_consul_values(<<"atom">>, Value) ->
  binary_to_atom(Value, utf8).

-spec get_env(Index::non_neg_integer()) -> Environment::map().
get_env(_Index) ->
  ignored.

-spec index({Index::non_neg_integer(), [{Application::atom(), Key::atom(), Value::term()}]}) -> Index::non_neg_integer().
index({Index, []}) ->
  Index.

build_uri() ->
  {ok, Tld}  = application:get_env(conserl_env, consul_tld,  "local"),
  {ok, Port} = application:get_env(conserl_env, consul_port, 8500),
  {ok, Key}  = application:get_env(conserl_env, consul_key,  "conserl_env"),
  "http://consul.service." ++ Tld ++ ":" ++ integer_to_list(Port) ++ "/v1/kv/" ++ Key ++ "?recurse".
