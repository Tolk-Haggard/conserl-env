-module(conserl_env_http).

-export([
         get_env/0,
         get_env/1,
         index/1
        ]).

-define(HEADERS, [{"Accept", "application/json"}]).

-spec get_env() -> Environment::map().
get_env() ->
  Uri = build_uri(),
  {ok, _, [{"X-Consul-Index", Index}], Body} = ibrowse:send_req(Uri, ?HEADERS, get),
  EncodedListOfKVs = jiffy:decode(Body),
  Env = maps:from_list([ decode_kv(X) || X <- EncodedListOfKVs ]),
  Env#{index => list_to_integer(Index)}.

decode_kv({[{Key, {[{<<"type">>, Type},{<<"value">>, Value}]}}]}) ->
  {binary_to_atom(Key, utf8), decode_consul_values(Type, Value)}.

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

-spec index(Environment::map()) -> Index::non_neg_integer().
index(#{}) ->
  ignored.

build_uri() ->
  {ok, Tld}  = application:get_env(conserl_env, consul_tld,  "local"),
  {ok, Port} = application:get_env(conserl_env, consul_port, 8500),
  {ok, Key}  = application:get_env(conserl_env, consul_key,  "conserl_env"),
  "http://consul.service." ++ Tld ++ ":" ++ integer_to_list(Port) ++ "/v1/kv/" ++ Key ++ "?recurse".
