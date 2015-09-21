-module(conserl_env_http_parser).

-export([
         parse_kv/1
        ]).

-spec parse_kv(KV::map()) -> {App::binary(), AppKey::binary(), Value::term()}.
parse_kv(#{<<"Key">> := Key, <<"Value">> := Value}) ->
  Tokens = binary:split(Key, <<"/">>, [global]),
  [_, App, AppKey] = Tokens,
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

