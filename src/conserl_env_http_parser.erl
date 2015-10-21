-module(conserl_env_http_parser).

-export([
         parse_kv/1
        ]).

-spec parse_kv(KV::map()) -> {App::binary(), AppKey::binary(), Value::term()}.
parse_kv(#{<<"Key">> := Key, <<"Value">> := Value}) ->
  kv_checker(Value, binary:split(Key, <<"/">>, [global]));
parse_kv(_) ->
  bad_kv.

kv_checker(Value, [_, App, AppKey]) ->
  try jiffy:decode(base64:decode(Value), [return_maps]) of
    #{<<"type">> := Type, <<"value">> := AppValue} ->
      {binary_to_atom(App, utf8), binary_to_atom(AppKey, utf8), decode_consul_values(Type, AppValue)}
  catch
    _:_ -> bad_value
  end;
kv_checker(_, _) ->
  bad_key.

decode_consul_values(<<"binary">>, Value) when is_binary(Value) ->
  Value;
decode_consul_values(<<"integer">>, Value) when is_integer(Value) ->
  Value;
decode_consul_values(<<"string">>, Value) when is_binary(Value) ->
  binary_to_list(Value);
decode_consul_values(<<"list_of_strings">>, Value) when is_list(Value) ->
  [ binary_to_list(X) || X <- Value ];
decode_consul_values(<<"list_of_binaries">>, Value) when is_list(Value) ->
  Value;
decode_consul_values(<<"atom">>, Value) when is_binary(Value) ->
  binary_to_atom(Value, utf8);
decode_consul_values(_, _) ->
  type_mismatch.
