-module(conserl_env_http_parser).

-export([
         parse_kv/1
        ]).

-spec parse_kv(KV::map()) -> {App::binary(), AppKey::binary(), Value::term()} | bad_value.
parse_kv(#{<<"Key">> := Key, <<"Value">> := Value}) ->
  kv_checker(Value, binary:split(Key, <<"/">>, [global]));
parse_kv(_) ->
  bad_value.

kv_checker(Value, [_, App, AppKey]) when Value =/= null ->
  {ok, T, _} = erl_scan:string(base64:decode_to_string(Value) ++ "."),
  case erl_parse:parse_term(T) of
    { ok, Val } -> {binary_to_atom(App, utf8), binary_to_atom(AppKey, utf8), Val};
    { error, _ } -> bad_value
  end;
kv_checker(_, _) ->
  bad_value.
