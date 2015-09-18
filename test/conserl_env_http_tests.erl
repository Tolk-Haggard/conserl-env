-module(conserl_env_http_tests).

-include("test_fixture.hrl").

-define(CONSUL_JSON(Key, Value), "{\"CreateIndex\": 10,\"Flags\": 0,\"Key\": \"conserl_env/app/" ++ Key ++ "\",\"LockIndex\": 0,\"ModifyIndex\": 10,\"Value\": \"" ++ binary_to_list(base64:encode(Value)) ++ "\"}").

setup() ->
  ?meck([ibrowse, application], [unstick]),
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON("first_key", "{\"type\": \"atom\",\"value\": \"first_value\"}") ++ "]"}),
  ?stub(application, get_env, fun(conserl_env, consul_tld, "local")       -> {ok, "clc"};
                                 (conserl_env, consul_port, 8500)         -> {ok, 8500};
                                 (conserl_env, consul_key, "conserl_env") -> {ok, "conserl_env"};
                                 (_, _, _)                                -> {no}
                              end).

get_env_calls_consul_at_correct_address() ->
  conserl_env_http:get_env(),

  ?called(ibrowse, send_req, ["http://consul.service.clc:8500/v1/kv/conserl_env?recurse", [{"Accept","application/json"}], get]).

get_env_returns_index_from_headers() ->
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, _}, Actual).

get_env_populates_single_atom_value_into_env_map() ->
  Key = "first_key",
  Value = "{\"type\": \"atom\",\"value\": \"first_value\"}",
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON(Key, Value) ++ "]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, [{app, first_key, first_value}]}, Actual).

get_env_populates_multiple_atom_values_into_env_map() ->
  FirstKey = "first_key",
  FirstValue = "{\"type\": \"atom\",\"value\": \"first_value\"}",
  SecondKey = "second_key",
  SecondValue = "{\"type\": \"atom\",\"value\": \"second_value\"}",
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON(FirstKey, FirstValue) ++ "," ++ ?CONSUL_JSON(SecondKey, SecondValue) ++ "]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, [{app, first_key, first_value}, {app, second_key, second_value}]}, Actual).

get_env_populates_single_binary_value_into_env_map() ->
  Key = "first_key",
  Value = "{\"type\": \"binary\",\"value\": \"first_value\"}",
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON(Key, Value) ++ "]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, [{app, first_key, <<"first_value">>}]}, Actual).

get_env_populates_list_binary_values_into_env_map() ->
  FirstKey = "first_key",
  FirstValue = "{\"type\": \"atom\",\"value\": \"first_value\"}",
  SecondKey = "second_key",
  SecondValue = "{\"type\": \"list_of_binaries\",\"value\": [\"bucket1\", \"bucket2\"]}",
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON(FirstKey, FirstValue) ++ "," ++ ?CONSUL_JSON(SecondKey, SecondValue) ++ "]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, [{app, first_key, first_value}, {app, second_key, [<<"bucket1">>, <<"bucket2">>]}]}, Actual).

get_env_populates_list_strings_values_into_env_map() ->
  FirstKey = "first_key",
  FirstValue = "{\"type\": \"atom\",\"value\": \"first_value\"}",
  SecondKey = "second_key",
  SecondValue = "{\"type\": \"list_of_strings\",\"value\": [\"bucket1\", \"bucket2\"]}",
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON(FirstKey, FirstValue) ++ "," ++ ?CONSUL_JSON(SecondKey, SecondValue) ++ "]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, [{app, first_key, first_value}, {app, second_key, ["bucket1", "bucket2"]}]}, Actual).

get_env_populates_single_string_value_into_env_map() ->
  Key = "first_key",
  Value = "{\"type\": \"string\",\"value\": \"first_value\"}",
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON(Key, Value) ++ "]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, [{app, first_key, "first_value"}]}, Actual).

get_env_populates_single_integer_value_into_env_map() ->
  Key = "first_key",
  Value = "{\"type\": \"integer\",\"value\": 10}",
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON(Key, Value) ++ "]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, [{app, first_key, 10}]}, Actual).

index_lens_returns_index() ->
  ?assertMatch(10, conserl_env_http:index({10, []})).
