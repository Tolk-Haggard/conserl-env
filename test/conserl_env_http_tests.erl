-module(conserl_env_http_tests).

-include("test_fixture.hrl").

setup() ->
  ?meck([ibrowse, application], [unstick]),
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"key\": {\"type\": \"atom\",\"value\": \"value\"}}]"}),
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

  ?assertMatch(#{index := 10}, Actual).

get_env_populates_single_atom_value_into_environment() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"first_key\": {\"type\": \"atom\",\"value\": \"first_value\"}}]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch(#{index := 10, first_key := first_value}, Actual).

get_env_populates_multiple_atom_values_into_environment() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"first_key\": {\"type\": \"atom\",\"value\": \"first_value\"}},
                                                                          {\"second_key\": {\"type\": \"atom\",\"value\": \"second_value\"}}]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch(#{index := 10, first_key := first_value, second_key := second_value}, Actual).

get_env_populates_single_binary_value_into_environment() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"first_key\": {\"type\": \"binary\",\"value\": \"first_value\"}}]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch(#{index := 10, first_key := <<"first_value">>}, Actual).

get_env_populates_list_binary_values_into_environment() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"first_key\": {\"type\": \"atom\",\"value\": \"first_value\"}},
                                                                          {\"buckets\": {\"type\": \"list_of_binaries\",\"value\": [\"bucket1\", \"bucket2\"]}}]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch(#{index := 10, first_key := first_value, buckets := [<<"bucket1">>, <<"bucket2">>] }, Actual).

get_env_populates_list_strings_values_into_environment() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"first_key\": {\"type\": \"atom\",\"value\": \"first_value\"}},
                                                                          {\"buckets\": {\"type\": \"list_of_strings\",\"value\": [\"bucket1\", \"bucket2\"]}}]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch(#{index := 10, first_key := first_value, buckets := ["bucket1", "bucket2"] }, Actual).

get_env_populates_single_string_value_into_environment() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"first_key\": {\"type\": \"string\",\"value\": \"first_value\"}}]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch(#{index := 10, first_key := "first_value"}, Actual).

get_env_populates_single_integer_value_into_environment() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[{\"first_key\": {\"type\": \"integer\",\"value\": 10}}]"}),
  Actual = conserl_env_http:get_env(),

  ?assertMatch(#{index := 10, first_key := 10}, Actual).

index_lens_returns_index() ->
  ?assertMatch(10, conserl_env_http:index(#{index => 10})).
