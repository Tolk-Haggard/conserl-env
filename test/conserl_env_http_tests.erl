-module(conserl_env_http_tests).

-include("test_fixture.hrl").

-define(CONSUL_JSON(Value), "{\"CreateIndex\": 10,\"Flags\": 0,\"Key\": \"conserl_env/app/key\",\"LockIndex\": 0,\"ModifyIndex\": 10,\"Value\": \"" ++ binary_to_list(base64:encode(Value)) ++ "\"}").

setup() ->
  ?meck([ibrowse, application], [unstick]),
  ?meck(conserl_env_http_parser, [non_strict]),
  ?stub(conserl_env_http_parser, parse_kv, 1, not_used),
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON("{\"type\": \"atom\",\"value\": \"first_value\"}") ++ "]"}),
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

get_env_calls_parser_for_each_kv_from_consul() ->
  ?stub(ibrowse, send_req, 3, {ok, status1, [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON("value1") ++ "]"}),

  conserl_env_http:get_env(),

  ExpectedKV = #{<<"CreateIndex">> => 10,
                 <<"Flags">> => 0,
                 <<"Key">> => <<"conserl_env/app/key">>,
                 <<"LockIndex">> => 0,
                 <<"ModifyIndex">> => 10,
                 <<"Value">> => <<"dmFsdWUx">>},

  ?called(conserl_env_http_parser, parse_kv, [ExpectedKV]).


index_lens_returns_index() ->
  ?assertMatch(10, conserl_env_http:index({10, []})).
