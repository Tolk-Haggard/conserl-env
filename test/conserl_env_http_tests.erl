-module(conserl_env_http_tests).

-include("test_fixture.hrl").

-define(CONSUL_JSON(Value), "{\"CreateIndex\": 10,\"Flags\": 0,\"Key\": \"conserl_env/app/key\",\"LockIndex\": 0,\"ModifyIndex\": 10,\"Value\": \"" ++ binary_to_list(base64:encode(Value)) ++ "\"}").

setup() ->
  ?meck([ibrowse, application, conserl_env_http_parser, conserl_env_poller], [unstick]),
  ?stub(conserl_env_http_parser, parse_kv, 1, not_used),
  ?stub(conserl_env_poller, set_env, 1, not_used),
  ?stub(ibrowse, send_req, 3, {ok, "200", [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON("{\"type\": \"atom\",\"value\": \"first_value\"}") ++ "]"}),
  ?stub(application, get_env, fun(conserl_env, consul_tld, "local")       -> "clc";
                                 (conserl_env, consul_port, 8500)         -> 8500;
                                 (conserl_env, consul_key, "conserl_env") -> "conserl_env/app";
                                 (_, _, _)                                -> {no}
                              end).

get_env_calls_consul_at_correct_address() ->
  conserl_env_http:get_env(),

  ?called(ibrowse, send_req, ["http://consul.service.clc:8500/v1/kv/conserl_env/app?recurse&index=0", [{"Accept","application/json"}], get]).

get_env_returns_index_from_headers() ->
  Actual = conserl_env_http:get_env(),

  ?assertMatch({10, _}, Actual).

get_env_calls_parser_for_each_kv_from_consul() ->
  ?stub(ibrowse, send_req, 3, {ok, "200", [{"X-Consul-Index", "10"}], "[" ++ ?CONSUL_JSON("value1") ++ "," ++  ?CONSUL_JSON("value2") ++"]"}),

  conserl_env_http:get_env(),

  ExpectedKV1 = #{<<"CreateIndex">> => 10,
                  <<"Flags">> => 0,
                  <<"Key">> => <<"conserl_env/app/key">>,
                  <<"LockIndex">> => 0,
                  <<"ModifyIndex">> => 10,
                  <<"Value">> => base64:encode(<<"value1">>)},

  ExpectedKV2 = #{<<"CreateIndex">> => 10,
                  <<"Flags">> => 0,
                  <<"Key">> => <<"conserl_env/app/key">>,
                  <<"LockIndex">> => 0,
                  <<"ModifyIndex">> => 10,
                  <<"Value">> => base64:encode(<<"value2">>)},

  ?called(conserl_env_http_parser, parse_kv, [ExpectedKV1]),
  ?called(conserl_env_http_parser, parse_kv, [ExpectedKV2]).

get_env_with_index_calls_consul_at_correct_address() ->
  conserl_env_http:get_env(10),
  timer:sleep(5),

  ?called(ibrowse, send_req, ["http://consul.service.clc:8500/v1/kv/conserl_env/app?recurse&index=10", [{"Accept","application/json"}], get]).

get_env_with_index_returns_pid() ->
  Actual = conserl_env_http:get_env(1),

  ?assert(is_pid(Actual)).

get_env_calls_conserl_env_poll_set_env_when_complete() ->
  ?stub(conserl_env_http_parser, parse_kv, 1, kv1),

  conserl_env_http:get_env(1),
  timer:sleep(5),

  ?called(conserl_env_poller, set_env, [{10, [kv1]}]).

get_env_calls_filters_bad_values_from_parser() ->
  ?stub(conserl_env_http_parser, parse_kv, 1, bad_value),

  conserl_env_http:get_env(1),
  timer:sleep(5),

  ?called(conserl_env_poller, set_env, [{10, []}]).

get_env_when_consul_unavailable_should_return_index_passed_in_and_empty_list() ->
  ?stub(ibrowse, send_req, 3, {error,{conn_failed,{error,nxdomain}}}),

  conserl_env_http:get_env(13),
  timer:sleep(5),

  ?called(conserl_env_poller, set_env, [{13, []}]).

get_env_when_consul_returns_non_success_status_code_should_return_index_passed_in() ->
  ?stub(ibrowse, send_req, 3, {ok,"404", [{"X-Consul-Index","428757"}], "[" ++ ?CONSUL_JSON("value1") ++ "]"}),

  conserl_env_http:get_env(14),
  timer:sleep(5),

  ?called(conserl_env_poller, set_env, [{14, []}]).

get_env_when_consul_returns_empty_body_should_return_index_passed_in() ->
  ?stub(ibrowse, send_req, 3, {ok,"200", [{"X-Consul-Index","428757"}], []}),

  conserl_env_http:get_env(15),
  timer:sleep(5),

  ?called(conserl_env_poller, set_env, [{15, []}]).

index_lens_returns_index() ->
  ?assertMatch(10, conserl_env_http:index({10, []})).
