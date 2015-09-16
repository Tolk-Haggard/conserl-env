-module(conserl_env_poller_tests).

-include("test_fixture.hrl").

setup() ->
  ?meck(conserl_env_http, [non_strict]),
  ?stub(conserl_env_http, get_env, 0, environment1),
  ?stub(conserl_env_http, index, 1, index1).

init_calls_consul_api() ->
  conserl_env_poller:init([]),

  ?called(conserl_env_http, get_env, []).

init_stores_last_index_in_state() ->
  ?assertMatch({ok, #{index := index1}, _}, conserl_env_poller:init([])),

  ?called(conserl_env_http, index, [environment1]).

