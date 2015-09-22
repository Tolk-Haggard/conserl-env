-module(conserl_env_poller_tests).

-include("test_fixture.hrl").

-define(DEFAULT_TIMEOUT, timeout1).

setup() ->
  ?meck([conserl_env_http, conserl_env_setter, application], [non_strict, unstick]),
  ?stub(conserl_env_http, get_env, 0, environment1),
  ?stub(conserl_env_http, get_env, 1, ok),
  ?stub(conserl_env_http, index, 1, index1),
  ?stub(conserl_env_setter, set_env, 1, ok),
  ?stub(application, get_env, 3, timeout1).

init_calls_consul_api() ->
  conserl_env_poller:init([]),

  ?called(conserl_env_http, get_env, []).

init_stores_last_index_in_state() ->
  ?assertMatch({ok, #{index := index1}, _}, conserl_env_poller:init([])),

  ?called(conserl_env_http, index, [environment1]).

init_calls_env_setter_with_environment_informtion() ->
  conserl_env_poller:init([]),

  ?called(conserl_env_setter, set_env, [environment1]).

handle_info_when_timeout_calls_consul_api() ->
  conserl_env_poller:handle_info(timeout, #{index => index2}),

  ?called(conserl_env_http, get_env, [index2]).

handle_info_when_timeout_preserves_state() ->
  State = #{index => index2, stuff => things, key1 => value1},
  ?assertMatch({noreply, State, _}, conserl_env_poller:handle_info(timeout, State)).

handle_cast_when_set_env_stores_last_index_in_state() ->
  ?stub(conserl_env_http, index, 1, index3),
  ?assertMatch({noreply, #{index := index3}, _}, conserl_env_poller:handle_cast({set_env, environment1}, #{index => index2})),

  ?called(conserl_env_http, index, [environment1]).

handle_cast_when_set_env_preserves_state() ->
  ?stub(conserl_env_http, index, 1, index3),
  State = #{index => index2, stuff => things, key1 => value1},
  ExpectedState = State#{index := index3},
  ?assertMatch({noreply, ExpectedState, _}, conserl_env_poller:handle_cast({set_env, environment1}, State)),

  ?called(conserl_env_http, index, [environment1]).

handle_cast_when_set_env_calls_env_setter_with_environment_informtion() ->
  conserl_env_poller:handle_cast({set_env, environment1}, #{index => index2}),

  ?called(conserl_env_setter, set_env, [environment1]).

init_when_timeout_returns_timeout() ->
  ?assertMatch({ok, _, ?DEFAULT_TIMEOUT}, conserl_env_poller:init([])).

handle_call_returns_timeout() ->
  ?assertMatch({reply, _, state1, ?DEFAULT_TIMEOUT}, conserl_env_poller:handle_call(any_req, from_me, state1)).

handle_cast_returns_timeout() ->
  ?assertMatch({noreply, state1, ?DEFAULT_TIMEOUT}, conserl_env_poller:handle_cast(any_msg, state1)).

handle_info_returns_timeout() ->
  ?assertMatch({noreply, state1, ?DEFAULT_TIMEOUT}, conserl_env_poller:handle_info(any_info, state1)).

handle_info_when_timeout_returns_timeout() ->
  ?assertMatch({noreply, _, ?DEFAULT_TIMEOUT}, conserl_env_poller:handle_info(timeout, #{index => index1})).

handle_cast_when_set_env_returns_timeout() ->
  ?assertMatch({noreply, _, ?DEFAULT_TIMEOUT}, conserl_env_poller:handle_cast({set_env, ignored}, #{index => index1})).

