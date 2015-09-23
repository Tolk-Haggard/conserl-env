-module(conserl_env_setter_tests).

-include("test_fixture.hrl").

set_env_sets_application_from_env() ->
  ?meck(application, [unstick]),
  ?stub(application, set_env, 3, ok),

  conserl_env_setter:set_env({foo, [{app1, key1, value1}, {app1, key2, value2}]}),

  ?called(application, set_env, [app1, key1, value1]),
  ?called(application, set_env, [app1, key2, value2]).

set_env_calls_environment_map() ->
  ?meck(application, [unstick]),
  ?stub(application, set_env, 3, ok),
  ?meck(conserl_env_http),
  ?stub(conserl_env_http, environment_map, 1, [{app1, key1, value1}] ),

  conserl_env_setter:set_env(consul_env1),

  ?called(conserl_env_http, environment_map, [consul_env1]).
