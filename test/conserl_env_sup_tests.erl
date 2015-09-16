-module(conserl_env_sup_tests).

-include("test_fixture.hrl").

init_should_spawn_child() ->
  ExpectedChildSpec = {conserl_env_poller, {conserl_env_poller, start_link, []}, permanent, 5000, worker, [conserl_env_poller]},
  Actual = conserl_env_sup:init([]),
  ?assertEqual({ok, { {one_for_one, 5, 10}, [ExpectedChildSpec]} }, Actual).
