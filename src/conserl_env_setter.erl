-module(conserl_env_setter).

-export([
         set_env/1
        ]).

-spec set_env(consul_env_http:consul_state()) -> ok.
set_env(ConsulState) ->
  Env = conserl_env_http:environment_map(ConsulState),
  [application:set_env(App, Key, Value) || {App, Key, Value} <- Env].

