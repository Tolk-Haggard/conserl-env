-module(conserl_env_setter).

-export([
         set_env/1
        ]).

-spec set_env({_Index::non_neg_integer(), [{App::atom(), Key::atom(), Value::term()}]}) -> ok.
set_env({_, Env}) ->
  [application:set_env(App, Key, Value) || {App, Key, Value} <- Env].

