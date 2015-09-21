-module(conserl_env_http).

-export([
         get_env/0,
         get_env/1,
         index/1
        ]).

-define(HEADERS, [{"Accept", "application/json"}]).

-spec get_env() -> {Index::non_neg_integer, [{Application::atom(), Key::atom(), Value::term()}]}.
get_env() ->
  get_env1(0).

-spec get_env(Index::non_neg_integer()) -> Environment::map().
get_env(Index) ->
  spawn_link(fun() -> 
                 Env = get_env1(Index),
                 conserl_env_poller:set_env(Env)
             end).

get_env1(Index) ->
  Uri = build_uri(Index),
  {ok, _, [{"X-Consul-Index", Index1}], Body} = ibrowse:send_req(Uri, ?HEADERS, get),
  ListOfKVs = jiffy:decode(Body, [return_maps]),
  ListOfAppEnvs = [ conserl_env_http_parser:parse_kv(X) || X <- ListOfKVs ],
  {list_to_integer(Index1), ListOfAppEnvs}.

-spec index({Index::non_neg_integer(), [{Application::atom(), Key::atom(), Value::term()}]}) -> Index::non_neg_integer().
index({Index, []}) ->
  Index.

build_uri(Index) ->
  {ok, Tld}  = application:get_env(conserl_env, consul_tld,  "local"),
  {ok, Port} = application:get_env(conserl_env, consul_port, 8500),
  {ok, Key}  = application:get_env(conserl_env, consul_key,  "conserl_env"),
  "http://consul.service." ++ Tld ++ ":" ++ integer_to_list(Port) ++ "/v1/kv/" ++ Key ++ "?recurse&index=" ++ integer_to_list(Index).
