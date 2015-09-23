-module(conserl_env_http).

-export([
         get_env/0,
         get_env/1,
         index/1,
         environment_map/1
        ]).

-define(HEADERS, [{"Accept", "application/json"}]).
-type environment_map() :: [{Application::atom(), Key::atom(), Value::term()}] | [].
-opaque consul_state() :: {non_neg_integer(), environment_map() }.
-export_type([consul_state/0, environment_map/0]).

-spec get_env() -> consul_state().
get_env() ->
  get_env1(0).

-spec get_env(Index::non_neg_integer()) -> pid().
get_env(Index) ->
  spawn_link(fun() ->
                 Env = get_env1(Index),
                 conserl_env_poller:set_env(Env)
             end).

get_env1(Index) ->
  Uri = build_uri(Index),
  maybe_parse_response( ibrowse:send_req(Uri, ?HEADERS, get), Index ).

-spec index(consul_state()) -> Index::non_neg_integer().
index({Index, _}) ->
  Index.

-spec environment_map(consul_state()) -> environment_map().
environment_map({_, Map}) ->
  Map.

build_uri(Index) ->
  Tld  = application:get_env(conserl_env, consul_tld, "local"),
  Port = application:get_env(conserl_env, consul_port, 8500),
  Key  = application:get_env(conserl_env, consul_key, "conserl_env"),
  Wait = application:get_env(conserl_env, consul_timeout_ms, 30000),
  "http://consul.service." ++ Tld ++ ":" ++ integer_to_list(Port) ++ "/v1/kv/" ++
    Key ++ "?recurse&wait=" ++ integer_to_list(Wait) ++"ms&index=" ++ integer_to_list(Index).

maybe_parse_response({ok, _StatusCode, _Headers, []}, Index) ->
  {Index, []};
maybe_parse_response({ok, "200", Headers, Body}, _Index) ->
  Index1 = proplists:get_value("X-Consul-Index", Headers),
  ListOfKVs = jiffy:decode(Body, [return_maps]),
  ListOfAppEnvs = lists:filter(fun(Elem) -> Elem =/= bad_value end,
                               [ conserl_env_http_parser:parse_kv(X) || X <- ListOfKVs ]),
  {list_to_integer(Index1), ListOfAppEnvs};
maybe_parse_response(_, Index) ->
  {Index, []}.

