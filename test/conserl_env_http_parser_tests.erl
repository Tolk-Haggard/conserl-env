-module(conserl_env_http_parser_tests).

-include("test_fixture.hrl").

-define(CONSUL_KV(Key, Value), #{<<"CreateIndex">> => 10,
                                 <<"Flags">> => 0,
                                 <<"Key">> => <<"conserl_env/app/", Key/binary>>,
                                 <<"LockIndex">> => 0,
                                 <<"ModifyIndex">> => 10,
                                 <<"Value">> => base64:encode(Value)}).

-define(KV_MISSING_VALUE(Key), #{<<"CreateIndex">> => 10,
                                 <<"Flags">> => 0,
                                 <<"Key">> => <<"conserl_env/app/", Key/binary>>,
                                 <<"LockIndex">> => 0,
                                 <<"ModifyIndex">> => 10
                                 }).
-define(KV_MALFORMED_KEY(Key), #{<<"CreateIndex">> => 10,
                                 <<"Flags">> => 0,
                                 <<"Key">> => <<"kee", Key/binary>>,
                                 <<"LockIndex">> => 0,
                                 <<"ModifyIndex">> => 10,
                                 <<"Value">> => base64:encode("{\"type\": \"atom\",\"value\": \"first_value\"}")}).

-define(KV_NULL_VALUE, #{<<"CreateIndex">> => 10,
                                 <<"Flags">> => 0,
                                 <<"Key">> => <<"conserl_env/app/">>,
                                 <<"LockIndex">> => 0,
                                 <<"ModifyIndex">> => 10,
                                 <<"Value">> => null}).

parse_kv_handles_missing_value() ->
  Key = <<"first_key">>,
  Actual = conserl_env_http_parser:parse_kv(?KV_MISSING_VALUE(Key)),

  ?assertEqual(bad_value, Actual).

parse_kv_handles_malformed_key() ->
  Actual = conserl_env_http_parser:parse_kv(?KV_MALFORMED_KEY(<<"whatever">>)),

  ?assertEqual(bad_value, Actual).

parse_kv_handles_null_value() ->
  Actual = conserl_env_http_parser:parse_kv(?KV_NULL_VALUE),

  ?assertEqual(bad_value, Actual).

parse_kv_bad_value_provided() ->
  Key = <<"first_key">>,
  Value = "{a, b",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch(bad_value, Actual).

parse_kv_populates_tuple_into_env_map() ->
  Key = <<"first_key">>,
  Value = "{value1, {\"value2\", <<\"value3\">>}}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, {value1, {"value2", <<"value3">>}}}, Actual).

parse_kv_populates_single_atom_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "value1",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, value1}, Actual).

parse_kv_populates_single_binary_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "<<\"value1\">>",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, <<"value1">>}, Actual).

parse_kv_populates_list_binary_values_into_env_map() ->
  Key = <<"first_key">>,
  Value = "[<<\"value1\">>, <<\"value2\">>]",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, [<<"value1">>, <<"value2">>]}, Actual).

parse_kv_populates_list_strings_values_into_env_map() ->
  Key = <<"first_key">>,
  Value = "[\"value1\", \"value2\"]",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, ["value1", "value2"]}, Actual).

parse_kv_populates_single_string_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "\"value1\"",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, "value1"}, Actual).

parse_kv_populates_single_integer_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "10",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, 10}, Actual).
