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

parse_kv_handles_missing_value() ->
  Key = <<"first_key">>,
  Actual = conserl_env_http_parser:parse_kv(?KV_MISSING_VALUE(Key)),

  ?assertEqual(bad_kv, Actual).

parse_kv_handles_malformed_key() ->
  Actual = conserl_env_http_parser:parse_kv(?KV_MALFORMED_KEY(<<"whatever">>)),

  ?assertEqual(bad_key, Actual).

parse_kv_when_jiffy_throws_returns_bad_value() ->
  Key = <<"first_key">>,
  Value = "{[\"type\"]: [\"integer\"[,{{]\"value\": 10",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch(bad_value, Actual).

parse_kv_populates_single_atom_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"atom\",\"value\": \"first_value\"}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, first_value}, Actual).

parse_kv_populates_single_binary_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"binary\",\"value\": \"first_value\"}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, <<"first_value">>}, Actual).

parse_kv_populates_list_binary_values_into_env_map() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"list_of_binaries\",\"value\": [\"bucket1\", \"bucket2\"]}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, [<<"bucket1">>, <<"bucket2">>]}, Actual).

parse_kv_populates_list_strings_values_into_env_map() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"list_of_strings\",\"value\": [\"bucket1\", \"bucket2\"]}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, ["bucket1", "bucket2"]}, Actual).

parse_kv_populates_single_string_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"string\",\"value\": \"first_value\"}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, "first_value"}, Actual).

parse_kv_populates_single_integer_value_into_env_map() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"integer\",\"value\": 10}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, 10}, Actual).

parse_kv_when_non_list_value_described_as_list_returns_type_mismatch() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"list_of_strings\",\"value\": 100}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, type_mismatch}, Actual).

parse_kv_when_non_list_value_described_as_binary_list_returns_type_mismatch() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"list_of_binaries\",\"value\": \"not_a_list\"}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, type_mismatch}, Actual).

parse_kv_when_non_integer_value_described_as_integer_returns_type_mismatch() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"integer\",\"value\": \"not_an_integer\"}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, type_mismatch}, Actual).

parse_kv_when_non_binary_value_described_as_binary_returns_type_mismatch() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"binary\",\"value\": 230}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, type_mismatch}, Actual).

parse_kv_when_non_string_value_described_as_string_returns_type_mismatch() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"string\",\"value\": 230}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, type_mismatch}, Actual).

parse_kv_when_non_atom_value_described_as_atom_returns_type_mismatch() ->
  Key = <<"first_key">>,
  Value = "{\"type\": \"atom\",\"value\": 230}",

  Actual = conserl_env_http_parser:parse_kv(?CONSUL_KV(Key, Value)),

  ?assertMatch({app, first_key, type_mismatch}, Actual).
