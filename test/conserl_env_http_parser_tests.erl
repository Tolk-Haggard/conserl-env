-module(conserl_env_http_parser_tests).

-include("test_fixture.hrl").

-define(CONSUL_KV(Key, Value), #{<<"CreateIndex">> => 10,
                                 <<"Flags">> => 0,
                                 <<"Key">> => <<"conserl_env/app/", Key/binary>>,
                                 <<"LockIndex">> => 0,
                                 <<"ModifyIndex">> => 10,
                                 <<"Value">> => base64:encode(Value)}).

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
