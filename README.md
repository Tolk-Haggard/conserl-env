# conserl_env
The purpose of conserl_env is to pull environment settings from consul's key-value datastore into an erlang application environment.

Consul's key-value datastore and its API are documented here:

https://www.consul.io/intro/getting-started/kv.html

Keys are expected to follow this convention:

"Key Base"/"Erlang Application Name"/"Erlang Environment Key"

Values are expected to be JSON objects with two properties (type and value).

The valid types are:
- binary
- integer
- string
- list_of_strings
- list_of_binaries
- atom

Valid values are anything that can be serialized in Erlang to the type specified.

##Examples

If you wanted to set the erlang key of backend_url to a binary string of <<"http://localhost">> for the erlang
application "my_app" and used the default base key, you would set "conserl_env/my_app/backend_url" to:

```
{"type": "binary", "value": "http://localhost"}
```

If you wanted to set the erlang key of fruit_names to the list of strings ["apple", "pear", "banana"] for the erlang
application "fruit_basket" and used a base key of "grocery_stand", you would set "grocery_stand/fruit_basket/fruits" to:

```
{"type": "list_of_strings", "value": ["apple", "pear", "banana"]}
```

As an alternative to adding key/values via the Consul web UI, you can use curl(1) to add/update key-values via the Consiul API:

```
curl -X PUT http://consul.service.qa3.clc:8500/v1/kv/conserl_env/s3head/dns_host --data '{"type": "string", "value": "63.251.170.211"}'
```
```
curl -X PUT http://consul.service.qa3.clc:8500/v1/kv/conserl_env/s3head/listen_port --data '{"type": "integer", "value": 10405}'
```

