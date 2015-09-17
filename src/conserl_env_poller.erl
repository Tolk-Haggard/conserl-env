-module(conserl_env_poller).

-behaviour(gen_server).

-export([
         start_link/0,
         set_env/1
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 30000).

-spec set_env(Env::map()) -> ok.
set_env(Env) ->
  gen_server:cast(?MODULE, {set_env, Env}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Env = conserl_env_http:get_env(),
  conserl_env_setter:set_env(Env),
  Index = conserl_env_http:index(Env),
  {ok, #{index => Index}, ?TIMEOUT}.

handle_call(_Request, _From, State) ->
  {reply, ok, State, ?TIMEOUT}.

handle_cast({set_env, Env}, State) ->
  conserl_env_setter:set_env(Env),
  Index = conserl_env_http:index(Env),
  {noreply, State#{index => Index}, ?TIMEOUT};
handle_cast(_Msg, State) ->
  {noreply, State, ?TIMEOUT}.

handle_info(timeout, State = #{index := Index}) ->
  ok = conserl_env_http:get_env(Index),
  {noreply, State, ?TIMEOUT};
handle_info(_Info, State) ->
  {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

