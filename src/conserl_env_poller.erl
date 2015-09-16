-module(conserl_env_poller).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 30000).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Env = conserl_env_http:get_env(),
  conserl_env_setter:set_env(Env),
  Index = conserl_env_http:index(Env),
  {ok, #{index => Index}, ?TIMEOUT}.

handle_call(_Request, _From, State) ->
  {reply, ok, State, ?TIMEOUT}.

handle_cast(_Msg, State) ->
  {noreply, State, ?TIMEOUT}.

handle_info(timeout, State = #{index := Index}) ->
  Env = conserl_env_http:get_env(Index),
  conserl_env_setter:set_env(Env),
  Index1 = conserl_env_http:index(Env),
  {noreply, State#{index => Index1}, ?TIMEOUT};
handle_info(_Info, State) ->
  {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

