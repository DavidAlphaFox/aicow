-module(aicow_session).
-export([get/0,remove/1]).
-export([new/2,get/2,put/1]).

-define(SESSION_COOKIE_NAME,<<"aicow.session">>).
-define(SESSION_COOKIE_CONFIG,#{http_only => true,path => <<"/">>}).

-define(SESSION_CACHE_KEY,{?MODULE,session}).
-define(COOKIE_NAME_CACHE_KEY,{?MODULE,name}).

-spec get() -> binary().
get()-> erlang:get(?SESSION_CACHE_KEY).

-spec remove(cowboy_req:req())-> cowboy_req:req().
remove(Req)->
  CookieName = name(),
  erlang:put(?SESSION_CACHE_KEY,undefined),
  remove(Req,CookieName).

remove(Req,undefined) -> Req;
remove(Req,CookieName) ->
  cowboy_req:set_resp_cookie(CookieName,<<"">>,Req,#{max_age => 0}).
%% for session handler only
get(Req,Env) ->
  CookieName = name(Env),
  try cowboy_req:parse_cookies(Req) of
    [] -> undefined;
    Cookies ->
      SessionID =  proplists:get_value(CookieName,Cookies),
      erlang:put(?SESSION_CACHE_KEY,SessionID),
      SessionID
  catch
    _:_ -> undefined
  end.

put(Session)-> erlang:put(?SESSION_CACHE_KEY,Session).

new(Req,Env)->
  CookieName = name(Env),
  SessionEnv = maps:get(session,Env,#{}),
  CookieConfig = maps:get(config,SessionEnv,?SESSION_COOKIE_CONFIG),
  Session = session(),
  erlang:put(?SESSION_CACHE_KEY,Session),
  cowboy_req:set_resp_cookie(CookieName,Session, Req,CookieConfig).

%%% internal function
name()-> erlang:get(?COOKIE_NAME_CACHE_KEY).

name(Env)->
  SessionEnv = maps:get(session,Env,#{}),
  CookieName = ai_string:to_string(maps:get(name,SessionEnv,?SESSION_COOKIE_NAME)),
  erlang:put(?COOKIE_NAME_CACHE_KEY,CookieName),
  CookieName.

session()->
  UUID = ai_uuid:uuid_to_string(ai_uuid:get_v4(),binary_standard),
  Padding = crypto:strong_rand_bytes(8),
  ai_base64:encode(<<UUID/binary,Padding/binary>>,#{padding => false}).
