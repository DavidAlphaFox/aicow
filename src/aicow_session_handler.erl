-module(aicow_session_handler).
-export([execute/2]).
-export([get_session/0,remove_session/1]).

-define(SESSION_COOKIE_NAME,<<"aicow.session">>).
-define(SESSION_COOKIE_CONFIG,#{http_only => true,path => <<"/">>}).

-define(SESSION_CACHE_KEY,{?MODULE,session}).
-define(COOKIE_NAME_CACHE_KEY,{?MODULE,cookie_name}).

execute(Req,Env)->
    case get_session(Req,Env) of
        undefined ->
            Req0 = create_session(Req,Env),
            {ok,Req0,Env};
        Session ->
            erlang:put(?SESSION_CACHE_KEY,Session),
            {ok,Req,Env}
    end.


get_session()-> erlang:get(?SESSION_CACHE_KEY).

remove_session(Req)->
    CookieName = erlang:get(?COOKIE_NAME_CACHE_KEY),
    erlang:put(?SESSION_CACHE_KEY,undefined),
    case CookieName of
        undefined -> Req;
        _ ->
            cowboy_req:set_resp_cookie(CookieName,<<"">>,Req,#{max_age => 0})
    end.

get_session(Req,Env) ->
    CookieName = cookie_name(Env),
    try cowboy_req:parse_cookies(Req) of
      [] -> undefined;
      Cookies ->
        SessionID =  proplists:get_value(CookieName,Cookies),
        erlang:put(?SESSION_CACHE_KEY,SessionID),
        SessionID
    catch
        _:_ -> undefined
    end.

create_session(Req,Env)->
    CookieName = cookie_name(Env),
    Session = maps:get(session,Env),
    CookieConfig = maps:get(cookie_config,Session,?SESSION_COOKIE_CONFIG),
    Session = session(),
    erlang:put(?SESSION_CACHE_KEY,Session),
    cowboy_req:set_resp_cookie(CookieName,Session, Req,CookieConfig).

cookie_name(Env)->
    Session = maps:get(session,Env),
    CookieName = ai_string:to_string(maps:get(cookie_name,Session,?SESSION_COOKIE_NAME)),
    erlang:put(?COOKIE_NAME_CACHE_KEY,CookieName),
    CookieName.

session()->
    UUID = ai_uuid:uuid_to_string(ai_uuid:get_v4(),binary_standard),
    Padding = crypto:strong_rand_bytes(8),
    ai_base64:encode(<<UUID/binary,Padding/binary>>,#{padding => false}).
