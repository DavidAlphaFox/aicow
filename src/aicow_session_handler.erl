-module(aicow_session_handler).
-export([execute/2]).
-export([get_session/0,remove_session/1]).

-define(SESSION_COOKIE_NAME,<<"aicow.session">>).
-define(SESSION_COOKIE_CONFIG,#{http_only => true,path => <<"/">>}).

execute(Req,Env)->
    case get_session(Req,Env) of
        undefined ->
            Req0 = set_session(Req,Env),
            {ok,Req0,Env};
        Session ->
            erlang:put(session,Session),
            {ok,Req,Env}
    end.


get_session()-> erlang:get(session).
remove_session(Req)->
    CookieName = erlang:get(cookie_name),
    erlang:put(session,undefined),
    cowboy_req:set_resp_cookie(CookieName,<<"">>,Req,#{max_age => 0}).

cookie_name(Env)->
    Session = maps:get(session,Env,#{}),
    CookieName = ai_string:to_string(maps:get(cookie_name,Session,?SESSION_COOKIE_NAME)),
    erlang:put(cookie_name,CookieName),
    CookieName.

cookie_config(Env)->
    Session = maps:get(session,Env,#{}),
    CookieConfig = maps:get(cookie_config,Session,?SESSION_COOKIE_CONFIG),
    erlang:put(cookie_config,CookieConfig),
    CookieConfig.

get_session(Req,Env) ->
    CookieName = cookie_name(Env),
    _ = cookie_config(Env),
    try cowboy_req:parse_cookies(Req) of
        [] -> undefined;
        Cookies ->
            SessionID =  proplists:get_value(CookieName,Cookies),
            erlang:put(session,SessionID),
            SessionID
    catch
        _:_ -> undefined
    end.
set_session(Req,Env)->
    CookieName = cookie_name(Env),
    CookieConfig = cookie_config(Env),
    Token = token(),
    erlang:put(session,Token),
    cowboy_req:set_resp_cookie(CookieName,Token, Req,CookieConfig).


token()->
    UUID = ai_uuid:uuid_to_string(ai_uuid:get_v4(),binary_standard),
    Padding = crypto:strong_rand_bytes(8),
    ai_base64:encode(<<UUID/binary,Padding/binary>>,#{padding => false}).
