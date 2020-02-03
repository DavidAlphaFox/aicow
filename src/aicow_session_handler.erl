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
        erlang:put(aicow_session,Session),
        {ok,Req,Env}
    end.


get_session()-> erlang:get(aicow_session).
remove_session(Req)->
    CookieName = erlang:get(cookie_name),
    erlang:put(aicow_session,undefined),
    cowboy_req:set_resp_cookie(CookieName,<<"">>,Req,#{max_age => 0}).

cookie_name(Env)->
    Session = maps:get(session,Env,#{}),
    CookieName = ai_string:to_string(maps:get(cookie_name,Session,?SESSION_COOKIE_NAME)),
    erlang:put(cookie_name,CookieName),
    CookieName.


get_session(Req,Env) ->
    CookieName = cookie_name(Env),
    try cowboy_req:parse_cookies(Req) of
      [] -> undefined;
      Cookies ->
        SessionID =  proplists:get_value(CookieName,Cookies),
        erlang:put(aicow_session,SessionID),
        SessionID
    catch
        _:_ -> undefined
    end.
set_session(Req,Env)->
    CookieName = cookie_name(Env),
    Session = maps:get(session,Env,#{}),
    CookieConfig = maps:get(cookie_config,Session,?SESSION_COOKIE_CONFIG),
    Token = token(),
    erlang:put(aicow_session,Token),
    cowboy_req:set_resp_cookie(CookieName,Token, Req,CookieConfig).


token()->
    UUID = ai_uuid:uuid_to_string(ai_uuid:get_v4(),binary_standard),
    Padding = crypto:strong_rand_bytes(8),
    ai_base64:encode(<<UUID/binary,Padding/binary>>,#{padding => false}).
