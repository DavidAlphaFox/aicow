-module(aicow_session_handler).
-export([execute/2]).
-export([session_id/1,session_id/2]).
-export([remove/1]).

-define(SESSION_COOKIE_NAME,<<"aicow.session">>).

execute(Req,Env)->
    case session_id(Req) of
        undefined ->
            Req0 = session_id(Req,Env),
            {ok,Req0,Env};
        Session ->
            erlang:put(session,Session),
            {ok,Req,Env}
    end.

session_id(Req)->
    case erlang:get(session) of
        undefined ->
            try cowboy_req:parse_cookies(Req) of
                [] -> undefined;
                Cookies ->
                    SessionID =  proplists:get_value(?SESSION_COOKIE_NAME,Cookies),
                    erlang:put(session,SessionID),
                    SessionID
            catch
                _:_ -> undefined
            end;
        SessionID -> SessionID
    end.

session_id(Req,Env)->
    DefaultSessionCookie = #{http_only => true,path => <<"/">>},
    SessionCookie = maps:get(session_cookie,Env,DefaultSessionCookie),
    Token = token(),
    erlang:put(session,Token),
    cowboy_req:set_resp_cookie(?SESSION_COOKIE_NAME,
                               Token, Req,SessionCookie).

remove(Req)->
    erlang:put(session,undefined),
    cowboy_req:set_resp_cookie(?SESSION_COOKIE_NAME,<<"">>,Req,#{max_age => 0}).
token()->
    UUID = ai_uuid:uuid_to_string(ai_uuid:get_v4(),binary_standard),
    Padding = crypto:strong_rand_bytes(8),
    ai_base64:encode(<<UUID/binary,Padding/binary>>,#{padding => false}).
