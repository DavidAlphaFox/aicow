-module(aicow_csrf).
-export([build/4,build/5,verify/6]).
-export([new/3,verify/3]).


build(Secret,Session,Param,Method,Path)->
  Payload = <<Session/binary,Param/binary,Method/binary,Path/binary>>,
  SecretPayload = crypto:mac(hmac,sha256,Secret,Payload),
  ai_base64:encode(SecretPayload,#{padding => false}).

build(Secret,Session,Method,Path)->
  Key0 = crypto:strong_rand_bytes(8),
  Key1 = ai_base64:encode(Key0,#{padding => false}),
  CSRFToken = build(Secret,Session,Key1,Method,Path),
  {Key1,CSRFToken}.

verify(Token,Secret,Session,Param,Method,Path)->
  CSRFToken = build(Secret,Session,Param,Method,Path),
  CSRFToken == Token.

new(Secret,Req,Method)->
  Path = aicow_path:with_query(Req),
  Session =  aicow_session_handler:get_session(),
  {CSRFKey,CSRFToken} = build(Secret,Session,Method,Path),
  #{
    path => Path,
    '_csrf_param' => CSRFKey,
    '_csrf_token' => CSRFToken
   }.

verify(Secret,Req,Form)->
  Method = cowboy_req:method(Req),
  Path = aicow_path:with_query(Req),
  case aicow_session_handler:get_session() of
    undefined -> false;
    Session ->
      CSRFKey = proplists:get_value(<<"_csrf_param">>,Form),
      CSRFToken = proplists:get_value(<<"_csrf_token">>,Form),
      verify(CSRFToken,Secret,Session,CSRFKey,Method,Path)
  end.
