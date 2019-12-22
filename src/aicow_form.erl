-module(aicow_form).
-export([build_csrf/4,build_csrf/5,verify_csrf/6]).
-export([build/3,verify/3]).


build_csrf(Secret,Session,Param,Method,Path)->
  Payload = <<Session/binary,Param/binary,Method/binary,Path/binary>>,
  SecretPayload = crypto:hmac(sha256,Secret,Payload),
  ai_base64:encode(SecretPayload,#{padding => false}).

build_csrf(Secret,Session,Method,Path)->
  Key0 = crypto:strong_rand_bytes(8),
  Key1 = ai_base64:encode(Key0,#{padding => false}),
  CSRFToken = build_csrf(Secret,Session,Key1,Method,Path),
  {Key1,CSRFToken}.

verify_csrf(Token,Secret,Session,Param,Method,Path)->
  CSRFToken = build_csrf(Secret,Session,Param,Method,Path),
  CSRFToken == Token.

build(Secret,Req,Method)->
    Path = aicow_path:with_query(Req),
    Session =  aicow_session_handler:session_id(Req) ,
    {CSRFKey,CSRFToken} = build_csrf(Secret,Session,Method,Path),
    #{
      <<"path">> => Path,
      <<"csrf">> => #{
                      <<"param">> => CSRFKey,
                      <<"token">> => CSRFToken
                     }
     }.

verify(Secret,Req,Form)->
    Method = cowboy_req:method(Req),
    Path = aicow_path:with_query(Req),
    case aicow_session_handler:session_id(Req) of
        undefined -> false;
        Session ->
            CSRFKey = proplists:get_value(<<"_csrf_param">>,Form),
            CSRFToken = proplists:get_value(<<"_csrf_token">>,Form),
            verify_csrf(CSRFToken,Secret,Session,CSRFKey,Method,Path)
    end.
