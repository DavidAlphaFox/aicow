-module(aicow_session_handler).
-export([execute/2]).

execute(Req,Env)->
  case aicow_session:get(Req,Env) of
    undefined ->
      Req0 = aicow_session:new(Req,Env),
      {ok,Req0,Env};
    Session ->
      aicow_session:put(Session),
      {ok,Req,Env}
  end.



