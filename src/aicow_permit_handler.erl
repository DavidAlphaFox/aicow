-module(aicow_permit_handler).
-export([execute/2]).
-ifdef(OTP_RELEASE).
-define(LISTS,lists).
-else.
-define(LISTS,ai_lists).
-endif.

execute( Req, Env) ->
  Permit = maps:get(permit,Env,undefined),
  permit(Req, Env, Permit).

permit(Req,Env,undefined)-> {ok,Req,Env};
permit(Req,Env,Permit) -> exclue(Req,Env,Permit).

exclue(Req,Env,#{exclude := ExcludePath} = Permit) ->
  if
    (ExcludePath == []) orelse
    (ExcludePath == undefined) ->
      include(Req,Env,false,Permit);
    true->
      Path = cowboy_req:path(Req),
      include(Req,Env,
              lists:any(
                fun(E)->
                    case re:run(Path,E) of
                      {match,_} -> true;
                      nomatch -> false
                    end
                end,ExcludePath),
              Permit)
  end;
exclue(Req,Env,Permit) -> include(Req,Env,false,Permit).

include(Req,Env,true,_)-> {ok,Req,Env};
include(Req,Env,_,#{include := IncludePath})->
  Path = cowboy_req:path(Req),
  execute(Req,Env,
          ?LISTS:search(
             fun({E,_Module})->
                 case re:run(Path,E) of
                   {match,_} -> true;
                   nomatch -> false
                 end
             end,IncludePath)).

execute(Req,Env,{value,{_,Module}})->
  case Module:permit(Req,Env) of
    redirect->  handle(Req,Env,redirect);
    {redirect,Req0} -> handle(Req0,Env,redirect);
    {redirect,Req0,To} -> handle(Req0,To);
    false -> handle(Req,Env,false);
    {false,Req0} ->  handle(Req0,Env,false);
    true -> {ok,Req,Env}
  end;

execute(Req,Env,_)-> {ok,Req,Env}.

handle(Req,_Env,false) -> {stop,cowboy_req:reply(401,Req)};
handle(Req,#{permit := Permit},redirect)->
  case maps:get(to,Permit,undefined) of
    undefined -> {stop,cowboy_req:reply(401,Req)};
    To -> { stop, cowboy_req:reply( 302, #{ <<"Location">> => To }, Req) }
  end.

handle(Req,To) -> {stop,cowboy_req:reply(302,#{<<"Location">> => To},Req)}.
