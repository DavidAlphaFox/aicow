-module(aicow_auth_handler).
-export([execute/2]).

execute( Req, Env) ->
    case maps:get(auth,Env,undefined) of
        undefined -> {ok,Req,Env};
        Auth ->
            #{exclude := ExcludePath, include := IncludePath} = Auth,
            Path = cowboy_req:path(Req),
            Exclude =
                lists:any(fun(E)->
                                  case re:run(Path,E) of
                                      {match,_} -> true;
                                      nomatch -> false
                                  end
                          end,ExcludePath),
            execute(Req,Env,Exclude,IncludePath)
    end.

execute(Req,Env,true,_IncludePath)-> {ok,Req,Env};
execute(Req,Env,_,IncludePath)->
    Path = cowboy_req:path(Req),
    Include =
        lists:search(fun({E,_Module})->
                             case re:run(Path,E) of
                                 {match,_} -> true;
                                 nomatch -> false
                             end
                     end,IncludePath),
    execute(Req,Env,Include).

execute(Req,Env,{value,{_,Module}})->
  case Module:execute(Req,Env) of
      redirect->  handle(Req,Env,redirect);
      {redirect,Req0} -> handle(Req0,Env,redirect);
      {redirect,Req0,To} -> handle(Req0,To);
      false -> handle(Req,Env,false);
      {false,Req0} ->  handle(Req0,Env,false);
      true -> {ok,Req,Env};
      R -> R
  end;

execute(Req,Env,_)-> {ok,Req,Env}.

handle(Req,_Env,false) -> {stop,cowboy_req:reply(401,Req)};
handle(Req,#{auth := Auth},redirect)->
    case maps:get(to,Auth,undefined) of
        undefined -> {stop,cowboy_req:reply(401,Req)};
        To -> { stop, cowboy_req:reply( 302, #{ <<"Location">> => To }, Req) }
    end.

handle(Req,To) -> {stop,cowboy_req:reply(302,#{<<"Location">> => To},Req)}.
