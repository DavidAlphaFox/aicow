-module(aicow_server).
-export([start/5,start/4]).
-export([start_page/6,start_page/4]).


start(Name,Port,Router,Auth,Session)->
    Env = env(Router),
    Env0 = env(Env,Auth,Session),
    start(Name,Port,Env0,[cowboy_router,aicow_session_handler,aicow_auth_handler,cowboy_handler]).

start(Name,Port,Env,Middlewares)->
    case Middlewares of
        undefined ->
            cowboy:start_clear(Name,[{port, Port}],#{env => Env});
        _ ->
            cowboy:start_clear(Name,[{port, Port}],#{env => Env,middlewares => Middlewares })
    end.

start_page(Name,Port,Router,Render,Auth,Session)->
    Env = env(Router),
    Env0 = Env#{render => Render},
    Env1 = env(Env0,Auth,Session),
    start(Name,Port,Env1,[cowboy_router,aicow_session_handler,aicow_auth_handler,aicow_page_handler]).

start_page(Name,Port,Router,Render)->
    Env = env(Router),
    Env0 = Env#{render => Render},
    start(Name,Port,Env0,[cowboy_router,aicow_page_handler]).

env(Router)->
    lists:foreach(
      fun({_Path,Module,_Ctx})->
          {module,Module} = code:ensure_loaded(Module)
      end,Router),
    Dispatch = cowboy_router:compile([{'_',Router}]),
    #{dispatch => Dispatch}.

env(Acc,undefined,undefined) -> Acc;
env(Acc,Auth,undefined) ->  Acc#{auth => Auth};
env(Acc,undefined,Session) ->  Acc#{session => Session};
env(Acc,Auth,Session) -> Acc#{session=> Session,auth => Auth}.
