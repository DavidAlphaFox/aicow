-module(aicow_server).
-export([start/5,start/3]).

start(Name,Port,Router,Auth,SessionCookie)->
    lists:foreach(
      fun({_Path,Module,_Ctx})->
          {module,Module} = code:ensure_loaded(Module)
      end,Router),
    Dispatch = cowboy_router:compile([{'_',Router}]),
    Env = env(#{dispatch => Dispatch},Auth,SessionCookie),
    start(Name,Port,Env).

start(Name,Port,Env)->
    cowboy:start_clear(Name,[{port, Port}],
                       #{
                         env => Env,
                         middlewares => [cowboy_router,aicow_session_handler,
                                         aicow_auth_handler,cowboy_handler]
                      }).

env(Acc,undefined,undefined) -> Acc;
env(Acc,Auth,undefined) ->  Acc#{auth => Auth};
env(Acc,undefined,SessionCookie) ->  Acc#{session_cookie => SessionCookie};
env(Acc,Auth,SessionCookie) -> Acc#{session_cookie => SessionCookie,auth => Auth}.
