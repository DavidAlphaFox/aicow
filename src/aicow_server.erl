-module(aicow_server).
-export([start/4]).

-spec start(atom(),integer(),list(),map())-> term().
start(Name,Port,Router,Config)->
    Env = env(Router,Config),
    Middlewares = middlewares(Config),
    start_clear(Name,Port,Env,Middlewares).

start_clear(Name,Port,Env,Middlewares)->
    case Middlewares of
      undefined ->
        cowboy:start_clear(Name,[{port, Port}],#{env => Env});
      _ ->
        cowboy:start_clear(Name,[{port, Port}],#{env => Env,middlewares => Middlewares })
    end.

env(Router,Config)->
    lists:foreach(
      fun({_Path,Module,_Ctx})->
          {module,Module} = code:ensure_loaded(Module)
      end,Router),
    Dispatch = cowboy_router:compile([{'_',Router}]),
    case Config of 
      undefined -> #{dispatch => Dispatch};
      _ -> Config#{dispatch => Dispatch}
    end.

middlewares(undefined) -> undefined;
middlewares(Config)->
  M0 =
    case maps:get(render,Config,undefined) of 
      undefined -> [cowboy_handler];
      Module ->
        code:ensure_loaded(Module),
        [aicow_page_handler]
    end,
  M1 = middlewares(Config,M0),
  [cowboy_router|M1].
middlewares(#{auth := _Auth} = Config,M)-> middlewares(maps:remove(auth,Config),[aicow_auth_handler|M]);
middlewares(#{session := _Session},M) -> [aicow_session_handler|M];
middlewares(_,M)-> M.

