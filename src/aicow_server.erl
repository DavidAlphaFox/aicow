-module(aicow_server).
-export([router/1,router/2]).

router(Router) -> router('_',Router).
router(Host,Router)->
  lists:foreach(
    fun({_Path,Module,_Ctx})->
        {module,Module} = code:ensure_loaded(Module)
    end,Router),
  Dispatch = cowboy_router:compile([{Host,Router}]),
  #{dispatch => Dispatch}.
