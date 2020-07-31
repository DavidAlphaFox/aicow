-module(aicow_server).
-export([router/1,router/2]).
-export([middlewares/0,session/2,permit/3]).

-spec router([{string(),atom(),term()}]) -> term().
router(Router) -> router('_',Router).
-spec router(atom() | string(),
             [{string(),atom(),term()}]) -> term().
router(Host,Router)->
  lists:foreach(
    fun({_Path,Module,_Ctx})->
        {module,Module} = code:ensure_loaded(Module)
    end,Router),
  cowboy_router:compile([{Host,Router}]).

-spec middlewares() -> list().
middlewares()->
  [cowboy_router,
   aicow_session_handler,
   aicow_permit_handler,
   cowboy_handler].

-spec session(undefined|binary(),undefined|map())-> map().
session(undefined,undefined)-> #{};
session(Name,undefined) -> #{name => Name};
session(undefined,Config) -> #{config => Config};
session(Name,Config) -> #{name => Name,config => Config}.

-spec permit(undefined|[{string(),atom()}],undefined | [string()],
             undefined|binary()) -> map().
permit(Include,Exclude,To)->
  Permit =
    if
      (Include == undefined) orelse
      (Include == [])-> #{};
      true -> #{include => Include}
    end,
  Permit0 =
    if
      (Exclude == undefined) orelse
      (Exclude == [])-> Permit;
      true -> Permit#{exclude => Exclude}
    end,
  if
    To == undefined -> Permit;
    true -> Permit0#{to => To}
  end.
      
