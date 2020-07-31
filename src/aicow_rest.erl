-module(aicow_rest).
-export([options/2,options/3]).
-export([json/4]).

-spec options(cowboy_req:req(),[atom()]) -> cowboy_req:req().
options(Req,Actions)-> options(Req,Actions,#{}).

-spec options(cowboy_req:req(),[atom()],map()) -> cowboy_req:req().
options(Req,Actions,Headers)->
  Actions1 =
    if
      Actions == undefined -> <<"*">>;
      true->
        Actions0 = lists:map(fun ai_string:to_string/1,Actions),
        ai_string:join(Actions0,",")
    end,
  DefaultHeaders = #{<<"access-control-allow-methods">> => Actions1,
                     <<"access-control-allow-origin">> => <<"*">>,
                     <<"access-control-allow-headers">> => <<"authorization">>},
  Headers0 = maps:merge(DefaultHeaders,Headers),
  cowboy_req:set_resp_headers(Headers0,Req).


-spec json(data|true|false,Data::map(),
           Req::cowboy:req(),State::term())-> tuple().
json(data,Data,Req,State)-> {jiffy:encode(Data),Req,State};
json(Result,Data,Req,State)->
  Req0 = cowboy_req:set_resp_body(jiffy:encode(Data),Req),
  {Result,Req0,State}.
