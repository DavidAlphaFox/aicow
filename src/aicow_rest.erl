-module(aicow_rest).

-export([options/2,options/3,allow_cors/2]).
-export([json/4]).

-define(HEADERS,#{<<"access-control-allow-origin">> => <<"*">>,
                  <<"access-control-allow-headers">> => <<"authorization, content-type">>}).
  

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
  DefaultHeaders = ?HEADERS#{<<"access-control-allow-methods">> => Actions1},
  Headers0 = maps:merge(DefaultHeaders,Headers),
  cowboy_req:set_resp_headers(Headers0,Req).

-spec allow_cors(cowboy_req:req(),undefined|map())-> cowboy_req:req().
allow_cors(Req,undefined) ->
  cowboy_req:set_resp_headers(?HEADERS, Req);
allow_cors(Req,Headers) ->
  Headers0 = maps:merge(?HEADERS,Headers),
  cowboy_req:set_resp_headers(Headers0,Req).


-spec json(data|true|false,Data::map(),
           Req::cowboy:req(),State::term())-> tuple().
json(data,Data,Req,State)-> {jiffy:encode(Data),Req,State};
json(Result,Data,Req,State)->
  Req0 = cowboy_req:set_resp_body(jiffy:encode(Data),Req),
  {Result,Req0,State}.
