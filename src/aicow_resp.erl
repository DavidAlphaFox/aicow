-module(aicow_resp).

-export([reply/5]).
-export([json/4,html/3,html/4]).

-spec json(error|data|integer(),map(),
           cowboy_req:req(),term())-> {ok,cowboy_req:req(),term()}.
json(error,Data,Req,State)->
  Body = jiffy:encode(Data),
  reply(400,Body,<<"application/json; charset=utf-8">>,Req,State);
json(data,Data,Req,State)->
  Body = jiffy:encode(Data),
  reply(200,Body,<<"application/json; charset=utf-8">>,Req,State);
json(Status,Data,Req,State)->
  Body = jiffy:encode(Data),
  reply(Status,Body,<<"application/json; charset=utf-8">>,Req,State).

-spec html(binary(),cowboy_req:req(),
           term())->{ok,cowboy_req:req(),term()}.
html(Data,Req,State)->
  reply(200, Data, <<"text/html; charset=utf-8">>, Req, State).

-spec html(integer(),binary(),cowboy_req:req(),
           term())->{ok,cowboy_req:req(),term()}.
html(Status,Data,Req,State)->
  reply(Status, Data, <<"text/html; charset=utf-8">>, Req, State).

-spec reply(integer(),binary(),binary(),
            cowboy_req:req(),term()) -> {ok,cowboy_req:req(),term()}.
reply(Status,Data,Format,Req,State)->
  Req0 = cowboy_req:reply(Status, #{<<"content-type">> => Format}, Data, Req),
  {ok,Req0,State}.
