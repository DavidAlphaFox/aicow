-module(aicow_body).
-export([read/1,read/2]).

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req0} -> {ok, << Acc/binary, Data/binary >>, Req0};
        {more, Data, Req0} -> read_body(Req0, << Acc/binary, Data/binary >>)
    end.

read(Req)-> read_body(Req,<<>>).
read(raw,Req)->
    read_body(Req,<<>>);
read(json,Req)->
    {ok,Body,Req0} = read_body(Req,<<>>),
    {ok,jiffy:decode(Body,[return_maps]),Req0}.
