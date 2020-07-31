-module(aicow_token).
-export([new/4,get/2,verify/2]).

-define(AUTHORIZATION,<<"authorization">>).

-spec new(
        Claims :: map() | list(),
        Secret :: binary(),
        Algo :: binary(),
        Expiration :: non_neg_integer() | undefined
       )-> {ok, Token :: binary()} | {error, any()}.
new(Claims,Secret,Algo,Expiration)->
    case Expiration of
        undefined -> aicow_jwt:encode(Algo,Claims,Secret);
        Expiration -> aicow_jwt:encode(Algo, Claims, Expiration,Secret)
    end.
-spec get(
        Req :: cowboy:req(),
        TokenParam::list()
       ) -> undefined | binary().
get(Req,TokenParam)->
  case cowboy_req:header(?AUTHORIZATION,Req) of
    undefined -> token(Req,TokenParam);
    Auth ->
      <<"Bearer ",Token/binary>> = Auth,
      Token
  end.
-spec verify(
        Token :: binary(),
        Secret :: binary()
       ) -> {Token :: binary(),Claims :: map() | list()} | undefined.
verify(Token,Secret) ->
  case aicow_jwt:decode(Token, Secret) of
    {ok,Claims} -> {Token,Claims};
    _ -> undefined
  end.
token(_Req,[])-> undefined;
token(Req,[{cookie,CookieName}|T])->
  R =
    try cowboy_req:parse_cookies(Req) of
        [] -> undefined;
        Cookies -> proplists:get_value(CookieName,Cookies)
    catch
      _:_ -> undefined
    end,
  if
    R == undefined -> token(Req,T);
    true -> R
  end;
token(Req,[{param,Param}|T]) ->
  QS = cowboy_req:parse_qs(Req),
  case proplists:get_value(Param,QS) of
    undefined -> token(Req,T);
    Token -> Token
  end.
