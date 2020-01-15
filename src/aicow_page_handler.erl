-module(aicow_page_handler).
-behaviour(cowboy_middleware).

-ifdef(OTP_RELEASE).
-compile({nowarn_deprecated_function, [{erlang, get_stacktrace, 0}]}).
-endif.


-export([execute/2]).
-export([terminate/5]).

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), any()}
	when Req::cowboy_req:req().

-callback terminate(any(), map(), any(),atom()) -> ok.
-optional_callbacks([terminate/4]).

-ifdef(OTP_RELEASE).
-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env=#{handler := Handler, handler_opts := HandlerOpts, render := Render }) ->
	try
		init(Req,Env)
	catch Class:Reason:StackTrace ->
			terminate({crash, Class, Reason}, Req, HandlerOpts, Handler,Render),
			erlang:raise(Class, Reason, StackTrace)
	end.
-else.
-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req,Env=#{handler := Handler, handler_opts := HandlerOpts, render := Render }) ->
	try
		init(Req,Env)
	catch Class:Reason ->
			StackTrace = erlang:get_stacktrace(),
			terminate({crash, Class, Reason}, Req, HandlerOpts, Handler,Render),
			erlang:raise(Class, Reason, StackTrace)
	end.
-endif.

terminate(Reason, Req, State, Handler,Render) ->
	case erlang:function_exported(Handler, terminate, 4) of
		true -> Handler:terminate(Reason, Req, State,Render);
		false ->
			case Reason of
				normal -> ok;
				_ -> Render:render_exception(Handler,Reason,Req,State)
			end
	end.

init(Req, Env=#{handler := Handler, handler_opts := HandlerOpts, render := Render }) ->
	case Handler:init(Req, HandlerOpts) of
		{ok,Req2,State}->
			Result = terminate(normal, Req2, State, Handler,Render),
			{ok, Req2, Env#{result => Result}};
		{ok, Template,Req2, State} ->
			Req3 = Render:render(Handler,Template,Req2,State),
			Result = terminate(normal, Req3, State, Handler,Render),
			{ok, Req3, Env#{result => Result}};
		{ok,Template,ContentType,Req2,State}->
			Req3 = Render:render(Handler,Template,ContentType,Req2,State),
			Result = terminate(normal, Req3, State, Handler,Render),
			{ok, Req3, Env#{result => Result}};
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State);
		{Mod, Req2, State, Opts} ->
			Mod:upgrade(Req2, Env, Handler, State, Opts)
	end.
