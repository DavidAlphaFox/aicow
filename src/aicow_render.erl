-module(aicow_render).
-callback render(Handler, Template,Req,State) ->
    Req when Handler::atom(), Template::binary(),
             Req::cowboy_req:req(), State::term().
-callback render(Handler,Template,ContentType,Req,State)->
    Req when Handler::atom(), Template::binary(), ContentType :: binary(),
             Req::cowboy_req:req(), State::term().
-callback render_exception(Handler,Reason,Req,State) ->
    ok when Handler::atom(),Reason::term(),
            Req::cowboy_req:req(), State::term().
