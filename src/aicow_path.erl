-module(aicow_path).
-export([with_query/1]).
-export([resource_and_format/1]).

with_query(Req)->
  Path = cowboy_req:path(Req),
  QS = cowboy_req:qs(Req),
  if
    erlang:byte_size(QS) > 0 -> <<Path/binary,"?",QS/binary>>;
    true -> Path
  end.

resource_and_format(Resource)->
  case re:run(Resource,<<"(.*)\\.(.+)$">>) of
    nomatch -> {Resource,undefined};
    {match,[_,R1,R2]} ->
      {ResourcePos,ResourceLength} = R1,
      {FormatPos,FormatLength} = R2,
      ResourceName = binary:part(Resource,ResourcePos,ResourceLength),
      Format = binary:part(Resource,FormatPos,FormatLength),
      {ResourceName,Format}
  end.
