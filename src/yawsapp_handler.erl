-module(yawsapp_handler).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("../include/yawsapp.hrl").

-export([out/1, handle_request/3]).

out(Arg) ->
  Req = Arg#arg.req,
  ReqPath = get_path(Arg),
  handle_request(Req#http_request.method, ReqPath, Arg).

get_path(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    Path.

handle_request('GET', "/account" ++ _, _Arg) -> % "/account" ...
    make_response(200, "<p>Please login or logout.</p>");

handle_request('GET', "/profile" ++ _, _Arg) -> % "/profile" ...
    make_response(200, "<p>This is a slick profile.</p>");

handle_request(_, Request, _Arg) -> % catchall
 {page, Request}.

make_response(Status, Message) ->
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) ->
    [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].
