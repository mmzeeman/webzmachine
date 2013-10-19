%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(webmachine_elli).

-export([handle/2, handle_event/3]).

-export([version/1, peer/1]).

-ifdef(TEST).
%% The test below acts as a webmachine controller.
-export([init/1, ping/2, to_html/2]).
-endif.

-include_lib("elli/include/elli.hrl").
-include_lib("include/wm_reqdata.hrl").

-behaviour(elli_handler).

%% @doc Get a webmachine request record from an elli request.
request(ElliReq) ->
    Method = elli_request:method(ElliReq),
    RawPath = binary:bin_to_list(elli_request:raw_path(ElliReq)),
    Headers = elli_request:headers(ElliReq),
    Version = webmachine_elli:version(ElliReq),
    Peer = webmachine_elli:peer(ElliReq),
    Body = elli_request:body(ElliReq),

    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),

    #wm_reqdata{method=Method,
                version=Version, 
                path=Path, 
                raw_path=RawPath, 
                path_info=dict:new(), 
                req_headers=Headers,
                req_body=Body,
                resp_headers=mochiweb_headers:empty(), %% TODO
                resp_body = <<>>, 
                peer=Peer}.


handle(#req{}=ElliReq, Args) ->
    handle(request(ElliReq), Args);
handle(#wm_reqdata{}=ReqData, Args) ->
    Host = case host_headers(ReqData) of
               [H|_] -> H;
               [] -> []
           end,
    Path = wrq:path(ReqData),
    {Dispatch, ReqDispatch} = dispatch(Host, Path, ReqData, Args),

    case Dispatch of
        {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens} ->
            ErrorHandler = get_error_handler(),
            {ErrorHTML, ReqState1} = ErrorHandler:render_error(404, ReqDispatch, {none, none, []}),
            ReqState2 = webmachine_request:append_to_response_body(ErrorHTML, ReqState1),
            respond(wrq:set_response_code(404, ReqState2));
        {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath} ->
            {ok, Resource} = webmachine_controller:init(ReqData, Mod, ModOpts),
            {ok, RD1} = webmachine_request:load_dispatch_data(Bindings, HostTokens, Port, PathTokens, AppRoot, StringPath, ReqDispatch),
            {ok, RD2} = webmachine_request:set_metadata('controller_module', Mod, RD1),
            try 
                case webmachine_decision_core:handle_request(Resource, RD2) of
                    {_, RsFin, RdFin} ->

                        %% TODO: fix this, need to add headers
                        %{_, RdResp} = webmachine_request:send_response(RdFin),
                        webmachine_controller:stop(RsFin, RdFin),

                        respond(RdFin);
                    {upgrade, UpgradeFun, RsFin, RdFin} ->
                        %%TODO: wmtracing 4xx result codes should ignore protocol upgrades? (because the code is 404 by default...)
                        webmachine_controller:stop(RsFin, RdFin),
                        Mod:UpgradeFun(RdFin, webmachine_controller:modstate(RsFin)),

                        %% TODO: use elli's handover functionality
                        exit(not_yet_implemented)
                end
            catch
                error:Error -> 
                    ?WM_DBG({error, Error, erlang:get_stacktrace()}),
                    RD3 = RD2#wm_reqdata{response_code=500},
                    webmachine_controller:stop(Resource, RD3),
                    respond(RD3)
            end;
        handled ->
            nop
    end.


%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    %io:fwrite(standard_error, "handle_event: ~p ~p ~p~n", [_Event, _Data, _Args]),
    ok.

%%
%% Missing elli api functions.
%% 

version(#req{version=Version}) ->
    Version.

peer(#req{socket = Socket} = Req) ->
    case elli_request:get_header(<<"X-Forwarded-For">>, Req, undefined) of
        undefined ->
            socket_peer(Socket);
        Ip ->
            Ip
    end.

socket_peer(undefined) -> 
    undefined;
socket_peer(Socket) ->
    case inet:peername(Socket) of
        {ok, {Address, _}} ->
            list_to_binary(inet_parse:ntoa(Address));
        {error, _} ->
            undefined
    end.

%%
%% Helper
%%


respond(Req) ->
     {Req#wm_reqdata.response_code, 
      mochiweb_headers:to_list(Req#wm_reqdata.resp_headers),
      Req#wm_reqdata.resp_body}.


get_error_handler() ->
    case application:get_env(webzmachine, error_handler) of
        {ok, ErrorHandler} -> 
            ErrorHandler;
        undefined ->
            webmachine_error_handler
    end. 

dispatch(Host, Path, ReqData, Args) ->
    Dispatcher = proplists:get_value(dispatcher, Args),
    dispatch(Dispatcher, Host, Path, ReqData, Args).

dispatch(undefined, Host, Path, ReqData, Args) ->
    DispatchList = proplists:get_value(dispatch_list, Args, []),
    {webmachine_dispatcher:dispatch(Host, Path, DispatchList), ReqData};    
dispatch(Dispatcher, Host, Path, ReqData, _Args) ->
    Dispatcher:dispatch(Host, Path, ReqData).

host_headers(ReqData) ->
    [ V || V <- [wrq:get_req_header_lc(H, ReqData)
                 || H <- ["x-forwarded-host",
                          "x-forwarded-server",
                          "host"]],
     V /= undefined].


%%
%% TESTS
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init(_Args) ->
    {ok, undefined}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

to_html(ReqData, State) ->
    Body = io_lib:format("<html><body>~s</body></html>~n",
                         [erlang:iolist_to_binary(<<"Hello from Elli-Webmachine">>)]),
    {Body, ReqData, State}.

elli_webmachine_test() ->
    DispatchList = [{["test", "time"], webmachine_elli, []}],

    Config = [
              {callback, elli_middleware},
              {callback_args,
               [{mods, [{webmachine_elli, [{dispatch_list, DispatchList}]}]}
              ]}],

    {404, _, _} = elli_test:call('GET', <<"/hello/world">>, [], <<>>, Config),
    %% io:fwrite(standard_error, "Res: ~p~n", [Res1]),

    {404, _, _} = elli_test:call('GET', <<"/hello/world?q=test">>, [], <<>>, Config),
    %% io:fwrite(standard_error, "Res: ~p~n", [Res2]),

    %% 
    {200, _, <<"<html><body>Hello from Elli-Webmachine</body></html>\n">>} = 
        elli_test:call('GET', <<"/test/time">>, [], <<>>, Config),
    %% io:fwrite(standard_error, "Res: ~p~n", [Res3]),
    
    ok.

-endif.
