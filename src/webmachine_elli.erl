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

-include_lib("elli/include/elli.hrl").
-include_lib("include/wm_reqdata.hrl").

-behaviour(elli_handler).

% wrq_request(ElliReq) ->
%     Socket = ElliReq#req.socket,
%     Scheme = http, %ElliReq#req.scheme,
%     Method = ElliReq#req.method,

%     RawPath = ElliReq#req.raw_path, 
%     Version = ElliReq#req.version,
%     Headers = ElliReq#req.headers,

%     %
%     io:fwrite(standard_error, "headers: ~p~n", [Headers]),
%     ReqData0 = wrq:create(Socket, Method, Scheme, Version, RawPath, Headers),

%     {Peer, ReqData} = webmachine_request:get_peer(ReqData0),
%     PeerState = wrq:set_peer(Peer, ReqData),

%     PeerState.

handle(#req{}=ElliReq, Args) ->
    %% Delegate to our handler function
    io:fwrite(standard_error, "handle: ~p, ~p~n", [ElliReq, Args]),
    Req = webzmachine:request(elli, ElliReq),
    handle(Req, Args);
handle(#wm_reqdata{}=ReqData, Args) ->
    Host = case host_headers(ReqData) of
               [H|_] -> H;
               [] -> []
           end,
    Path = wrq:path(ReqData),
    {Dispatch, ReqDispatch} = dispatch(Host, Path, ReqData, Args),

    io:fwrite(standard_error, "Dispatch: ~p~n", [Dispatch]),
    case Dispatch of
        {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens} ->
            ErrorHandler = get_error_handler(),
            {ErrorHTML, ReqState1} = ErrorHandler:render_error(404, ReqDispatch, {none, none, []}),
            ReqState2 = webmachine_request:append_to_response_body(ErrorHTML, ReqState1),
            %% {ok, ReqState3} = webmachine_request:send_response(404, ReqState2),

            io:fwrite(standard_error, "headers: ~p~n", [ReqState2#wm_reqdata.resp_headers]),
            {404, mochiweb_headers:to_list(ReqState2#wm_reqdata.resp_headers), ReqState2#wm_reqdata.resp_body};

        {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath} ->
            BootstrapResource = webmachine_controller:new(x,x,x,x),
            {ok, Resource} = BootstrapResource:wrap(ReqData, Mod, ModOpts),
            {ok,RD1} = webmachine_request:load_dispatch_data(Bindings,HostTokens,Port,PathTokens,AppRoot,StringPath,ReqDispatch),
            {ok,RD2} = webmachine_request:set_metadata('controller_module', Mod, RD1),
            try 
                case webmachine_decision_core:handle_request(Resource, RD2) of
                    {_, RsFin, RdFin} ->
                        EndTime = os:timestamp(),
                        {_, RdResp} = webmachine_request:send_response(RdFin),
                        RsFin:stop(RdResp),                       
                        ok;
                    {upgrade, UpgradeFun, RsFin, RdFin} ->
                        %%TODO: wmtracing 4xx result codes should ignore protocol upgrades? (because the code is 404 by default...)
                        RsFin:stop(RdFin),
                        Mod:UpgradeFun(RdFin, RsFin:modstate()),
                        erlang:put(mochiweb_request_force_close, true)
                end
            catch
                error:_ -> 
                    ?WM_DBG({error, erlang:get_stacktrace()}),
                    {ok,RD3} = webmachine_request:send_response(500, RD2),
                    Resource:stop(RD3),
                    webmachine_decision_core:do_log(RD3)
            end;
        handled ->
            nop
    end.


%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    io:fwrite(standard_error, "handle_event: ~p ~p ~p~n", [_Event, _Data, _Args]),
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

get_error_handler() ->
    case application:get_env(webzmachine, error_handler) of
        {ok, ErrorHandler} -> 
            ErrorHandler;
        undefined ->
            webmachine_error_handler
    end. 

dispatch(Host, Path, ReqData, Args) ->
    case proplists:get_value(dispatcher, Args) of
        undefined ->
            DispatchList = proplists:get_value(dispatch_list, Args, []),
            {webmachine_dispatcher:dispatch(Host, Path, DispatchList), ReqData};
        Dispatcher ->
            Dispatcher:dispatch(Host, Path, ReqData)                                      
    end.

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

elli_test() ->
    Config = [
              {callback, elli_middleware},
              {callback_args,
               [{mods, [
                        {webmachine_elli, [test, bla]}
                        ]}]}
              ],

    {404, _, _} = Res1 = elli_test:call('GET', <<"/hello/world">>, [], <<>>, Config),
    io:fwrite(standard_error, "Res: ~p~n", [Res1]),

    {404, _, _} = Res2 = elli_test:call('GET', <<"/hello/world?q=test">>, [], <<>>, Config),
    io:fwrite(standard_error, "Res: ~p~n", [Res2]),

    ok.

-endif.
