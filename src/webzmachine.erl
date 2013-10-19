%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
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

-module(webzmachine).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([request/2, start/0, stop/0]).
-export_type([reqdata/0, context/0, reply/0]).

-include_lib("elli/include/elli.hrl").
-include("webmachine_logger.hrl").
-include_lib("include/wm_reqdata.hrl").

-opaque reqdata() :: #wm_reqdata{}.
-type context() :: term().
-type reply() :: {Result::term(), reqdata(), context()}.

%% @doc Get a webmachine request record from a mochiweb request.
request(mochiweb, MochiReq) ->
    Socket = MochiReq:get(socket),
    Scheme = MochiReq:get(scheme),
    Method = MochiReq:get(method),
    RawPath = MochiReq:get(raw_path),
    Version = MochiReq:get(version),
    Headers = MochiReq:get(headers),
    %
    ReqData0 = wrq:create(Socket,Method,Scheme,Version,RawPath,Headers),
    {Peer, ReqData} = webmachine_request:get_peer(ReqData0),
    PeerState = wrq:set_peer(Peer, ReqData),
    LogData = #wm_log_data{req_id=webmachine_id:generate(),
                           start_time=os:timestamp(),
                           method=Method,
                           headers=Headers,
                           peer=PeerState#wm_reqdata.peer,
                           path=RawPath,
                           version=Version,
                           response_code=404,
                           response_length=0},
    PeerState#wm_reqdata{log_data=LogData};

%% @doc Get a webmachine request record from an elli request.
request(elli, ElliReq) ->
    Method = elli_request:method(ElliReq),
    RawPath = binary:bin_to_list(elli_request:raw_path(ElliReq)),
    Headers = elli_request:headers(ElliReq),
    Version = webmachine_elli:version(ElliReq),
    Peer = webmachine_elli:peer(ElliReq),
    Body = elli_request:body(ElliReq),

    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),

    io:fwrite(standard_error, "path: ~p~n", [Path]),

    #wm_reqdata{method=Method,
                version=Version, 
                path=Path, 
                raw_path=RawPath, 
                path_info=dict:new(), 
                req_headers=Headers,
                req_body=Body,
                resp_headers=mochiweb_headers:empty(), %% TODO
                resp_body = <<>>, %% TODO          
                peer=Peer}.

%% @spec start() -> ok
%% @doc Start the webmachine server.
start() ->
    webmachine_deps:ensure(),
    application:set_env(webzmachine, server_header, webmachine_request:server_header()),
    application:start(crypto),
    application:start(webzmachine).

%% @spec stop() -> ok
%% @doc Stop the webmachine server.
stop() ->
    application:stop(webzmachine).
