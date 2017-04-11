%% MIT License

%% Copyright (c) 2017 Yunfeng Tao

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(ots).
-include("ots.hrl").
-include("ots_proto.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([newCredential/2, newCredential/3]).
-export([newAccessPoint/1, newAccessPoint/2]).
-export([newOtsClient/2, shutdown/1]).
-export([validate/1]).
-export([listTable/2, createTable/2, deleteTable/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec newCredential(string(), string()) -> {ok, #ots_Credential{}}.
-spec newCredential(string(), string(), string()) -> {ok, #ots_Credential{}}.

newCredential(AccessKeyId, AccessKeySecret) ->
    {ok, #ots_Credential{accessKeyId=AccessKeyId, accessKeySecret=AccessKeySecret}}.

newCredential(AccessKeyId, AccessKeySecret, SecurityToken) ->
    {ok, #ots_Credential{accessKeyId=AccessKeyId, accessKeySecret=AccessKeySecret, securityToken=SecurityToken}}.

-spec newAccessPoint(string(), string()) -> {ok, #ots_AccessPoint{}}.
-spec newAccessPoint(string()) -> {ok, #ots_AccessPoint{}} | {error, string()}.

newAccessPoint(Endpoint, Instance) ->
    {ok, #ots_AccessPoint{endpoint=Endpoint, instance=Instance}}.

newAccessPoint(Endpoint) ->
    case re:run(Endpoint, "(http|https)[:][/][/]([^\\s.]+)[.].*") of
        nomatch -> 
            {error, "Endpoint is invalid in syntax, which looks like \"http://abc.cn-hangzhou.ots.aliyuncs.com/\""};
        {match, Cap} -> 
            {Offset, Length} = lists:nth(3, Cap),
            newAccessPoint(Endpoint, string:slice(Endpoint, Offset, Length))
    end.

-record(processor, {
          path :: string(),
          requestToPb :: fun((any()) -> binary()),
          responseFromPb :: fun(([{string(), string()}], binary()) -> any())}).
-record(innerClient, {
          credential :: #ots_Credential{},
          accessPoint :: #ots_AccessPoint{},
          processors :: #{atom() => #processor{}}}).

-spec newOtsClient(#ots_AccessPoint{}, #ots_Credential{}) -> {ok, #ots_Client{}}.

newOtsClient(AccessPoint, Credential) ->
    Processors = #{
      listTable => 
          #processor{
             path = "/ListTable",
             requestToPb = fun(_) -> ots_proto:encode_msg(#'ListTableRequest'{}) end,
             responseFromPb = fun listTable_respFromPb/2},
      createTable => 
          #processor{
            path = "/CreateTable",
            requestToPb = fun createTable_reqToPb/1,
            responseFromPb = fun createTable_respFromPb/2},
      deleteTable => 
          #processor{
            path = "/DeleteTable",
            requestToPb = fun deleteTable_reqToPb/1,
            responseFromPb = fun deleteTable_respFromPb/2}},
    inets:start(),
    InnerClient = #innerClient{
                     accessPoint=AccessPoint, 
                     credential=Credential,
                     processors=Processors},
    Client = #ots_Client{
                shutdown = fun() -> shutdown_(InnerClient) end,
                listTable = fun(ApiReq) -> issue(InnerClient, ApiReq, listTable) end,
                createTable = fun(ApiReq) -> issue(InnerClient, ApiReq, createTable) end,
                deleteTable = fun(ApiReq) -> issue(InnerClient, ApiReq, deleteTable) end},
    {ok, Client}.

-spec validate(any()) -> ok | {error, #ots_Error{}}.
validate(X) ->
    Res = validate_(X),
    case Res of
        {error, Msg} -> {error, error:newPredefined(invalidParameter, [{message, Msg}])};
        _ -> ok
    end.

-spec shutdown(#ots_Client{}) -> ok.
shutdown(Client) -> (Client#ots_Client.shutdown)().

-spec listTable(#ots_Client{}, #ots_ListTableRequest{}) -> 
                       {ok, #ots_ListTableResponse{}} | {error, #ots_Error{}}.
listTable(Client, ApiReq) -> (Client#ots_Client.listTable)(ApiReq).

-spec createTable(#ots_Client{}, #ots_CreateTableRequest{}) ->
                         {ok, #ots_CreateTableResponse{}} | {error, #ots_Error{}}.
createTable(Client, ApiReq) -> (Client#ots_Client.createTable)(ApiReq).

-spec deleteTable(#ots_Client{}, #ots_DeleteTableRequest{}) ->
                         {ok, #ots_DeleteTableResponse{}} | {error, #ots_Error{}}.
deleteTable(Client, ApiReq) -> (Client#ots_Client.deleteTable)(ApiReq).

%% Internal functions

shutdown_(_) -> inets:stop().

-spec listTable_respFromPb([{string(), string()}], string()) -> 
                                  #ots_ListTableResponse{}.
listTable_respFromPb(Headers, RespBody) ->
    PbResponse = ots_proto:decode_msg(RespBody, 'ListTableResponse'),
    #ots_ListTableResponse{
       requestId = getRequestId(Headers),
       tables = PbResponse#'ListTableResponse'.table_names}.

-spec createTable_reqToPb(#ots_CreateTableRequest{}) -> string().
createTable_reqToPb(ApiReq) ->
    ApiMeta = ApiReq#ots_CreateTableRequest.meta,
    ApiOptions = ApiReq#ots_CreateTableRequest.options,
    ReservedThroughput = ApiOptions#ots_TableOptions.reservedThroughput,
    PbReq = #'CreateTableRequest'{
               table_meta = toPb_TableMeta(ApiMeta),
               reserved_throughput = toPb_ReservedThroughput(ReservedThroughput),
               table_options = toPb_TableOptionsInCreateTable(ApiOptions)},
    ots_proto:encode_msg(PbReq).

-spec createTable_respFromPb([{string(), string()}], string()) ->
                                    #ots_CreateTableResponse{}.
createTable_respFromPb(Headers, _) ->
    #ots_CreateTableResponse{
       requestId = getRequestId(Headers)}.

-spec deleteTable_reqToPb(#ots_DeleteTableRequest{}) -> string().
deleteTable_reqToPb(ApiReq) ->
    PbReq = #'DeleteTableRequest'{
               table_name = ApiReq#ots_DeleteTableRequest.name},
    ots_proto:encode_msg(PbReq).

-spec deleteTable_respFromPb([{string(), string()}], string()) ->
                                    #ots_DeleteTableResponse{}.
deleteTable_respFromPb(Headers, _) ->
    #ots_DeleteTableResponse{
       requestId = getRequestId(Headers)}.
    
issue(OtsClient, ApiReq, Action) ->
    Res = validate(ApiReq),
    case Res of
        ok -> issue_(OtsClient, ApiReq, Action);
        {error, _} -> Res
    end.

issue_(OtsClient, ApiReq, Action) ->
    AccessPoint = OtsClient#innerClient.accessPoint,
    Credential = OtsClient#innerClient.credential,
    Endpoint = AccessPoint#ots_AccessPoint.endpoint,
    Proc = maps:get(Action, OtsClient#innerClient.processors),
    ReqBody = (Proc#processor.requestToPb)(ApiReq),
    Headers = fntools:chain(
                OtsClient,
                [fun baseHeaders/1,
                 fun(X) -> headerIfSts(X, Credential) end,
                 fun headerDate/1,
                 fun(X) -> headerBodyHash(X, ReqBody) end,
                 fun(X) -> headerSignature(X, Proc#processor.path, Credential#ots_Credential.accessKeySecret) end]),
    Url = Endpoint ++ Proc#processor.path,
    ContentType = "application/x.pb2",
    HttpOptions = [{timeout, 30000}, {connect_timeout, 30000}],
    Options = [{body_format, binary}],
    Resp = httpc:request(post, {Url, Headers, ContentType, ReqBody}, HttpOptions, Options),
    io:format("response: ~p~n", [Resp]),
    case Resp of
        {error, Reason} -> 
            case Reason of
                {failed_connect, _} -> {error, error:newPredefined(couldntConnect, [{message, Endpoint}])};
                {send_failed, _} -> {error, error:newPredefined(writeRequestFail, [])}
            end;
        {ok, {StatusLine, RespHeaders, RespBody}} ->
            Result = case validateResponse(RespHeaders, RespBody) of
                         {error, _} = E0 -> E0;
                         ok ->
                             {_, Status, _} = StatusLine,
                             if Status =:= 200 -> 
                                     ApiResp = (Proc#processor.responseFromPb)(RespHeaders, RespBody),
                                     {ok, ApiResp};
                                true -> {error, error:fromPb(RespBody, [{httpStatus, Status}])}
                             end
                     end,
            case Result of
                {ok, _} = Z -> Z;
                {error, X} -> {error, error:decorate(X, [{requestId, getRequestId(RespHeaders)}])}
            end
    end.

headerSignature(Headers, Path, AccessKeySecret) ->
    Hmac = crypto:hmac_init(sha, AccessKeySecret),
    crypto:hmac_update(Hmac, Path),
    crypto:hmac_update(Hmac, [$\n, $P, $O, $S, $T, $\n, $\n]),
    OtsHeaders = lists:sort(
                   fun({K0, _}, {K1, _}) -> K0 =< K1 end, 
                   [{K, V} || {K, V} <- Headers,
                              string:prefix(K, "x-ots-") /= nomatch]),
    lists:map(fun({K, V}) ->
                      crypto:hmac_update(Hmac, K),
                      crypto:hmac_update(Hmac, [$:]),
                      crypto:hmac_update(Hmac, V),
                      crypto:hmac_update(Hmac, [$\n]) end,
              OtsHeaders),
    Digest = crypto:hmac_final(Hmac),
    [{"x-ots-signature", base64:encode_to_string(Digest)} | Headers].

headerBodyHash(Headers, Body) ->
    Digest = base64:encode_to_string(crypto:hash(md5, Body)),
    [{"x-ots-contentmd5", Digest} | Headers].

headerDate(Headers) ->
    {{Year, Month, Day}, {Hour, Minute, Sec}} = calendar:universal_time(),
    [{"x-ots-date", 
      lists:flatten(
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.000000Z",
                      [Year, Month, Day, Hour, Minute, Sec]), 
        [])}
     | Headers].

headerIfSts(Headers, Credential) ->
    case Credential#ots_Credential.securityToken of
        undefined -> Headers;
        Sts -> [{"x-ots-ststoken", Sts} | Headers]
    end.

baseHeaders(OtsClient) ->
    AccessPoint = OtsClient#innerClient.accessPoint,
    Credential = OtsClient#innerClient.credential,
    [{"x-ots-apiversion", "2015-12-31"},
     {"x-ots-accesskeyid", Credential#ots_Credential.accessKeyId},
     {"x-ots-instancename", AccessPoint#ots_AccessPoint.instance},
     {"User-Agent", "aliyun-tablestore-sdk-erlang/4.0.0"},
     {"Accept", "application/x.pb2"}].

find([], _) -> undefined;
find([{K, V} | T], Key) -> 
    if K =:= Key -> V;
       true -> find(T, Key)
    end.
     
-spec getRequestId([{string(), string()}]) -> string().

getRequestId(Headers) ->
    case find(Headers, "x-ots-requestid") of
        undefined -> "";
        X -> X
    end.

validateResponse(Headers, Body) ->
    case find(Headers, "x-ots-contentmd5") of
        undefined -> {error, 
                      error:newPredefined(corruptedResponse, 
                                          [{message, "no \"x-ots-contentmd5\" in response headers"}])};
        ExpectedDigest -> 
            RealDigest = base64:encode_to_string(crypto:hash(md5, Body)),
            if ExpectedDigest =:= RealDigest -> ok;
               true ->
                    ErrMsg = lists:flatten(
                               io_lib:format(
                                 "response digest mismatches: expect=~s, real=~s", 
                                 [ExpectedDigest, RealDigest])),
                    {error, error:newPredefined(corruptedResponse, [{message, ErrMsg}])}
            end
    end.

-ifndef(DIALYZER).
-define(CHECK_UNDEF(X), X =:= undefined).
-else.
-define(CHECK_UNDEF(X), false).
-endif.

validate_([]) -> ok;
validate_([H|T]) ->
    case validate_(H) of
        {error, _} = X -> X;
        ok -> validate_(T)
    end;
validate_(X) when is_record(X, ots_ListTableRequest) -> ok;
validate_(X) when is_record(X, ots_PrimaryKeyColumnSchema) ->
    Name = X#ots_PrimaryKeyColumnSchema.name,
    Type = X#ots_PrimaryKeyColumnSchema.type,
    Option = X#ots_PrimaryKeyColumnSchema.option,
    if ?CHECK_UNDEF(Name) orelse length(Name) =:= 0 -> 
            {error, "\"name\" is required in PrimaryKeySchema."};
       ?CHECK_UNDEF(Type) -> 
            {error, "\"type\" is required in PrimaryKeySchema."};
       Type =/= integer andalso Type =/= string andalso Type =/= binary -> 
            {error, "\"type\" of PrimaryKeySchema must be integer, string or binary."};
       Option =/= undefined andalso Option =/= autoIncr ->
            {error, "\"option\" of PrimaryKeySchema must be either undefined or autoIncr."};
       true -> ok
    end;
validate_(X) when is_record(X, ots_TableMeta) ->
    Name = X#ots_TableMeta.name,
    Schema = X#ots_TableMeta.schema,
    if ?CHECK_UNDEF(Name) orelse length(Name) =:= 0 -> 
            {error, "Table name is required."};
       ?CHECK_UNDEF(Schema) orelse length(Schema) =:= 0 ->
            {error, "Table schema must be nonempty."};
       true -> validate_(Schema)
    end;
validate_(X) when is_record(X, ots_CapacityUnit) ->
    if X#ots_CapacityUnit.read =/= undefined andalso X#ots_CapacityUnit.read < 0 ->
            {error, "Read capacity unit must be nonnegative."};
       X#ots_CapacityUnit.write =/= undefined andalso X#ots_CapacityUnit.write < 0 ->
            {error, "Write capacity unit must be nonnegative."};
       true -> ok
    end;
validate_(X) when is_record(X, ots_TableOptions) -> 
    fntools:chain(
      X,
      [fun(_) ->
               case X#ots_TableOptions.timeToLive of
                   undefined -> ok;
                   Y ->
                       TTL = chrono:toUsec(Y),
                       if TTL =< 0 -> {error, "TimeToLive must be positive."};
                          TTL rem 1000000 /= 0 -> 
                               {error, "TimeToLive must be integral multiple of seconds."};
                          true -> ok
                       end
               end
       end,
       fun(_) ->
               case X#ots_TableOptions.maxVersions of
                   undefined -> ok;
                   Y ->
                       if Y =< 0 -> {error, "MaxVersions must be positive."};
                          true -> ok
                       end
               end
       end,
       fun(_) ->
               case X#ots_TableOptions.maxTimeDeviation of
                   undefined -> ok;
                   Y ->
                       Dev = chrono:toUsec(Y),
                       if Dev =< 0 -> 
                               {error, "MaxTimeDeviation must be positive."};
                          Dev rem 1000000 /= 0 ->
                               {error, "MaxTimeDeviation must be integral multiple of seconds."};
                          true -> ok
                       end
               end
       end,
       fun(_) ->
               case X#ots_TableOptions.reservedThroughput of
                   undefined -> ok;
                   Y -> validate_(Y)
               end
       end]);
validate_(X) when is_record(X, ots_CreateTableRequest) -> 
    fntools:chain(
      X,
      [fun(_) -> 
               Y = X#ots_CreateTableRequest.meta,
               if ?CHECK_UNDEF(Y) -> 
                       {error, "Meta is required for creating a table."};
                  true -> validate_(Y)
               end
       end,
       fun(_) -> 
               Y = X#ots_CreateTableRequest.options,
               if ?CHECK_UNDEF(Y) -> 
                       {error, "Options is required for creating a table."};
                  true -> validate_(Y)
               end
       end,
       fun(_) ->
               Opts = X#ots_CreateTableRequest.options,
               ?assert(Opts =/= undefined),
               fntools:chain(
                 Opts,
                 [fun(_) -> 
                          Y = Opts#ots_TableOptions.reservedThroughput,
                          if Y =:= undefined -> ok;
                             true -> 
                                  fntools:chain(
                                    Y,
                                    [fun(_) ->
                                             if Y#ots_CapacityUnit.read =:= undefined orelse Y#ots_CapacityUnit.write =:= undefined ->
                                                     {error, "Both read and write capacity units are required."};
                                                true -> ok
                                             end
                                     end,
                                     fun(_) -> validate_(Y) end])
                            end
                  end])
       end]);
validate_(X) when is_record(X, ots_DeleteTableRequest) -> 
    Y = X#ots_DeleteTableRequest.name,
    if ?CHECK_UNDEF(Y) ->
            {error, "Table name is required in a DeleteTableRequest."};
       length(Y) =:= 0 ->
            {error, "Table name is required in a DeleteTableRequest."};
       true -> ok
    end.


-spec toPb_CapacityUnit(#ots_CapacityUnit{}) -> #'CapacityUnit'{}.
toPb_CapacityUnit(Cu) ->
    fntools:chain(
      #'CapacityUnit'{},
      [fun(X) ->
               Read = Cu#ots_CapacityUnit.read,
               if Read =/= undefined ->
                       X#'CapacityUnit'{read = Read};
                  true -> X
               end
       end,
       fun(X) ->
               Write = Cu#ots_CapacityUnit.write,
               if Write =/= undefined ->
                       X#'CapacityUnit'{write = Write};
                  true -> X
               end
       end]).

-spec toPb_TableOptionsInCreateTable(#ots_TableOptions{}) -> #'TableOptions'{}.
toPb_TableOptionsInCreateTable(To) ->
    fntools:chain(
      #'TableOptions'{},
      [fun(X) ->
               TTL = To#ots_TableOptions.timeToLive,
               if TTL =/= undefined ->
                       X#'TableOptions'{time_to_live = chrono:toUsec(TTL) div 1000000};
                  true -> X#'TableOptions'{time_to_live = -1}
               end
       end,
       fun(X) ->
               MaxVersions = To#ots_TableOptions.maxVersions,
               if MaxVersions =/= undefined ->
                       X#'TableOptions'{max_versions = MaxVersions};
                  true -> X
               end
       end,
       fun(X) ->
               Dev = To#ots_TableOptions.maxTimeDeviation,
               if Dev =/= undefined ->
                       X#'TableOptions'{deviation_cell_version_in_sec = chrono:toUsec(Dev) div 1000000};
                  true -> X
               end
       end]).
                       

-spec toPb_TableMeta(#ots_TableMeta{}) -> #'TableMeta'{}.
toPb_TableMeta(ApiMeta) ->
    #'TableMeta'{
       table_name = ApiMeta#ots_TableMeta.name,
       primary_key = [toPb_Schema(X) || X <- ApiMeta#ots_TableMeta.schema]}.

-spec toPb_Schema(#ots_PrimaryKeyColumnSchema{}) -> #'PrimaryKeySchema'{}.
toPb_Schema(ApiSchema) -> 
    fntools:chain(
      #'PrimaryKeySchema'{
        name = ApiSchema#ots_PrimaryKeyColumnSchema.name},
      [fun(X) ->
               case ApiSchema#ots_PrimaryKeyColumnSchema.type of
                   integer ->
                       X#'PrimaryKeySchema'{type = 'INTEGER'};
                   string ->
                       X#'PrimaryKeySchema'{type = 'STRING'};
                   binary ->
                       X#'PrimaryKeySchema'{type = 'BINARY'}
               end
       end,
       fun(X) ->
               case ApiSchema#ots_PrimaryKeyColumnSchema.option of
                   undefined -> X;
                   autoIncr -> X#'PrimaryKeySchema'{option = 'AUTO_INCREMENT'}
               end
       end]).
                        
toPb_ReservedThroughput(ApiCapacityUnit) ->
    RealCu = if ApiCapacityUnit =:= undefined -> 
                     #ots_CapacityUnit{read = 0, write = 0};
                true -> ApiCapacityUnit
             end,
    #'ReservedThroughput'{
       capacity_unit = toPb_CapacityUnit(RealCu)}.
