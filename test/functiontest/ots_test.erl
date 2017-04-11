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

-module(ots_test).
-include_lib("ots.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([main/1]).

main(Args) ->
    Cases = #{
      "ft_listTable" =>
          testa:verify(
            fun(_, OtsClient) ->
                    Result = ots:listTable(OtsClient, #ots_ListTableRequest{}),
                    io:format("~p~n", [Result]),
                    Result
            end,
            fun(_, Res, _) ->
                    case Res of
                        {error, _} -> Res;
                        {ok, Resp} ->
                            fntools:chain(
                              Resp,
                              [fun(_) ->
                                       ReqId = Resp#ots_ListTableResponse.requestId,
                                       if length(ReqId) > 0 -> ok;
                                          true -> {error, "request id is missing."}
                                       end
                               end,
                               fun(_) ->
                                       Tables = Resp#ots_ListTableResponse.tables,
                                       Any = lists:any(fun(X) -> X =:= "pet" end, Tables),
                                       if Any -> ok;
                                          true -> {error, "\"pet\" is missing."}
                                       end
                               end])
                    end
            end,
            fun(X, Y) -> tb(X, Y) end)},
    testa:main(Args, Cases).

extractCredential() ->
    AkId = os:getenv("OTS_ACCESS_KEY_ID"),
    ?assert(AkId =/= false),
    AkSecret = os:getenv("OTS_ACCESS_KEY_SECRET"),
    ?assert(AkSecret =/= false),
    case os:getenv("OTS_SECURITY_TOKEN") of
        false -> ots:newCredential(AkId, AkSecret);
        SecurityToken -> ots:newCredential(AkId, AkSecret, SecurityToken)
    end.

extractAccessPoint() ->
    Endpoint = os:getenv("OTS_ENDPOINT"),
    ?assert(Endpoint /= false),
    case os:getenv("OTS_INSTANCE") of
        false -> ots:newAccessPoint(Endpoint);
        Inst -> ots:newAccessPoint(Endpoint, Inst)
    end.

tb(CaseName, Case) ->
    {ok, AccessPoint} = extractAccessPoint(),
    {ok, Credential} = extractCredential(),
    {ok, OtsClient} = ots:newOtsClient(AccessPoint, Credential),
    CtRes = createTable(CaseName, OtsClient),
    case CtRes of
        {error, _} -> CtRes;
        {ok, _} ->
            Result = Case(OtsClient),
            deleteTable(CaseName, OtsClient),
            ots:shutdown(OtsClient),
            Result
    end.

createTable(CaseName, OtsClient) ->
    Req = makeCreateTableRequest(CaseName),
    ots:createTable(OtsClient, Req).

makeCreateTableRequest(CaseName) ->
    Meta = #ots_TableMeta{
              name = CaseName,
              schema = [#ots_PrimaryKeyColumnSchema{
                           name = "pkInt",
                           type = integer},
                        #ots_PrimaryKeyColumnSchema{
                           name = "pkStr",
                           type = string},
                        #ots_PrimaryKeyColumnSchema{
                           name = "pkBlob",
                           type = binary},
                        #ots_PrimaryKeyColumnSchema{
                           name = "pkAutoIncr",
                           type = integer,
                           option = autoIncr}]},
    Opt = #ots_TableOptions{
            maxVersions = 1},
    #ots_CreateTableRequest{
       meta = Meta,
       options = Opt}.

deleteTable(CaseName, OtsClient) ->
    Req = #ots_DeleteTableRequest{name = CaseName},
    ots:deleteTable(OtsClient, Req).
