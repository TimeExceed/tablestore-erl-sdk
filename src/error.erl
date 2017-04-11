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

-module(error).
-include("ots.hrl").
-include("ots_proto.hrl").

-export([fromPb/2]).
-export([newPredefined/2]).
-export([decorate/2]).

fromPb(RespBody, Opts) ->
    PbError = ots_proto:decode_msg(RespBody, 'Error'),
    Res = #ots_Error{
             httpStatus = 0,
             errorCode = PbError#'Error'.code,
             message = PbError#'Error'.message,
             requestId = ""},
    decorate(Res, Opts).

newPredefined(ErrorCode, Opts) ->
    Error = case ErrorCode of
                corruptedResponse -> #ots_Error{
                                        httpStatus = 56,
                                        errorCode = "OTSCorruptedResponse",
                                        message = "",
                                        requestId = ""};
                couldntConnect -> #ots_Error{
                                     httpStatus = 7,
                                     errorCode = "OTSCouldntConnect",
                                     message = "",
                                     requestId = ""};
                writeRequestFail -> #ots_Error{
                                       httpStatus = 55,
                                       errorCode = "OTSWriteRequestFail",
                                       message = "",
                                       requestId = ""};
                invalidParameter -> #ots_Error{
                                       httpStatus = 400,
                                       errorCode = "OTSParameterInvalid",
                                       message = "",
                                       requestId = ""}
            end,
    decorate(Error, Opts).
%% kPredefined_OperationTimeout,

decorate(Error, []) -> Error;
decorate(Error, [{K, V} | T]) -> 
    NewError = case K of
                   httpStatus -> Error#ots_Error{httpStatus = V};
                   requestId -> Error#ots_Error{requestId = V};
                   message -> Error#ots_Error{message = V}
               end,
    decorate(NewError, T).


