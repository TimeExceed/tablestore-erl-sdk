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

-record(ots_Credential, {
          accessKeyId :: string(), 
          accessKeySecret :: string(), 
          securityToken :: undefined | string()}).

-record(ots_AccessPoint, {
          endpoint :: string(),
          instance :: string()}).

-record(ots_Error, {
         httpStatus :: integer(),
         errorCode :: string(),
         message :: string(),
         requestId :: string()}).

-record(ots_ListTableRequest, {}).
-record(ots_ListTableResponse, {
         requestId :: string(),
         tables :: [string()]}).

-record(ots_PrimaryKeyColumnSchema, {
          name :: string(),
          type :: integer | string | binary,
          option :: undefined | autoIncr}).

-record(ots_TableMeta, {
          name :: string(),
          schema :: [#ots_PrimaryKeyColumnSchema{}]}).

-record(ots_CapacityUnit, {
          read :: undefined | integer(),
          write :: undefined | integer()}).

-record(ots_TableOptions, {
          reservedThroughput :: undefined | #ots_CapacityUnit{},
          timeToLive :: undefined | chrono:duration(),
          maxVersions :: undefined | integer(),
          maxTimeDeviation :: undefined | chrono:duration()}).

-record(ots_CreateTableRequest, {
          meta :: #ots_TableMeta{},
          options :: #ots_TableOptions{}}).
-record(ots_CreateTableResponse, {
          requestId :: string()}).

-record(ots_DeleteTableRequest, {
          name :: string()}).
-record(ots_DeleteTableResponse, {
          requestId :: string()}).

-record(ots_DescribeTableRequest, {
          name :: string()}).
-record(ots_DescribeTableResponse, {
          meta :: #ots_TableMeta{},
          options :: #ots_TableOptions{},
          status :: active | inactive | loading | unloading | updating,
          shardSplitPoints}).

-record(ots_Client, {
          shutdown :: fun(() -> ok),
          listTable :: fun((#ots_ListTableRequest{}) -> 
                                  {ok, #ots_ListTableResponse{}} | {error, #ots_Error{}}),
          createTable :: fun((#ots_CreateTableRequest{}) -> 
                                    {ok, #ots_CreateTableResponse{}} | {error, #ots_Error{}}),
          deleteTable :: fun((#ots_DeleteTableRequest{}) ->
                                    {ok, #ots_DeleteTableResponse{}} | {error, #ots_Error{}}),
          describeTable :: fun((#ots_DescribeTableRequest{}) ->
                                      {ok, #ots_DescribeTableResponse{}} | {error, #ots_Error{}})}).

