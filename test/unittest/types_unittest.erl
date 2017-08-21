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

-module(types_unittest).
-include_lib("ots.hrl").
-include_lib("stdlib/include/assert.hrl").

cases() ->
    #{"ut.TableMeta.ValidateTableName" =>
          testa:is(
            fun(_) ->
                    Meta = #ots_TableMeta{
                              schema = [#ots_PrimaryKeyColumnSchema{
                                           name = "pkey", 
                                           type = integer}]},
                    ots:validate(Meta)
            end,
            {error, #ots_Error{
                       httpStatus = 400, 
                       errorCode = "OTSParameterInvalid",
                       message = "Table name is required.",
                       requestId = ""}}),
      "ut.TableMeta.ValidatePrimaryKeyOption" =>
          testa:is(
            fun(_) ->
                    Meta = #ots_TableMeta{
                              name = "table",
                              schema = [#ots_PrimaryKeyColumnSchema{
                                           name = "pkey", 
                                           type = integer,
                                           option = unknown}]},
                    ots:validate(Meta)
            end,
            {error, #ots_Error{
                       httpStatus = 400, 
                       errorCode = "OTSParameterInvalid",
                       message = "\"option\" of PrimaryKeySchema must be either undefined or autoIncr.",
                       requestId = ""}}),
      "ut.TableMeta.ValidateSchemaNonemptiness" =>
          testa:is(
            fun(_) ->
                    Meta = #ots_TableMeta{
                             name = "table"},
                    ots:validate(Meta)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "Table schema must be nonempty.",
                       requestId = ""}}),
      "ut.CapacityUnit.Validate_0" =>
          testa:is(
            fun(_) ->
                    CU = #ots_CapacityUnit{read = -1},
                    ots:validate(CU)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "Read capacity unit must be nonnegative.",
                       requestId = ""}}),
      "ut.CapacityUnit.Validate_1" =>
          testa:is(
            fun(_) ->
                    CU = #ots_CapacityUnit{write = -1},
                    ots:validate(CU)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "Write capacity unit must be nonnegative.",
                       requestId = ""}}),
      "ut.TableOptionsForCreateTable.Validate_FractionalTTL" =>
          testa:is(
            fun(_) ->
                    To = #ots_TableOptionsForCreateTable{
                            timeToLive = chrono:duration(1, usec)},
                    ots:validate(To)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "TimeToLive must be integral multiple of seconds.",
                       requestId = ""}}),
      "ut.TableOptionsForCreateTable.Validate_NonpositiveTTL" =>
          testa:is(
            fun(_) ->
                    To = #ots_TableOptionsForCreateTable{
                            timeToLive = chrono:duration(0, sec)},
                    ots:validate(To)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "TimeToLive must be positive.",
                       requestId = ""}}),
      "ut.TableOptionsForCreateTable.MaxVersions_NonposiveMaxVersions" =>
          testa:is(
            fun(_) ->
                    To = #ots_TableOptionsForCreateTable{
                            maxVersions = 0},
                    ots:validate(To)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "MaxVersions must be positive.",
                       requestId = ""}}),
      "ut.TableOptionsForCreateTable.MaxTimeDeviation_Fractional" =>
          testa:is(
            fun(_) ->
                    To = #ots_TableOptionsForCreateTable{
                            maxTimeDeviation = chrono:duration(1, usec)},
                    ots:validate(To)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "MaxTimeDeviation must be integral multiple of seconds.",
                       requestId = ""}}),
      "ut.TableOptionsForCreateTable.MaxTimeDeviation_Nonpositive" =>
          testa:is(
            fun(_) ->
                    To = #ots_TableOptionsForCreateTable{
                            maxTimeDeviation = chrono:duration(0, sec)},
                    ots:validate(To)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "MaxTimeDeviation must be positive.",
                       requestId = ""}}),
      "ut.CreateTableRequest.ReservedThroughputReadExistence" =>
          testa:is(
            fun(_) ->
                    Meta = #ots_TableMeta{
                              name = "table",
                              schema = [#ots_PrimaryKeyColumnSchema{
                                           name = "pk",
                                           type = integer}]},
                    Opts = #ots_TableOptionsForCreateTable{
                              reservedThroughput = #ots_CapacityUnit{write = 0},
                              timeToLive = chrono:duration(1, hour),
                              maxVersions = 1,
                              maxTimeDeviation = chrono:duration(1, hour)},
                    Req = #ots_CreateTableRequest{
                             meta = Meta,
                             options = Opts},
                    ots:validate(Req)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "Both read and write capacity units are required.",
                       requestId = ""}}),
      "ut.CreateTableRequest.ReservedThroughputWriteExistence" =>
          testa:is(
            fun(_) ->
                    Meta = #ots_TableMeta{
                              name = "table",
                              schema = [#ots_PrimaryKeyColumnSchema{
                                           name = "pk",
                                           type = integer}]},
                    Opts = #ots_TableOptionsForCreateTable{
                              reservedThroughput = #ots_CapacityUnit{read = 0},
                              timeToLive = chrono:duration(1, hour),
                              maxVersions = 1,
                              maxTimeDeviation = chrono:duration(1, hour)},
                    Req = #ots_CreateTableRequest{
                             meta = Meta,
                             options = Opts},
                    ots:validate(Req)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "Both read and write capacity units are required.",
                       requestId = ""}}),
      "ut.CreateTableRequest.ReservedThroughputReadNonnegative" =>
          testa:is(
            fun(_) ->
                    Meta = #ots_TableMeta{
                              name = "table",
                              schema = [#ots_PrimaryKeyColumnSchema{
                                           name = "pk",
                                           type = integer}]},
                    Opts = #ots_TableOptionsForCreateTable{
                              reservedThroughput = #ots_CapacityUnit{
                                                      read = -1,
                                                      write = 0},
                              timeToLive = chrono:duration(1, hour),
                              maxVersions = 1,
                              maxTimeDeviation = chrono:duration(1, hour)},
                    Req = #ots_CreateTableRequest{
                             meta = Meta,
                             options = Opts},
                    ots:validate(Req)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "Read capacity unit must be nonnegative.",
                       requestId = ""}}),
      "ut.CreateTableRequest.ReservedThroughputWriteNonnegative" =>
          testa:is(
            fun(_) ->
                    Meta = #ots_TableMeta{
                              name = "table",
                              schema = [#ots_PrimaryKeyColumnSchema{
                                           name = "pk",
                                           type = integer}]},
                    Opts = #ots_TableOptionsForCreateTable{
                              reservedThroughput = #ots_CapacityUnit{
                                                       read = 0,
                                                       write = -1},
                              timeToLive = chrono:duration(1, hour),
                              maxVersions = 1,
                              maxTimeDeviation = chrono:duration(1, hour)},
                    Req = #ots_CreateTableRequest{
                             meta = Meta,
                             options = Opts},
                    ots:validate(Req)
            end,
            {error, #ots_Error{
                       httpStatus = 400,
                       errorCode = "OTSParameterInvalid",
                       message = "Write capacity unit must be nonnegative.",
                       requestId = ""}})
}.


main(Args) ->
    testa:main(Args, cases()).
