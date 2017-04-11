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

-module(chrono_unittest).
-include_lib("stdlib/include/assert.hrl").

main(Args) ->
    Cases = #{
      "ut.Chrono_UtcNow_positive" =>
          testa:verify(
            fun(_, _) ->
                    chrono:utcNow()
            end,
            fun(_, Res, _) ->
                    X = chrono:toUsec(Res),
                    if X > 0 -> ok;
                       true -> {error, "UTC time must be positive right now."}
                    end
            end,
            fun(_, Case) ->
                    Case(0)
            end),
      "ut.Chrono_Utc_duration" =>
          testa:verify(
            fun(_, Dur) ->
                    Start = chrono:utcNow(),
                    chrono:sleepFor(Dur),
                    End = chrono:utcNow(),
                    chrono:diff(End, Start)
            end,
            fun(_, Res, In) ->
                    InUsec = chrono:toUsec(In),
                    ResUsec = chrono:toUsec(Res),
                    if InUsec - 10000 =< ResUsec andalso ResUsec =< InUsec + 10000 -> ok;
                       true -> {error, io_lib:format("Time differs too much: Real=~p Expect=~p~n", [Res, In])}
                    end
            end,
            fun(_, Case) -> Case(chrono:duration(1, sec)) end),
      "ut.Chrono_Monotonic_duration" =>
          testa:verify(
            fun(_, Dur) ->
                    Start = chrono:monotonicNow(),
                    chrono:sleepFor(Dur),
                    End = chrono:monotonicNow(),
                    chrono:diff(End, Start)
            end,
            fun(_, Res, In) ->
                    InUsec = chrono:toUsec(In),
                    ResUsec = chrono:toUsec(Res),
                    if InUsec =< ResUsec andalso ResUsec =< InUsec + 10000 -> ok;
                       true -> {error, io_lib:format("Time differs too much: Real=~p Expect=~p~n", [Res, In])}
                    end
            end,
            fun(_, Case) -> Case(chrono:duration(1, sec)) end)},
    testa:main(Args, Cases).

