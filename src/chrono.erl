%%! +C single_time_warp 
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

-module(chrono).

-export_type([duration/0, utc/0, monotonic/0]).
-export([toUsec/1]).
-export([utcNow/0, monotonicNow/0]).
-export([utc/2, monotonic/2, duration/2]).
-export([diff/2]).
-export([sleepFor/1, sleepUntil/1]).

%% types

-record(recDuration, {v :: integer()}).
-record(recUtc, {v :: integer()}).
-record(recMonotonic, {v :: integer()}).

-opaque duration() :: #recDuration{}.
-opaque utc() :: #recUtc{}.
-opaque monotonic() :: #recMonotonic{}.

%% APIs

-spec toUsec(utc()) -> integer();
            (duration()) -> integer();
            (monotonic()) -> integer().
toUsec({recDuration, X}) -> X;
toUsec({recUtc, X}) -> X;
toUsec({recMonotonic, X}) -> X.

-spec utcNow() -> utc().
utcNow() -> #recUtc{v = erlang:system_time(microsecond)}.

-spec monotonicNow() -> monotonic().
monotonicNow() -> #recMonotonic{v = erlang:monotonic_time(microsecond)}.

-spec utc(integer(), usec | msec | sec | min | hour) -> utc().
utc(X, Unit) ->
    #recUtc{v = X * multiply(Unit)}.

-spec monotonic(integer(), usec | msec | sec | min | hour) -> monotonic().
monotonic(X, Unit) ->
    #recMonotonic{v = X * multiply(Unit)}.

-spec duration(integer(), usec | msec | sec | min | hour) -> duration().
duration(X, Unit) ->
    #recDuration{v = X * multiply(Unit)}.

-spec diff(utc(), utc()) -> duration();
          (monotonic(), monotonic()) -> duration().
diff({recUtc, X}, {recUtc, Y}) -> #recDuration{v = X - Y};
diff({recMonotonic, X}, {recMonotonic, Y}) -> #recDuration{v = X - Y}.

-spec sleepFor(duration()) -> ok.
sleepFor({recDuration, X}) ->
    if X =< 0 -> ok;
       true -> 
            Y = (X + 999) div 1000,
            timer:sleep(Y)
    end.

-spec sleepUntil(monotonic()) -> ok.
sleepUntil(X) ->
    Delta = diff(X, monotonicNow()),
    sleepFor(Delta).

%% internal functions

multiply(Unit) ->
    case Unit of
        usec -> 1;
        msec -> 1000;
        sec -> 1000 * 1000;
        min -> 1000 * 1000 * 60;
        hour -> 1000 * 1000 * 60 * 60
    end.
             
