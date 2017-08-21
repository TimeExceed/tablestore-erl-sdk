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

-module(fntools).

-export([chain/2]).
-export([maybe_chain/2]).
-export_type([maybe/1]).

-type maybe(T) :: T | none.

-spec chain(any(), [fun((any()) -> any() | {error, any()})]) -> any() | {error, any()}.
chain(X, []) -> X;
chain(X, [H|T]) ->
    case H(X) of
        {error, _} = Z -> Z;
        Y -> chain(Y, T)
    end.

-spec maybe_chain(maybe(any()), [fun((any()) -> maybe(any()))]) -> maybe(any()).
maybe_chain(X, []) -> X;
maybe_chain(X, [H|T]) -> 
    case X of
        none -> none;
        _ -> maybe_chain(H(X), T)
    end.
