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

-module(fntools_unittest).
-include_lib("stdlib/include/assert.hrl").

cases() ->
    #{"ut.fntools.chain.gothrough" =>
          testa:is(
            fun(_) ->
                    fntools:chain(
                      "1",
                      [fun(X) -> case string:to_integer(X) of
                                     {error, _} = Z -> Z;
                                     {Y, _} -> Y
                                 end
                       end,
                       fun(X) -> X + 1 end])
            end,
            2),
      "ut.fntools.chain.fall" =>
          testa:is(
            fun(_) ->
                    fntools:chain(
                      "a",
                      [fun(X) -> case string:to_integer(X) of
                                     {error, _} = Z -> Z;
                                     {Y, _} -> Y
                                 end
                       end,
                       fun(X) -> X + 1 end])
            end,
            {error, no_integer}),
      "ut.fntools.maybe_chain.init_none" =>
          testa:is(
            fun(_) ->
                    fntools:maybe_chain(
                      none,
                      [fun(X) -> X + 1 end])
            end,
            none),
      "ut.fntools.maybe_chain.gothrough" =>
          testa:is(
            fun(_) ->
                    fntools:maybe_chain(
                      "1",
                      [fun(X) -> case string:to_integer(X) of
                                     {error, _} -> none;
                                     {Y, _} -> Y
                                 end
                       end,
                       fun(X) -> X + 1 end])
            end,
            2),
      "ut.fntools.maybe_chain.fall" =>
          testa:is(
            fun(_) ->
                    fntools:maybe_chain(
                      "a",
                      [fun(X) -> case string:to_integer(X) of
                                     {error, _} -> none;
                                     {Y, _} -> Y
                                 end
                       end,
                       fun(X) -> X + 1 end])
            end,
            none)}.
    

main(Args) ->
    Cases = cases(),
    testa:main(Args, Cases).

