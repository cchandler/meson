%% Copyright (c) 2009 Chris Chandler <chris@chrischandler.name>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% 
%% @author Chris Chandler <chris@chrischandler.name>
%% @copyright 2009 Chris Chandler
%% @version 0.1alpha
%% @doc The meson goal server supervisor
%% 
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/cchandler/meson/

-module(meson_core_supervisor).

-behavior(supervisor).

-export([start/0, start_in_shell/0, init/1, start_link/1]).

start() ->
	spawn( fun() ->
		supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
		end).

start_in_shell() ->
	{ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid).

start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
	{ok, {{one_for_one, 3, 10},
	[{tag1,
		{goal_server, start_link, ["localhost",[] ]},
		permanent,
		10000,
		worker,
		[goal_server]}
		]}}.