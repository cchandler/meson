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
%% @doc The meson core application start module
%% 
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/cchandler/meson/


% Start the app from the command line
% erl -boot start_sasl -eval 'application:lomeson_core), application:start(meson_core).'

-module(meson_core).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, StartArgs) ->
	case meson_core_supervisor:start_link(StartArgs) of
		{ok, Pid} ->
			{ok,Pid};
		Error ->
			Error
	end.
	
stop(_State) ->
	ok.