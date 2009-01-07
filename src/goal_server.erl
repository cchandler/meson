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
%% @doc The goal server module
%% 
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/cchandler/meson/

-module(goal_server).

-behaviour(gen_server).
-behaviour(gen_mod).
% -behaviour(gen_mod).

-include_lib("rabbit_erlang_client/include/amqp_client.hrl").

%export for gen_mod
-export([start_link/2, start/2, stop/0]).
% -export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
	
-define(SERVER, goal-finder).
-define(PROCNAME, goal_finder).

-record(state, {host}).

start(Host, Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Opts], []).

stop() -> gen_server:call(?MODULE, stop).
	
start_link(Host,Opts) -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Opts], []).

init([Host,Opts]) -> 
	% Activate a receiver
	% Activate a sender
	Pid = spawn( fun loop/0 ),
	register(goalsender, Pid),
	
	{ok, Consumer} = gen_event:start_link(),
    gen_event:add_handler(Consumer, goal_server_consumer , [] ),
	Connection = amqp_connection:start("internal", "password", "localhost", <<"internal">>),
	Channel = lib_amqp:start_channel(Connection),
	Q = <<"find-goal">>,
	lib_amqp:subscribe(Channel, Q, Consumer, true),
	io:format("Subscribing~n", []),
	
	% MyHost = gen_mod:get_opt_host(Host, Opts, "meson.@HOST@"),
	{ok, #state{host = Host}}.

handle_call(stop, _From, State) -> {stop, normal, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

% handle_info(_Info, State) -> {noreply, State};
handle_info({what}, State) -> 
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, Extra) -> {ok, State}.
	
%%% Borrowed from mod_meson
loop() ->
	{Connection, Channel} = setup_connection_and_channel(),
	send_to_amqp(Connection,Channel),
	teardown_connection_and_channel(Connection,Channel),
	ok.
	
setup_connection_and_channel() ->
	Connection = amqp_connection:start("internal", "password", "localhost", <<"parrotlabs">>),
	Channel = lib_amqp:start_channel(Connection),
	{Connection,Channel}.

teardown_connection_and_channel(Connection, Channel) ->
	lib_amqp:teardown(Connection,Channel).
	
send_to_amqp(Connection,Channel) ->
	receive
		{send_to_amqp, Vhost, Queue, Exchange, Binding, Payload} ->
			io:format("Attempting to send message to vhost ~p on queue ~p with payload ~p", [Vhost,Queue, Payload]),
			lib_amqp:declare_queue(Channel, Queue),
			lib_amqp:declare_exchange(Channel, Exchange),
			lib_amqp:bind_queue(Channel, Exchange, Queue, Binding),
			lib_amqp:publish(Channel, Exchange, Binding, Payload),
			send_to_amqp(Connection, Channel);
		{close} ->
			lib_amqp:teardown(Connection,Channel)
	end.