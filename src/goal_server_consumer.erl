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
%% @doc The goal server consumer module
%% 
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/cchandler/meson/

-module(goal_server_consumer).

-behaviour(gen_event).

-include("../include/meson_core_records.hrl").

-include_lib("rabbitmq_server/include/rabbit_framing.hrl").
-include_lib("rfc4627/include/rfc4627.hrl").

-export([init/1, handle_info/2, terminate/2]).
-export([code_change/3, handle_call/2, handle_event/2]).

% -record(meson_message, {goal,body, jid}).

init(_Args) ->
	io:format("Initialized a receiver", []),
    {ok, []}.

handle_call(_Request, State) ->
    {ok, not_understood, State}.

handle_event(_Event, State) ->
    {ok, State}.

handle_info(shutdown, State) ->
    io:format("AMQP Consumer SHUTDOWN~n", []),
    {remove_handler, State};

handle_info(#'basic.consume_ok'{consumer_tag = ConsumerTag}, State) ->
    io:format("AMQP Consumer, rec'd consume ok, tag= ~p~n", [ConsumerTag] ),
    {ok, State};

handle_info(#'basic.cancel_ok'{consumer_tag = ConsumerTag}, State) ->
    io:format("AMQP Consumer, rec'd cancel ok, tag= ~p~n", [ConsumerTag] ),
    {ok, State};

handle_info({#'basic.deliver'{},
              {content, _ClassId, _Properties, _PropertiesBin, Payload}},
              State) ->
     io:format("AMQP Consumer, rec'd: ~p~n", [ Payload ] ),
	%%% Make sure to take the first element from the payload list for forwarding
	 [P] = Payload,
	 % DPayload = parse(binary_to_list(P)),
	 Message = meson_core_util:meson_message_to_goal(binary_to_list(P)),
	 Destination = goal_to_destination( binary_to_list(Message#meson_message.goal)),
	 case Destination of
		{invalid_goal} -> 
			io:format("Invalid route.  Discarding messaging ~n");
		_ ->
			io:format("Routing to ~p~n", [Destination]),
	 		route_result(Destination, P)
		end,
	{ok, State}.

route_result({xmpp, Jids}, Payload) ->
	io:format("Routing to XMPP destination: JID: ~p Payload ~p", [Jids,Payload]),
	%TODO: This was a copy-paste from above.  Refactor to make it unnecessary
	Message = meson_core_util:meson_message_to_goal(binary_to_list(Payload)),
	NewPayload = meson_core_util:encode_payload_for_xmpp(Message, Jids),
	Pid = whereis(goalsender),
	Pid ! {send_to_amqp, <<"internal">>, <<"xmpp_outbound">>, <<"xmpp_outbound">>, <<"xmpp_outbound">>, NewPayload},
	ok;
route_result({amqp, Vhost, Q}, Payload) -> 
	io:format("Routing to AMQP destination: Vhost: ~p Queue: ~p Payload ~p", [Vhost,Q,Payload]),
	Pid = whereis(goalsender),
	Pid ! {send_to_amqp, Vhost, Q, <<"find-goal">>, <<"find-goal">>, Payload},
	ok.
     
terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
goal_to_destination(Input) ->
	io:format("Looking up input ~p~n", [Input]),
	try
		Goal = meson_core_util:goal_from_db("localhost", "meson_development", Input),
		meson_core_util:convert_goal_to_route(Goal)
	catch
		_:_ -> io:format("Invalid goal ~n"),
		{invalid_goal}
	end.