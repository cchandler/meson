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
%% @doc The goal server utility module
%% 
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/cchandler/meson/

-module(meson_core_util).

-export([goal_from_db/3, convert_goal_to_route/1, meson_message_to_goal/1, encode_jids/2, encode_payload_for_xmpp/2]).

-include("../include/meson_core_records.hrl").

goal_from_db(Host, Database, Goal) ->
	A = erlang_couchdb:retrieve_document({Host,5984}, Database, Goal),
	Input = #goal{},
	parse_goal(A, Input).
	
convert_goal_to_route(Goal = #goal{protocol= <<"xmpp">>} ) ->
	{xmpp, Goal#goal.jid};
convert_goal_to_route(Goal = #goal{protocol= <<"amqp">>} ) ->
	{amqp, Goal#goal.vhost, Goal#goal.queue};
convert_goal_to_route(_) ->
	{invalid_goal}.
%%% Some kind of recursive descent parser to convert the
%%% JSON record into a usable Erlang record.

parse_goal({json, A}, Input) ->
	parse_goal(A, Input);
parse_goal({struct, A}, Input) ->
	% io:format("Struct ~p ~n", [A]),
	parse_goal(A, Input);
parse_goal({A, B}, Input) when is_binary(A) and is_tuple(B)->
	% io:format("Pair w/ tuple ~p ~p~n", [A,B]),
	case A of
		<<"amqp">> ->
			Intermediate = Input#goal{protocol=A};
		<<"xmpp">> ->
			Intermediate = Input#goal{protocol=A};
		_ ->
			Intermediate = Input
	end,
	parse_goal(B, Intermediate);
%Single key pairs
parse_goal({C,D}, Input) when is_binary(C) and is_binary(D) ->
	% io:format("Binary keypair ~p ~p ~n", [C,D]),
	case C of 
		<<"_id">> ->
			% io:format("Found the ID: ~p~n", [D]),
			Input#goal{id=D};
		<<"_rev">> ->
			% io:format("Found the rev: ~p~n", [D]);
			Input#goal{rev=D};
		<<"queue">> ->
			% io:format("Found the queue: ~p~n", [D]);
			Input#goal{queue=D};
		<<"vhost">> ->
			% io:format("Found vhost: ~p~n", [D]);
			Input#goal{vhost=D};
		<<"jid">> ->
			Input#goal{jid=D};
		_ ->
			Input
	end;
parse_goal({C,D}, Input) when is_binary(C) and is_list(D) ->
	case C of
		<<"jid">> ->
			Input#goal{jid=[D]}
		end;
%Empty list
parse_goal([], Input) ->
	Input;
	% io:format("Empty~n");
%Main body of response
parse_goal([H|T], Input) ->
	% io:format("Element ~p~n", [H]),
	Intermediate = parse_goal(H, Input),
	parse_goal(T, Intermediate);
%Handle no match
parse_goal(A, Input) ->
	io:format("No match ~p ~n", [A]),
	Input.
	
	
%%%
% ("{\"message\":\"Response from server\",\"goal\":\"f944460a582be8287e72ae9c85e7d5ad\"}"

meson_message_to_goal(A) ->
	S = mochijson2:decode(A),
	Input = #meson_message{},
	parse_message(S, Input).

parse_message({json, A}, Input) ->
		parse_message(A, Input);	
parse_message({struct, A}, Input) ->
	parse_message(A, Input);
parse_message({C,D}, Input) when is_binary(C) and is_binary(D) ->
	case C of 
		<<"message">> ->
			Input#meson_message{body=D};
		<<"goal">> ->
			Input#meson_message{goal=D};
		_ ->
			%Ignore unknowns
			Input
	end;
parse_message([H|T], Input) ->
	Intermediate = parse_message(H, Input),
	parse_message(T, Intermediate);
parse_message([], Input) ->
	Input.
	
%%Take the original payload and encode in the new JIDs for destinations

encode_payload_for_xmpp(Message = #meson_message{}, Jids) ->
	list_to_binary(mochijson2:encode(
		{struct, [{message, Message#meson_message.body}, {goal, Message#meson_message.goal },
		{jidlist, encode_jids(Jids, []) }] }
	)).

encode_jids([H|T], Input) ->
	[Username,Domain,Resource] = H,
	Intermediate = Input ++ [encode_jid(Username,Domain,Resource)],
	encode_jids(T, Intermediate);
encode_jids([], Input) ->
	Input.
	
encode_jid(Username, Domain, Resource) ->
	{struct, [  {username, Username }, {domain, Domain}, {resource, Resource}   ]}.
	% {struct, [  {username, list_to_binary(Username) }, {domain, list_to_binary(Domain)}, {resource, list_to_binary(Resource)}   ]}.