-module(proposer).
-export([start/5]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId) ->
	spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId) end).

init(Name, Proposal, Acceptors, Sleep, PanelId) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	timer:sleep(Sleep),
	Round = order:null(Name),
	round(Name, ?backoff, Round, Proposal, Acceptors, PanelId).

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
	% Update gui
	io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", [Name, Round, Proposal]),
	PanelId ! {updateProp, "Round: " ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: " ++ lists:flatten(io_lib:format("~p", [Proposal])), Proposal},
	case ballot(Name, Round, Proposal, Acceptors, PanelId) of
		{ok, Decision} ->
			io:format("[Proposer ~w] ~w decided ~w in round ~w~n", [Name, Acceptors, Decision, Round]),
			{ok, Decision};
		abort ->
			timer:sleep(random:uniform(Backoff)),
			Next = order:inc(Round),
			round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
	end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
	prepare(Round, Acceptors),
	Quorum = (length(Acceptors) div 2) + 1,
	Max = order:null(),
	case collect(Quorum, Round, Max, Proposal) of
		{accepted, Value} ->
			% update gui
			io:format("[Proposer ~w] Phase 2: round ~w proposal ~w~n", [Name, Round, Value]),
			PanelId ! {updateProp, "Round: " ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: " ++ lists:flatten(io_lib:format("~p", [Value])), Value},
			accept(Round, Value, Acceptors),
			case vote(Quorum, Round) of
				ok ->
					{ok, Value};
				abort ->
					abort
			end;
		abort ->
			abort
	end.

collect(0, _, _, Proposal) ->
	{accepted, Proposal};
collect(N, Round, MaxVoted, Proposal) ->
	receive
		{promise, Round, _, na} ->
			collect(N-1, Round, MaxVoted, Proposal);
		{promise, Round, Voted, Value} ->
			case order:gr(Voted, MaxVoted) of
				true ->
					collect(N, Round, Voted, Value);
				false ->
					collect(N-1, Round, MaxVoted, Proposal)
			end;
		{promise, _, _, _} ->
			collect(N, Round, MaxVoted, Proposal);
		{sorry, {prepare, Round}} ->
			collect(N, Round, MaxVoted, Proposal);
		{sorry, _} ->
			collect(N, Round, MaxVoted, Proposal)
	after ?timeout ->
			abort
	end.

vote(0, _) ->
	ok;
vote(N, Round) ->
	receive
		{vote, Round} ->
			vote(N-1, Round);
		{vote, _} ->
			vote(N, Round);
		{sorry, {accept, Round}} ->
			vote(N, Round);
		{sorry, _} ->
			vote(N, Round)
	after ?timeout ->
			abort
	end.

prepare(Round, Acceptors) ->
	Fun = fun(Acceptor) ->
		send(Acceptor, {prepare, self(), Round})
	end,
	lists:map(Fun, Acceptors).
accept(Round, Proposal, Acceptors) ->
	Fun = fun(Acceptor) ->
		send(Acceptor, {accept, self(), Round, Proposal})
	end,
	lists:map(Fun, Acceptors).

send(Name, Message) ->
	Name ! Message.