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
	case collect(Quorum, Round, Max, Proposal, Name, []) of
		{accepted, Value, AgreedPromiseAcceptors} ->
			% update gui
      io:format("[Proposer ~w] ~w acceptors promised ~w in round ~w~n", [Name, AgreedPromiseAcceptors, Value, Round]),
			io:format("[Proposer ~w] Phase 2: round ~w proposal ~w~n", [Name, Round, Value]),
			PanelId ! {updateProp, "Round: " ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: " ++ lists:flatten(io_lib:format("~p", [Value])), Value},
			accept(Round, Value, Acceptors),
			case vote(Quorum, Round, Name, []) of
        {ok, AgreedVoteAcceptors} ->
          io:format("[Proposer ~w] ~w acceptors voted ~w in round ~w~n", [Name, AgreedVoteAcceptors, Value, Round]),
					{ok, Value};
				abort ->
					abort
			end;
		abort ->
			abort
	end.

collect(0, _, _, Proposal, _, Acceptors) ->
	{accepted, Proposal, Acceptors};

collect(N, Round, MaxVoted, Proposal, Name, Acceptors) ->
	receive
		{promise, Round, _, na, Acceptor} ->
			collect(N-1, Round, MaxVoted, Proposal, Name, Acceptors ++ [Acceptor]);
		{promise, Round, Voted, Value, Acceptor} ->
			case order:gr(Voted, MaxVoted) of
				true ->
%%           io:format("[Proposer ~w] voted ~w gr than maxvoted ~w, update with value: ~w rather than proposal ~w~n", [Name, Voted, MaxVoted, Value, Proposal]),
					collect(N, Round, Voted, Value, Name, Acceptors);
				false ->
%%           io:format("[Proposer ~w] voted ~w NOOOOOT gr than maxvoted ~w, update with value: ~w rather than proposal ~w~n", [Name, Voted, MaxVoted, Proposal, Value]),
					collect(N-1, Round, MaxVoted, Proposal, Name, Acceptors ++ [Acceptor])
			end;
		{promise, _, _, _, _} ->
      io:format("[Proposer ~w] Collect: round ~w received OLD PROMISE~n", [Name, Round]),
			collect(N, Round, MaxVoted, Proposal, Name, Acceptors);
		{sorry, {prepare, Round}, Acceptor} ->
      io:format("[Proposer ~w] Collect: round ~w received sorry PREPARE from ~w~n", [Name, Round, Acceptor]),
			collect(N, Round, MaxVoted, Proposal, Name, Acceptors);
		{sorry, _, _} ->
      io:format("[Proposer ~w] Collect: round ~w received OLD sorry PREPARE~n", [Name, Round]),
			collect(N, Round, MaxVoted, Proposal, Name, Acceptors)
	after ?timeout ->
      io:format("[Proposer ~w] aborting collection of PROMISES in round ~w~n", [Name, Round]),
			abort
	end.

vote(0, _, _, Acceptors) ->
  {ok, Acceptors};
vote(N, Round, Name, Acceptors) ->
	receive
		{vote, Round, Acceptor} ->
			vote(N-1, Round, Name, Acceptors ++ [Acceptor]);
		{vote, _} ->
      io:format("[Proposer ~w] Vote: round ~w received OLD VOTE~n", [Name, Round]),
			vote(N, Round, Name, Acceptors);
		{sorry, {accept, Round}, Acceptor} ->
      io:format("[Proposer ~w] Vote: round ~w received sorry ACCEPT from ~w~n", [Name, Round, Acceptor]),
			vote(N, Round, Name, Acceptors);
		{sorry, _} ->
      io:format("[Proposer ~w] Vote: round ~w received OLD sorry ACCEPT~n", [Name, Round]),
			vote(N, Round, Name, Acceptors)
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

%% send(Name, Message) ->
%% 	Name ! Message.

send(Name, Message) ->
  case whereis(Name) of
    undefined ->
      down;
    Pid ->
      Pid ! Message
  end.