-module(acceptor).
-export([start/2]).
-define(delay, 20).
-define(drop, 10).

start(Name, PanelId) ->
	spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	Promised = order:null(),
	Voted = order:null(),
	Value = na,
	acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
	receive
		{prepare, Proposer, Round} ->
      %R = random:uniform(?delay),
      %io:format("[Acceptor ~w] delaying prepare msg~n", [Name]),
      %timer:sleep(R),
			case order:gr(Round, Promised) of
				true ->
          case random:uniform(?drop) of
            ?drop ->
              io:format("----------------------[Acceptor ~w] Phase 1: PROMISE message dropped in round ~w~n", [Name, Round]);
            _ ->
              Proposer ! {promise, Round, Voted, Value, Name}
          end,
%% 					Proposer ! {promise, Round, Voted, Value, Name},
					% Update gui
					if
						Value == na ->
							Colour = {0,0,0};
						true ->
							Colour = Value
					end,
					io:format("[Acceptor ~w] Phase 1: voted ~w promised ~w colour ~w~n", [Name, Voted, Round, Value]),
					PanelId ! {updateAcc, "Voted: " ++ lists:flatten(io_lib:format("~p", [Voted])), "Promised: " ++ lists:flatten(io_lib:format("~p", [Round])), Colour},
					acceptor(Name, Round, Voted, Value, PanelId);
				false ->
          Proposer ! {sorry, {prepare, Round}, Name},
					acceptor(Name, Promised, Voted, Value, PanelId)
			end;
		{accept, Proposer, Round, Proposal} ->
      %R = random:uniform(?delay),
      %io:format("[Acceptor ~w] delaying accept msg~n", [Name]),
      %timer:sleep(R),
			case order:goe(Round, Promised) of
				true ->
          case random:uniform(?drop) of
            ?drop ->
              io:format("----------------------[Acceptor ~w] Phase 1: VOTE message dropped in round ~w~n", [Name, Round]);
            _ ->
              Proposer ! {vote, Round, Name}
          end,
%% 					Proposer ! {vote, Round, Name},
					case order:goe(Round, Voted) of
						true ->
              io:format("[Acceptor ~w] Phase 2: round ~w goe than voted ~w~n", [Name, Round, Voted]),
							% Update gui
							io:format("[Acceptor ~w] Phase 2: voted ~w promised ~w colour ~w~n", [Name, Round, Promised, Proposal]),
							PanelId ! {updateAcc, "Voted: " ++ lists:flatten(io_lib:format("~p", [Round])), "Promised: " ++ lists:flatten(io_lib:format("~p", [Promised])), Proposal},
							acceptor(Name, Promised, Round, Proposal, PanelId);
						false ->
              io:format("[Acceptor ~w] Phase 2: round ~w NOT goe than voted ~w~n", [Name, Round, Voted]),
							% Update gui
							io:format("[Acceptor ~w] Phase 2: voted ~w promised ~w colour ~w~n", [Name, Voted, Promised, Value]),
							PanelId ! {updateAcc, "Voted: " ++ lists:flatten(io_lib:format("~p", [Voted])), "Promised: " ++ lists:flatten(io_lib:format("~p", [Promised])), Value},
							acceptor(Name, Promised, Voted, Value, PanelId)
					end;
				false ->
          Proposer ! {sorry, {accept, Round}, Name},
					acceptor(Name, Promised, Voted, Value, PanelId)
			end;
		stop ->
			ok
	end.