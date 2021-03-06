-module(paxy).
-export([start/1, stop/0, stop/1]).
-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
	AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3", "Acceptor 4", "Acceptor 5"],
	AccRegister = [a, b, c, d, e],
	ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3"],
	PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
	% computing panel heights
	AccPanelHeight = length(AcceptorNames)*50 + 0, %plus the spacer value
	PropPanelHeight = length(ProposerNames)*50 + 0,
	register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames,AccPanelHeight, PropPanelHeight) end)),
	gui ! {reqState, self()},
	receive
		{reqState, State} ->
			{AccIds, PropIds} = State,
			start_acceptors(AccIds, AccRegister),
			start_proposers(PropIds, PropInfo, AccRegister, Sleep)
	end,
	true.
	
start_acceptors(AccIds, AccReg) ->
	case AccIds of
		[] ->
			ok;
		[AccId|Rest] ->
			[RegName|RegNameRest] = AccReg,
			register(RegName, acceptor:start(RegName, AccId)),
			start_acceptors(Rest, RegNameRest)
	end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
	case PropIds of
		[] ->
			ok;
		[PropId|Rest] ->
			[{RegName, Colour}|RestInfo] = PropInfo,
			[FirstSleep|RestSleep] = Sleep,
			proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId),
			start_proposers(Rest, RestInfo, Acceptors, RestSleep)
		end.

stop() ->
	stop(gui),
	stop(a),
	stop(b),
	stop(c),
	stop(d),
	stop(e).

stop(Name) ->
	case whereis(Name) of
		undefined ->
			ok;
		Pid ->
			Pid ! stop
	end.