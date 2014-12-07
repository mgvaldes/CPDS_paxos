-module(paxy_sep).
%% -export([start/1, stop/0, stop/1]).
-export([start_sep_proposers/1, start_sep_acceptors/1, stop/0, stop/1]).
-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(YELLOW, {255,255,0}).
-define(PURPLE, {102,0,153}).
-define(PINK, {255,0,128}).

% Sleep is a list with the initial sleep time for each proposer
%% start(Sleep) ->
%% 	AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3", "Acceptor 4", "Acceptor 5", "Acceptor 6", "Acceptor 7", "Acceptor 8", "Acceptor 9", "Acceptor 10"],
%% 	AccRegister = [a, b, c, d, e, f, g, h, i, j],
%% 	ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3", "Proposer 4", "Proposer 5", "Proposer 6"],
%% 	PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}, {kevin, ?YELLOW}, {max, ?PURPLE}, {jeffry, ?PINK}],
%% 	% computing panel heights
%% 	AccPanelHeight = length(AcceptorNames)*50 + 0, %plus the spacer value
%% 	PropPanelHeight = length(ProposerNames)*50 + 0,
%% 	register(gui_acceptors, spawn(fun() -> gui:start_acceptors(AcceptorNames, AccPanelHeight) end)),
%%   register(gui_proposers, spawn(fun() -> gui:start_proposers(ProposerNames, PropPanelHeight) end)),
%%   gui_acceptors ! {reqStateAccep, self()},
%%   gui_proposers ! {reqStateProp, self()},
%%
%% 	receive
%% 		{reqStateAccep, State} ->
%% 			{AccIds} = State,
%% 			start_acceptors(AccIds, AccRegister);
%%     {reqStateProp, State} ->
%%       {PropIds} = State,
%%       start_proposers(PropIds, PropInfo, AccRegister, Sleep)
%% 	end.

start_sep_proposers(Sleep) ->
  register(proposers, spawn(start_sep_proposers2(Sleep))).

start_sep_proposers2(Sleep) ->
  ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3", "Proposer 4", "Proposer 5", "Proposer 6"],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}, {kevin, ?YELLOW}, {max, ?PURPLE}, {jeffry, ?PINK}],
  % computing panel heights
  PropPanelHeight = length(ProposerNames)*50 + 0,
  register(gui_proposers, spawn(fun() -> gui_sep:start_proposers(ProposerNames, PropPanelHeight, proposers@mgvaldes) end)),
  {gui_proposers, proposers@mgvaldes} ! {reqStateProp},

  receive
    {reqStateProp, State} ->
      io:format("----------------------HOLAAAAAAAAAA reqStateProp~n", []),
      receive
        {accReg, AccRegister} ->
          {PropIds} = State,
          io:format("----------------------YA RECIBI LOS REGISTER~n", []),
          start_proposers(PropIds, PropInfo, AccRegister, Sleep)
      end
  end.

start_sep_acceptors(PropNode) ->
  AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3", "Acceptor 4", "Acceptor 5", "Acceptor 6", "Acceptor 7", "Acceptor 8", "Acceptor 9", "Acceptor 10"],
  AccRegister = [a, b, c, d, e, f, g, h, i, j],
  {proposers, PropNode} ! {accReg, AccRegister},

  % computing panel heights
  AccPanelHeight = length(AcceptorNames)*50 + 0, %plus the spacer value
  register(gui_acceptors, spawn(fun() -> gui_sep:start_acceptors(AcceptorNames, AccPanelHeight) end)),
  gui_acceptors ! {reqStateAccep, self()},

  receive
    {reqStateAccep, State} ->
      {AccIds} = State,
      start_acceptors(AccIds, AccRegister, PropNode)
  end.
	
start_acceptors(AccIds, AccReg, PropNode) ->
	case AccIds of
		[] ->
			ok;
		[AccId|Rest] ->
			[RegName|RegNameRest] = AccReg,
			register(RegName, acceptor_sep:start(RegName, AccId, PropNode)),
			start_acceptors(Rest, RegNameRest, PropNode)
	end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
	case PropIds of
		[] ->
			ok;
		[PropId|Rest] ->
			[{RegName, Colour}|RestInfo] = PropInfo,
			[FirstSleep|RestSleep] = Sleep,
			proposer_sep:start(RegName, Colour, Acceptors, FirstSleep, PropId),
			start_proposers(Rest, RestInfo, Acceptors, RestSleep)
		end.

stop() ->
  stop(gui_acceptors),
	stop(gui_proposers),
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