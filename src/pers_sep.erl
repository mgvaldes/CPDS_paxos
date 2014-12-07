-module(pers_sep).
-export([read/1, store/5, delete/1]).

%% dets module provides term storage on file

%% returns the object with the key ’perm’ stored in the table ’Name’
read(Name) ->
	{ok, Name} = dets:open_file(Name, []),
	case dets:lookup(Name, perm) of
		[{perm, Pr, Vt, Vl, Pn}] ->
			{Pr, Vt, Vl, Pn};
		[] ->
			{order_sep:null(), order_sep:null(), na, na}
	end.

%% inserts one object {Pr, Vt, Vl, Pn} into the table ’Name’
store(Name, Pr, Vt, Vl, Pn)->
	dets:insert(Name, {perm, Pr, Vt, Vl, Pn}).

delete(Name) ->
	dets:delete(Name, perm),
	dets:close(Name).