%% Author: Uri
%% Created: 12/05/2011
%% Description: TODO: Add description to overlay_client
-module(overlay_client).
-behaviour(gen_server).

-record(state,{serverRef , serverName, clientName,tableId,lookupRefs=[],neighbors=[],isregistered=true,regmessageref=1}).
-define (NUMOFNEIGHBORS, 3).
-define (LOOKUPTIMEOUT,2000).
-define (TIMEOUT, 2000).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([lookup/3, get_peers/1, client/2, start_client/2,add_file/3,get_file/3,find_file/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%
%% API Functions
%%

start_client(ServerName,Name) when is_atom(ServerName),is_list(Name)->
	{ok,ClientPid} = gen_server:start(?MODULE, [ServerName,Name], []),
	MsgRef = make_ref(),
	{registered,MsgRef} = gen_server:call(ServerName,{subscribe,MsgRef,self(),ClientPid, Name}),
	gen_server:cast(ClientPid,{findneighbors}),
	ClientPid.

client(ServerName,Name) when is_atom(ServerName),is_list(Name)->
	ServerRef = monitor(process,whereis(ServerName)),
	TableId = ets:new(files, [private]),
	#state{serverRef=ServerRef , serverName=ServerName, clientName=Name, tableId = TableId}.


get_peers(ClientPid)when is_pid(ClientPid)->
	MsgRef= make_ref(),
	{Status,MsgRef,Pids} = gen_server:call(ClientPid,{getpeers,MsgRef,self()}),
	case Status of
		peers -> {ok,[Pids]};
		Reason-> {error,Reason}
	end.

lookup(ClientPid,NodeName,TTL) ->
	MsgRef = make_ref(),
	gen_server:cast(ClientPid,{look,MsgRef,{node,NodeName},TTL,self()}),
	receive
		{found,MsgRef,Pid, _Name} -> {ok,Pid}
		after ?LOOKUPTIMEOUT ->
			{error,notfound}
	end.

add_file(Pid, FileName, File) ->
	gen_server:call(Pid,{addfile,FileName,File}).

get_file(Pid, FileName, TTL)->
	{Status,Result} = find_file1(Pid,FileName, TTL),
	case Status of
		ok->{ClientName,ResultPid} = Result,
			gen_server:call(ResultPid, {getfile,ClientName,FileName});
		error-> {error,notfound}
	end.

find_file(Pid,FileName, TTL)->
	{Status,Result} = find_file1(Pid,FileName, TTL),
	case Status of
		ok->{Name,_ResultPid} = Result,
			{ok,Name};
		error-> {error,notfound}
	end.

find_file1(ClientPid,FileName, TTL)->
	MsgRef = make_ref(),
	gen_server:cast(ClientPid,{look,MsgRef,{file,FileName},TTL,self()}),
	receive
		{found,MsgRef,Pid,ClientName} -> {ok,{ClientName,Pid}}
		after ?LOOKUPTIMEOUT ->
			{error,notfound}
	end.

%%
%% gen_server callbacks
%%

init([ServerName,Name])->
	{ok,client(ServerName,Name)}.

handle_call({getfile,ClientName,FileName},_From,State)->
	MyName = State#state.clientName,
	case ClientName of
		MyName-> 
			Result = ets:lookup(State#state.tableId, FileName),
			if
				length(Result)==0->
					{reply,{error,notfound},State};
				true->
					{_Name,Data} = hd(Result),
					{reply,{ok,Data},State}
			end;
		_->{reply,{error,notme},State}
	end;

handle_call({getpeers,MsgRef,_SenderPid},_From,State)->
		Reply =  {peers,MsgRef,getPidList(State)},
		{reply,Reply,State};

handle_call({addfile,FileName,File},_From,State)->
	TableId = State#state.tableId,
	ets:insert(TableId, {FileName,File}),
	{reply,ok,State};

handle_call({removeLookUpRef,MsgRef},_From,State) ->
			{noreply,State#state{lookupRefs=lists:delete(MsgRef, State#state.lookupRefs)}};

handle_call({registered,_MsgRef},_From,State)->
			io:format("Server is alive again\n"),
			{noreply,State#state{isregistered=true, regmessageref=1}}.

handle_cast({checkregistration},State)->
			io:format("Check Registration message arrived\n"),
			if 
				State#state.isregistered->
					{noreply,State};
				true->
					receive
						after ?TIMEOUT-> ok
					end,
					MsgRef = make_ref(),
					gen_server:call(State#state.serverName,{subscribe,MsgRef,self(),self(), State#state.clientName}),
					gen_server:cast(self(),{checkregistration}),
					{noreply,State#state{isregistered=false, regmessageref=MsgRef}}
			end;


handle_cast({findneighbors},State)->
			{ReturnCode,Client} = overlay_server:get_random(self(), State#state.serverName),
			PidList = getPidList(State),
			InNeighbors = lists:member(Client,PidList),
			if 
				length(PidList)>=?NUMOFNEIGHBORS -> 
					{noreply,State};
				 ReturnCode==ok andalso not InNeighbors ->
					gen_server:cast(self(),{findneighbors}),
					NeighborRef = monitor(process,Client),
					{noreply,State#state{neighbors=lists:keystore(NeighborRef, 1, State#state.neighbors, {NeighborRef,Client})}};
				true-> gen_server:cast(self(),{findneighbors}),
					   {noreply,State}
			end;

handle_cast({look,MsgRef,LookRequest,TTL,SenderPid},State)->
		Found = found(LookRequest, State),
		case {Found,TTL} of
			{true,TTL} -> IsOld = lists:member(MsgRef, State#state.lookupRefs),
				if
					IsOld -> {noreply,State};
					true  -> SenderPid ! {found,MsgRef,self(), State#state.clientName},
 					  {noreply,addLookupRef(State,MsgRef,self())}
				end;
			{_,0}->{noreply,State};
			{_,TTL} -> IsOld = lists:member(MsgRef, State#state.lookupRefs),
				if
					IsOld -> {noreply,State};
					true  -> [gen_server:cast(Neighbor,{look,MsgRef,LookRequest,TTL-1,SenderPid}) || Neighbor<-getPidList(State)],
						 	{noreply,addLookupRef(State,MsgRef,self())}
 				end
		end;

handle_cast({shutdown},_State)->
	exit(normal);

handle_cast(Msg, State) ->
	io:format("invalid cast message ~w\n",[Msg]),
	{noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason},State) ->
	IsNeighbor = lists:member(Ref, getRefList(State)),
	if
		Ref == State#state.serverRef -> 
			io:format("Server is dead\n"),
			case Reason of
				normal->
					{serverdied, Pid, Reason};
				_-> 
					receive
						after ?TIMEOUT-> ok
					end,
					MsgRef = make_ref(),
					gen_server:call(State#state.serverName,{subscribe,MsgRef,self(),self(), State#state.clientName}),
					gen_server:cast(self(),{checkregistration}),
					{noreply,State#state{isregistered=false, regmessageref=MsgRef}}
			end;
		IsNeighbor-> 
			gen_server:cast(self(),{findneighbors}),
			{noreply,State#state{neighbors=lists:keydelete(Ref, 1, State#state.neighbors)}};
		true -> {noreply,State}
	end;

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason,State) -> 
	ets:delete(State#state.tableId),
	ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Local Functions
%%

found({node, Name}, State)->Name==State#state.clientName;
found({file, Name},State)->length(ets:lookup(State#state.tableId, Name))>0.

%% addLookupRef spawns a timer process which will notify the client to discard a referance of an old lookup
%% query.
addLookupRef(State,MsgRef,Pid)->
	spawn(fun()->receive 
					 after ?LOOKUPTIMEOUT->
						 gen_server:call(Pid,{removeLookUpRef,MsgRef})
				end
		   end),
	State#state{lookupRefs=[MsgRef|State#state.lookupRefs]}.

%% getPidList returns a list of neighbors pid.
getPidList(State)->
	lists:map(fun({_Ref,Pid})->Pid end, State#state.neighbors).

%% getRefList returns a list of monitored neighbors referances.
getRefList(State)->
	lists:map(fun({Ref,_Pid})->Ref end, State#state.neighbors).
