%% Author: Uri
%% Created: 12/05/2011
%% Description: TODO: Add description to overlay_server
-module(overlay_server).
-behaviour(gen_server).
-ver(1.0).

-define (CLIENTS,clients).


%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([stop/1, get_random/2, get_live/1, start_server/1,switch_version/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%
%% API Functions
%%

%% start_server spawns a robust server and registers it under ServerName.
%% It returns the pid of the server.
start_server(ServerName) when is_atom(ServerName)->
	%%io:format("In start server. Server = ~w\n",[ServerName]),
	{ok, _ServerPid} = gen_server:start_link({local, ServerName}, ?MODULE, [], []).
	%%io:format("In start server. Server Pid= ~w\n",[ServerPid]),
	%%ServerPid.

%% Stops the server, returns ok if the operation is succesful, otherwise {error, Reason}.
%%TODO:check the result of shutdown
stop(ServerName) -> gen_server:cast(ServerName, {shutdown}).

%% Query server name for list of all live clients.
%% Returns a list of all live clients.
get_live(ServerName) when is_atom(ServerName) ->
	{Status,Result} = gen_server:call(ServerName,{getlive,self()}),
	case Status of
		ok -> {ok,Result}; %%Result is the ClientList
		_-> {error,Result} %%Result is the reason
	end.

%% Query ServerName for random active node's pid different from From.
%% Returns {ok,Pid} of a random network node or {error,Reason}. 
get_random(From,ServerName)when is_atom(ServerName),is_pid(From)->
	MsgRef = make_ref(),
	{Status,MsgRef,Result} = gen_server:call(ServerName,{getrandom,MsgRef,From}),
	case {Status,Result} of
		{randomClient,nil} -> {error, noclients};
		{randomClient,Pid} -> {ok, Pid};
		{Status,_} -> {error, Status}
	end.

%% Switch version downgrades overlay_server to use orddict dictionary.
%% Returns the upgraded state
switch_version(ServerName)->
	sys:suspend(ServerName),
	sys:change_code(ServerName, ServerName, [1.0], []),
	sys:resume(ServerName).

%%
%% gen_server callbacks
%%

init([]) -> 
	%%io:format("In start server 1.0 init. \n"),
	{ok, orddict:new()}.
 

handle_call({subscribe,Ref,_SenderPid,ClientPid, ClientName},_From,State) ->
	ClientRef = monitor(process,ClientPid),
	State1 = orddict:filter(fun (_Key,{_ClientPid,ClientName1})-> ClientName1 =/= ClientName end, State),
	NewState = orddict:store(ClientRef, {ClientPid,ClientName}, State1),
    Reply = {registered, Ref},
    {reply, Reply, NewState};

handle_call({getlive,_Pid},_From,State) ->
	%%io:format("In getlive. \n"),
    LiveClients = [ClientName || {_Key,{_Pid1,ClientName}}<-orddict:to_list(State)],
	Reply = {ok,LiveClients},
	{reply,Reply,State};

handle_call({getrandom, MsgRef, SenderPid},_From,State)->
	Reply = case getRandom(State, SenderPid) of
				none -> {randomClient, MsgRef, nil};
				Pid -> {randomClient, MsgRef, Pid}
			end,
	{reply,Reply,State}.

handle_cast({shutdown},_State)->
	%%io:format("In shutdown. \n"),
	exit(normal);

handle_cast(Msg, State) -> 
	io:format("In handle cast. Msg is ~w, State is ~w \n",[Msg,State]),
	{noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason},State)->
	io:format("In DOWN. \n"),
	{noreply,orddict:erase(Ref,State)};

handle_info(Info, State) ->
	io:format("In handle info. info is ~w, State is ~w \n",[Info,State]),
	{noreply, State}.
terminate(_Reason, _State) -> 
	%%io:format("server is shutting down\n"),
	ok.

code_change(OldVsn, State, _Extra) ->
	%%io:format("In code change: Old=~w, Extra=~w\n",[OldVsn, Extra]),
	CurrentVersion = hd(OldVsn),
	case CurrentVersion of
%%		2.0-> {ok, buildEtsFromDict(State)};
		1.0-> NewState = buildDictFromEts(State),
			  {ok, NewState};
		_->{error,badversion}
	end.

%%
%% Local Functions
%%

%% buildEtsFromDict(State)->
%% 	Clients=orddict:to_list(State),
%% 	case ets:info(?CLIENTS) of
%% 		undefined-> ets:new(?CLIENTS, [public, named_table]),
%% 					io:format("Registering client table by ~w\n",[self()]);
%% 		_->ok
%% 	end,
%% 	ets:insert(?CLIENTS, Clients).


%% buildDictFromEts builds orddict based state from ets based state
%% Returns the new state
buildDictFromEts(_State)->
	Clients=ets:tab2list(?CLIENTS),
	%%io:format("buildDictFromEts: State=~w\n", [Clients]),
	NewState = buildDict(orddict:new(),Clients),
	%%io:format("buildDictFromEts, after conversion: State=~w\n", [NewState]),
	NewState.

buildDict(State, [])->State;
buildDict(State,[{Ref, Pid, Name}|T])->buildDict(orddict:store(Ref, {Pid, Name}, State),T).

%% getRandom returns a random client registered to the server.
%% Returns: The pid of a random client.
getRandom(State, SenderPid) ->
	Len = orddict:size(State),
	case Len of
		0 -> none;
		1 -> [{_Ref, {Pid, _Name}}] = orddict:to_list(State),
			  case Pid of
					SenderPid -> none;
				    _ -> Pid
			  end;
		_ ->  {_Ref, {Pid, _Name}} = lists:nth(random:uniform(Len), orddict:to_list(State)),
			  case Pid of
					SenderPid -> getRandom(State, SenderPid);
				    _ -> Pid
			  end
	end.


    

