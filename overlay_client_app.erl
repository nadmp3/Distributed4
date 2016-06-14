%% Author: Uri
%% Created: 18/05/2011
%% Description: TODO: Add description to overlay_client_app
-module(overlay_client_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Parameters:
%%		Type: Start type
%%		StartArgs: A list of {server,[clients]}
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, StartArgs) when is_list(StartArgs), length(StartArgs)>0->
	Clients = start_clientsOnServer(StartArgs),
	{ok, hd(hd(Clients))}.
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Start clients on servers
start_clientsOnServer(StartArgs)->
	[start_clients(ServerName,ClientsNameList) || {ServerName,ClientsNameList}<-StartArgs].

%% Starts a list of clients and register them on the given server
start_clients(ServerName,ClientsNameList)->
	[overlay_client:start_client(ServerName, Name) || Name<-ClientsNameList].

