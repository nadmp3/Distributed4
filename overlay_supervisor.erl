%% Author: Uri
%% Created: 12/05/2011
%% Description: TODO: Add description to overlay_supervisor
-module(overlay_supervisor).
-behaviour(supervisor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/1]).
-export([init/1]).
%%
%% API Functions
%%

start_link(ServerList) when is_list(ServerList), length(ServerList) > 0 ->
	supervisor:start_link({local,?MODULE}, ?MODULE, ServerList).

init(ServerList) ->
	%%io:format("In sup init\n"),
	Servers = [{ServerName, {overlay_server, start_server, [ServerName]},
            permanent,2000,worker,[overlay_server]} || ServerName<-ServerList],
  {ok,{{one_for_one,5,10}, Servers}}.


%%
%% Local Functions
%%

