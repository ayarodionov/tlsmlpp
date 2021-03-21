%-----------------------------------------------------------------------------------------------
% @doc tlsmlpp top level supervisor.
% @end
%-----------------------------------------------------------------------------------------------

-module(tlsmlpp_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%-----------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([init/1, get_status/0, which_children/0, count_children/0]).

%-----------------------------------------------------------------------------------------------
-define(TEST, true).

-ifdef(TEST).
-export([mk_name/2, mk_child/2]). 
-endif.

-define(SERVER, ?MODULE).

%-----------------------------------------------------------------------------------------------
% @doc Starts {@module}
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

%-----------------------------------------------------------------------------------------------
-spec init([]) -> {ok, tuple()}.
% @doc Initializes {@module} and starts children.
% Reads parameters from <i>sys.config</i> file. 
% Parameters consist of tuples:
% {tcp_server or tcp_client, [list of ]{ipv4, port}, sertificate}]}
% 
% For exaple:
% <pre>
  % {tlsmlpp, [
  %   {tcp_server, [
  %     {{"127.0.0.1", 9999}, []},
  %     {{"127.0.0.1", 9998}, default}
  %   ]},
  %   {tcp_client, [
  %     {{"127.0.0.1", 9999}, []},
  %     {{"127.0.0.1", 9998}, default}
  %   ]}
  % ]}
% </pre>
init([]) ->
  SupFlags = #{strategy => one_for_one,
                intensity => 1,
                period => 5},
  ?LOG_INFO(SupFlags),
  ChildSpecs = 
    [mk_child(tcp_server, SA) || SA <- application:get_env(tlsmlpp, tcp_server, [])] 
    ++
    [mk_child(tcp_client, SA) || SA <- application:get_env(tlsmlpp, tcp_client, [])],
  ?LOG_INFO("~p: ChildSpecs=~p~n", [?FUNCTION_NAME, ChildSpecs]),
  {ok, {SupFlags, ChildSpecs}}.

%-----------------------------------------------------------------------------------------------
-spec get_status() -> tuple().
% @doc Retunrs information about {@module} status.
% For debugging only
% For now it is <i>supervisor:which_children(?MODULE)</i>
% Later some more information can be added
get_status() -> supervisor:which_children(?MODULE).

-spec which_children() -> tuple().
% @doc Shortcut for supervisor:which_children(?MODULE).
% Useful for debugging
which_children() -> supervisor:which_children(?MODULE).

-spec count_children() -> tuple().
% @doc Shortcut for supervisor:count_children(?MODULE).
% Useful for debugging
count_children() -> supervisor:count_children(?MODULE).

%-----------------------------------------------------------------------------------------------
%  Private
%-----------------------------------------------------------------------------------------------
-spec mk_name(atom(), {string(), integer()}) -> atom().
% @doc Creates process name.
% 
% Example:
% 'tcp_client_1.1.1.1:5678' = tlsmlpp_sup:mk_name(tcp_client, {"1.1.1.1", 5678}).
mk_name(Type, {Ip, Port}) ->
    list_to_atom(atom_to_list(Type) ++ "_" ++ Ip ++ ":" ++ integer_to_list(Port)).

%-----------------------------------------------------------------------------------------------
-spec mk_child(tcp_client | tcp_server, {{string(), pos_integer()}, [tuple()] | default}) -> tuple().
% @doc Creates child process description
mk_child(Module, {Addr, default}) ->  mk_child(Module, {Addr, util:default_credits()});
mk_child(Module, {Addr, Credits}) -> 
	Name = mk_name(Module, Addr),
	{
		Name,                   % child id
		{
			Module,               % module name
			start_link,           % start function
			[Name, Addr, Credits] % start function arguments
		}, 
		permanent,              % restart type
		10,                     % wait for an exit signal for this time
		worker,                 % process type
		[Module]                % modules
	}.

%-----------------------------------------------------------------------------------------------
