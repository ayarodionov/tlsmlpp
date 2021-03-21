%-----------------------------------------------------------------------------------------------
% TCP server
% https://erlang.org/doc/apps/ssl/ssl.pdf
%-----------------------------------------------------------------------------------------------

-module(tcp_server).
-behavior(gen_server).
-vsn(1.0).

%-----------------------------------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").
-include("record_macros.hrl").

%-----------------------------------------------------------------------------------------------
-export([ start_link/2, start_link/3, init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2, terminate/2]).
-export([get_status/1]).

%-----------------------------------------------------------------------------------------------
-define(DEBUG, true).

-ifdef(DEBUG).
-export([start/2, start/3]).
-endif.

%-----------------------------------------------------------------------------------------------
-record(s_t, {
  name                :: atom(), % process name
  addr       = false  :: false | {inet:ipv4(), pos_integer()},          
  socket     = false  :: false | pid(),
  lsocket    = false  :: false | pid(),
  tls_socket = false  :: false | pid(),
  credits    = []     :: [tuple()]
  }).
-type s_t() :: #s_t{}.

%-----------------------------------------------------------------------------------------------
% Interfaces
%-----------------------------------------------------------------------------------------------
-ifdef(DEBUG).

-spec start(atom(), {string(), pos_integer()}) -> {ok, pid()}.
% @doc Starts {@module}; for testing only
start(Name, Saddr) -> start(Name, Saddr,  util:default_credits()).

-spec start(atom(), {string(), pos_integer()}, [tuple()]) -> {ok, pid()}.
% @doc Starts {@module}; for testing only
start(Name, Saddr, Credits) ->
  gen_server:start({local, Name}, ?MODULE, {Name, Saddr, Credits}, []).

-endif.

%-----------------------------------------------------------------------------------------------
-spec start_link(atom(), {string(), pos_integer()}) -> {ok, pid()}.
% @doc Starts {@module} in plain mode
start_link(Name, Saddr) -> start_link(Name, Saddr, []).

-spec start_link(atom(), {string(), pos_integer()}, [tuple()]) -> {ok, pid()}.
% @doc Starts {@module} with TLS encription
start_link(Name, Saddr, Credits) ->
  gen_server:start_link({local, Name}, ?MODULE, {Name, Saddr, Credits}, []).

%-----------------------------------------------------------------------------------------------
-spec init({atom(), {string(), non_neg_integer()}, [tuple()]}) -> {ok, s_t()}.
% @doc Initializes {@module}  and opens <i>gen_tcp</i> socket
init({Name, {Addr, Port}, Credits})  -> 
  process_flag(trap_exit, true),

  {ok, Ip} = inet:parse_ipv4strict_address(Addr), 
  {ok, ListenSocket} = gen_tcp:listen(Port, [{reuseaddr, true}, {ip, Ip}]),
  self() ! accept, 
  ST = #s_t{
    name       = Name,
    addr       = {Ip, Port},
    lsocket    = ListenSocket,
    credits    = Credits
    },
  ?LOG_INFO(s_t(ST)),
  {ok, ST}.

%-----------------------------------------------------------------------------------------------
-spec get_status(atom()) -> map().
% @doc Gets and print state record. For debugging only.
% @param Name registered name 
% @return current state 
get_status(Name) ->
    gen_server:call(Name, get_status).

%-----------------------------------------------------------------------------------------------
%  Callbacks
%-----------------------------------------------------------------------------------------------
-spec handle_cast(term(), s_t()) -> term().
% @doc Whenever a <i>gen_server</i> process receives a request sent using <i>cast/2</i>, 
% this function is called to handle the request.
handle_cast(_Msg, ST) ->
  ?LOG_ERROR("~p: unknown message=~p~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST}.

%-----------------------------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, s_t()) -> s_t().
% @doc Whenever a <i>gen_server</i> process receives a request sent using <i>call/2,3</i> , 
% this function is called to handle the request.
handle_call(get_status, _From, ST) ->
    ?LOG_DEBUG(s_t(ST)),
    {reply, ST, ST};

handle_call(_Msg, _From, ST) ->
  ?LOG_ERROR("~p:unknown message=~p~n", [?FUNCTION_NAME, _Msg]),
  {reply, false, ST}.

%-----------------------------------------------------------------------------------------------
-spec handle_info(timeout | term(), s_t()) -> tuple().
%@doc  This function is called by a <i>gen_server</i> process when a time-out occurs or 
% when it receives any other message than a synchronous or asynchronous request 
% (or a system message).
handle_info(_Msg=accept, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  Self = self(),
  spawn(fun() -> 
    {ok, Socket} = gen_tcp:accept(ST#s_t.lsocket),
    gen_tcp:controlling_process(Socket, Self),
    Self ! {accept, Socket}
    end),
  {noreply, ST};

handle_info(_Msg={accept, Socket}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  case ST#s_t.credits of 
    [] -> inet:setopts(Socket, [{active, true}, binary]);
    _  -> self() ! handshake
  end,
  {noreply, ST#s_t{socket = Socket}};

handle_info(_Msg=handshake, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  ok = inet:setopts(ST#s_t.socket, [{active, false}, binary]),
  Self = self(),
  Pid  = spawn(fun() -> 
      receive
        {go, Self} ->
          {ok, TLSSocket} = ssl:handshake(ST#s_t.socket, ST#s_t.credits),
          gen_tcp:controlling_process(ST#s_t.socket, Self),
          ssl:controlling_process(TLSSocket, Self),
          Self ! {handshake, TLSSocket}
      end
    end),
  gen_tcp:controlling_process(ST#s_t.socket, Pid),
  Pid ! {go, Self},
  {noreply, ST};

handle_info(_Msg={handshake, TLSSocket}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  ok = ssl:setopts(TLSSocket, [{active, true}]),
  {noreply, ST#s_t{tls_socket = TLSSocket}};

handle_info(_Msg={ssl, _, _Data}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST};

handle_info(_Msg={tcp, _Port, _Data}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST};

handle_info(_Msg={tcp_closed, _Port}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST};

handle_info(_Msg, ST) ->
  ?LOG_ERROR("~p: unknown message=~p~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST}.

%-----------------------------------------------------------------------------------------------
-spec terminate(normal | shutdown | {shutdown, term()} | term(), s_t()) -> terminated.
% @doc This function is called by a <i>gen_server</i> process when it is about to terminate.
% Closes socket.
terminate(_Msg, ST) ->
  ?LOG_INFO("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  ST#s_t.tls_socket andalso ssl:close(ST#s_t.tls_socket),
  ST#s_t.socket andalso gen_tcp:close(ST#s_t.socket),
  ST#s_t.lsocket andalso gen_tcp:close(ST#s_t.lsocket),
  terminated.

%-----------------------------------------------------------------------------------------------
%  Private
%-----------------------------------------------------------------------------------------------
?RECORD_TF_MAP(s_t).

%-----------------------------------------------------------------------------------------------
% tcp_server:start(ss, {"127.0.0.1", 9999}).
% tcp_server:get_status(ss).

%-----------------------------------------------------------------------------------------------
% tcp_server:start(ss, {"127.0.0.1", 9999}, []).
% tcp_server:get_status(ss).

%-----------------------------------------------------------------------------------------------
