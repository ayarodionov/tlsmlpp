%-----------------------------------------------------------------------------------------------
% TCP client
% https://erlang.org/doc/apps/ssl/ssl.pdf
%-----------------------------------------------------------------------------------------------

-module(tcp_client).
-behavior(gen_server).
-vsn(1.0).

%-----------------------------------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").
-include("record_macros.hrl").

%-----------------------------------------------------------------------------------------------
-export([ start_link/2, start_link/3, init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2, terminate/2]).
-export([send/2]).
-export([get_status/1]).

%-----------------------------------------------------------------------------------------------
% -define(TEST, true).

-ifdef(TEST).
-export([start/2, start/3]).
-endif.

%-----------------------------------------------------------------------------------------------
-record(s_t, {
  name                :: atom(), % process name
  addr       = false  :: false | {inet:ipv4(), pos_integer()},          
  socket     = false  :: false | port(),
  tls_socket = false  :: false | port(),
  credits    = []     :: [tuple()]
  }).
-type s_t() :: #s_t{}.

%-----------------------------------------------------------------------------------------------
% Interfaces
%-----------------------------------------------------------------------------------------------
-ifdef(TEST).

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
init(_Msg = {Name, {Addr, Port}, Credits})  -> 
  process_flag(trap_exit, true),
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  {ok, Ip} = inet:parse_ipv4strict_address(Addr), 
  self() ! connect, 
  ST = #s_t{
    name       = Name,
    addr       = {Ip, Port},
    credits    = Credits
    },
  ?LOG_INFO(s_t(ST)),
  {ok, ST}.

%-----------------------------------------------------------------------------------------------
-spec send(atom(), string() | binary()) -> ok.
% @doc sends data over tcp or ssl socket.
% @param Name registered name 
% @param Data data to be send
% @return ok 
send(Name, Data) ->
    gen_server:cast(Name, {send, Data}).

%-----------------------------------------------------------------------------------------------
-spec get_status(atom()) -> s_t().
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
handle_cast(_Msg={send, Data}, ST) when ST#s_t.credits == [] andalso ST#s_t.socket /= false ->
  ?LOG_DEBUG("~p: gen_tcp:send: ~p~n", [?FUNCTION_NAME, _Msg]),
  case gen_tcp:send(ST#s_t.socket, Data) of
    ok -> ok;
    Error -> ?LOG_ERROR("~p: message was not send", [?FUNCTION_NAME, Data, Error])
  end,
  {noreply, ST};

handle_cast(_Msg={send, Data}, ST) when ST#s_t.tls_socket /= false ->
  ?LOG_DEBUG("~p: ssl:send: ~p~n", [?FUNCTION_NAME, _Msg]),
  case ssl:send(ST#s_t.tls_socket, Data) of
    ok -> ok;
    Error -> ?LOG_ERROR("~p: message ~p was not send; reason: ~p~n", [?FUNCTION_NAME, Data, Error])
  end,
  {noreply, ST};

handle_cast(_Msg={send, _Data}, ST) ->
  ?LOG_ERROR("~p: socket is not opened; message=~p was not sent~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST};

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
handle_info(_Msg=connect, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  Self = self(),
  spawn(fun() ->do_connect(Self, ST#s_t.addr) end),
  {noreply, ST};

handle_info(_Msg={connect, Socket}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  case ST#s_t.credits of 
    [] -> ok;
    _  -> self() ! handshake
  end,
  {noreply, ST#s_t{socket = Socket}};

handle_info(_Msg=handshake, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  Self = self(),
  Pid  = spawn(fun() -> do_handshake(Self, ST#s_t.socket, ST#s_t.credits) end),
  gen_tcp:controlling_process(ST#s_t.socket, Pid),
  Pid ! {go, Self},
  {noreply, ST};

handle_info(_Msg={handshake, TLSSocket}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST#s_t{tls_socket = TLSSocket}};

handle_info(Msg={tcp_closed, _Port}, ST) ->
  ?LOG_DEBUG("~p: ~p~n", [?FUNCTION_NAME, Msg]),
  self() ! {stop, Msg},
  {noreply, ST};

handle_info(_Msg = {report, _Reason}, ST) ->
  ?LOG_INFO("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  {noreply, ST};

handle_info(_Msg = {restart, _Reason}, ST) ->
  ?LOG_INFO("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  spawn(fun() -> tlsmlpp_sup:restart(ST#s_t.name) end),
  {noreply, ST};

handle_info(_Msg = {stop, _Reason}, ST) ->
  ?LOG_INFO("~p: ~p~n", [?FUNCTION_NAME, _Msg]),
  spawn(fun() -> tlsmlpp_sup:stop(ST#s_t.name) end),
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
  close_socket(ssl, ST#s_t.tls_socket),
  close_socket(tcp, ST#s_t.socket),
  terminated.

%-----------------------------------------------------------------------------------------------
%  Private
%-----------------------------------------------------------------------------------------------
?RECORD_TF_MAP(s_t).

%-----------------------------------------------------------------------------------------------
-spec close_socket(tcp | ssl, port()) -> ok.
% @doc Closes tcp or ssl sockets
close_socket(_, false) -> ok;
close_socket(tcp, Socket) -> gen_tcp:close(Socket);
close_socket(ssl, Socket) -> ssl:close(Socket).

%-----------------------------------------------------------------------------------------------
-spec do_connect(pid(), {inet:ipv4(), pos_integer()}) -> ok.
% @doc Connect to tcp socket 
do_connect(Parent, {Ip, Port}) -> 
  case gen_tcp:connect(Ip, Port, [], infinity) of 
    {ok, Socket} ->
      gen_tcp:controlling_process(Socket, Parent),
      Parent ! {connect, Socket};
    Error -> 
      Parent ! {report, Error}
  end.

%-----------------------------------------------------------------------------------------------
-spec do_handshake(pid(), port(), [tuple()]) -> ok.
% @doc Connect to ssl socket 
do_handshake(Parent, Socket, Credits) ->
  receive
    {go, Parent} ->
      case ssl:connect(Socket, Credits) of
        {ok, TLSSocket} ->
          gen_tcp:controlling_process(Socket, Parent),
          ssl:controlling_process(TLSSocket, Parent),
          Parent ! {handshake, TLSSocket};
      Error -> 
        Parent ! {report, Error}
      end
  end.

%-----------------------------------------------------------------------------------------------
% tcp_client:start(cc, {"127.0.0.1", 9999}).
% tcp_server:get_status(cc).
% tcp_client:send(cc, "Hellow server!").

%-----------------------------------------------------------------------------------------------
% tcp_client:start(cc, {"127.0.0.1", 9999}, []).
% tcp_client:send(cc, "Hellow server!").
% tcp_client:get_status(cc).
%-----------------------------------------------------------------------------------------------
% tcp_client:send('tcp_client_127.0.0.1:9999', "Hellow tcp!").
% tcp_client:send('tcp_client_127.0.0.1:9998', "Hellow ssl!").
% 'tcp_client_127.0.0.1:9999' ! {restart, "Restart tcp!"}.
% 'tcp_client_127.0.0.1:9998' ! {restart, "Restart ssl!"}.
% 'tcp_client_127.0.0.1:9999' ! {stop, "Stop tcp!"}.
% 'tcp_client_127.0.0.1:9998' ! {stop, "Stop ssl!"}.
%-----------------------------------------------------------------------------------------------
