%-------------------------------------------------------------------
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @copyright 2021 Anatoly Rodionov
%-------------------------------------------------------------------
% @doc Tests for tlsmlpp_sup
% @end
%------------------------------------------------------------------

-module(rtpecho_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%-----------------------------------------------------------------------------------------------
mk_name_1_test() ->
    ?assertMatch(
        'tcp_client_1.1.1.1:5678',
        tlsmlpp_sup:mk_name(tcp_client, {"1.1.1.1", 5678})),
    ?assertMatch(
        'tcp_server_1.1.1.2:6666',
        tlsmlpp_sup:mk_name(tcp_server, {"1.1.1.2", 6666})),
    ok.

%-----------------------------------------------------------------------------------------------
