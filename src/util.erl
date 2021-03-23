%-----------------------------------------------------------------------------------------------
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @copyright 2021 Anatoly Rodionov

%-----------------------------------------------------------------------------------------------
% @doc Collection of multiple used utilities.
% 
% @end
%-----------------------------------------------------------------------------------------------
-module(util).

-export([default_credits/0, default_credits/2]).

%-----------------------------------------------------------------------------------------------
-define(DEFAULT_CERT, "cert.pem").
-define(DEFAULT_KEY,  "key.pem").
-define(APPL_NAME, tlsmlpp).

-spec default_credits() -> [{atom(), string()}].
% @doc generate default credits for testing purposes
default_credits() -> default_credits(?DEFAULT_CERT, ?DEFAULT_KEY).

-spec default_credits(string(), string()) -> [{atom(), string()}].
% @doc generate default credits for testing purposes
default_credits(CERT, KEY) ->
  PrivDir = code:priv_dir(?APPL_NAME),
  [{certfile, filename:join(PrivDir, CERT)}, {keyfile, filename:join(PrivDir, KEY)}].

%-----------------------------------------------------------------------------------------------
