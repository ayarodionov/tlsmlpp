%-----------------------------------------------------------------------------------------------
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @copyright 2021 Anatoly Rodionov

%-----------------------------------------------------------------------------------------------
% @doc MLLP coder and decoder
% 
% See < a href="https://www.hl7.org/documentcenter/public/wg/inm/mllp_transport_specification.PDF">mlpp specification</a>
% @end
%-----------------------------------------------------------------------------------------------
-ifndef(MLPP).
%-----------------------------------------------------------------------------------------------
-define(MLPP, true).

-define(SB, 16#0B).
-define(EB, 16#1C).
-define(CR, 16#0D).

%-----------------------------------------------------------------------------------------------
-compile({inline, [encode/1, decode/1]}).

%-----------------------------------------------------------------------------------------------
-spec encode(string() | binary()) -> binary().
% @doc Encodes to MLLP fort
encode(Msg) when is_list(Msg)   -> <<?SB:8,(list_to_binary(Msg))/binary, ?EB:8,?CR:8>>;
encode(Msg) when is_binary(Msg) -> <<?SB:8,Msg/binary,?EB:8,?CR:8>>.

%-----------------------------------------------------------------------------------------------
-spec decode(binary()) -> binary().
% @doc Decodes from MLLP format
decode(<<?SB:8,M/binary>>) ->
	L = byte_size(M) - 2,
	<<Msg:L/binary, ?EB:8,?CR:8>> = M,  
	Msg.

%-----------------------------------------------------------------------------------------------
-endif.
%-----------------------------------------------------------------------------------------------
