-module(whistle_util).

-export([to_e164/1, to_npanxxxxxx/1, to_1npanxxxxxx/1]).
-export([to_integer/1, to_float/1, to_hex/1, to_list/1, to_binary/1, to_atom/1, to_atom/2]).
-export([to_boolean/1, is_true/1]).
-export([a1hash/3, floor/1, ceiling/1]).
-export([current_tstamp/0]).

-include_lib("proper/include/proper.hrl").

%% must be a term that can be changed to a list
-spec(to_hex/1 :: (S :: term()) -> string()).
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

%% +18001234567 -> +18001234567
-spec(to_e164/1 :: (DID :: binary()) -> binary()).
to_e164(<<$+, $1, N/bitstring>>=E164) when erlang:bit_size(N) =:= 80 -> % 8bits/ch * 10ch
    E164;
%% 18001234567 -> +18001234567
to_e164(<<$1, N/binary>>=NPAN1) when erlang:bit_size(N) =:= 80 ->
    << $+, NPAN1/bitstring >>;
%% 8001234567 -> +18001234567
to_e164(NPAN) when erlang:bit_size(NPAN) =:= 80 ->
    <<$+, $1, NPAN/bitstring>>;
to_e164(Other) ->
    Other.

prop_to_e164() ->
    ?FORALL(Number, list(range(0,9)),
	    begin
		BinNum = list_to_binary(Number),
		E164 = to_e164(BinNum),
		case {length(Number), BinNum} of
		    {11, <<$1, _/binary>>} -> E164 =:= <<$+, BinNum/binary>>;
		    {10, _} -> E164 =:= <<$+, $1, BinNum/binary>>;
		    _ -> E164 =:= BinNum
		end
	    end).

%% end up with 8001234567 from 1NPAN and E.164
-spec(to_npanxxxxxx/1 :: (NPAN :: binary()) -> binary()).
to_npanxxxxxx(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) =:= 80 ->
    N;
to_npanxxxxxx(<<$1, N/bitstring>>) when erlang:bit_size(N) =:= 80 ->
    N;
to_npanxxxxxx(NPAN) when erlang:bit_size(NPAN) =:= 80 ->
    NPAN;
to_npanxxxxxx(Other) ->
    Other.

prop_to_npanxxxxxx() ->
    ?FORALL(Number, list(range(0,9)),
	    begin
		BinNum = list_to_binary(Number),
		NPAN = to_npanxxxxxx(BinNum),
		case {length(Number), BinNum} of
		    {11, <<_, N/binary>>} -> N =:= NPAN;
		    {10, _} -> NPAN =:= BinNum;
		    _ -> NPAN =:= BinNum
		end
	    end).

to_1npanxxxxxx(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) =:= 80 ->
    <<$1, N/bitstring>>;
to_1npanxxxxxx(<<$1, N/bitstring>>=NPAN1) when erlang:bit_size(N) =:= 80 ->
    NPAN1;
to_1npanxxxxxx(NPAN) when erlang:bit_size(NPAN) =:= 80 ->
    <<$1, NPAN/bitstring>>;
to_1npanxxxxxx(Other) ->
    Other.

-spec(to_integer/1 :: (X :: list() | binary() | integer() | float()) -> integer()).
to_integer(X) when is_float(X) ->
    round(X);
to_integer(X) when is_binary(X) ->
    list_to_integer(binary_to_list(X));
to_integer(X) when is_list(X) ->
    list_to_integer(X);
to_integer(X) when is_integer(X) ->
    X.

-spec(to_float/1 :: (X :: list() | binary() | integer() | float()) -> float()).
to_float(X) when is_binary(X) ->
    list_to_float(binary_to_list(X));
to_float(X) when is_list(X) ->
    try
	list_to_float(X)
    catch
	error:badarg -> to_float(list_to_integer(X)) %% "500" -> 500.0
    end;
to_float(X) when is_integer(X) ->
    X * 1.0;
to_float(X) when is_float(X) ->
    X.

-spec(to_list/1 :: (X :: atom() | list() | binary() | integer() | float()) -> list()).
to_list(X) when is_float(X) ->
    mochinum:digits(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

-spec(to_binary/1 :: (X :: atom() | list() | binary() | integer() | float()) -> binary()).
to_binary(X) when is_float(X) ->
    to_binary(mochinum:digits(X));
to_binary(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) ->
    iolist_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.

%% the safer version, won't let you leak atoms
-spec(to_atom/1 :: (X :: term()) -> atom()).
to_atom(X) when is_list(X) ->
    list_to_existing_atom(X);
to_atom(X) ->
    to_atom(to_list(X)).

%% only if you're really sure you want this
-spec(to_atom/2 :: (X :: term(), true) -> atom()).
to_atom(X, true) when is_list(X) ->
    list_to_atom(X);
to_atom(X, true) ->
    to_atom(to_list(X), true).

-spec(to_boolean/1 :: (X :: term()) -> boolean()).
to_boolean(<<"true">>) -> true;
to_boolean("true") -> true;
to_boolean(true) -> true;
to_boolean(<<"false">>) -> false;
to_boolean("false") -> false;
to_boolean(false) -> false.

-spec(is_true/1 :: (X :: term()) -> boolean()).
is_true(<<"true">>) -> true;
is_true("true") -> true;
is_true(true) -> true;
is_true(_) -> false.

-spec(a1hash/3 :: (User :: binary() | list(), Realm :: binary() | list(), Password :: binary() | list()) -> string()).
a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

%% found via trapexit
-spec(floor/1 :: (X :: integer() | float()) -> integer()).
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T =:= 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).

%% found via trapexit
-spec(ceiling/1 :: (X :: integer() | float()) -> integer()).
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T =:= 0 of
        true -> T;
        false -> T + 1
    end.

current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% EUNIT TESTING

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

%-export([to_integer/1, to_float/1, to_hex/1, to_list/1, to_binary/1]).
%-export([a1hash/3, floor/1, ceiling/1]).

to_e164_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"+11234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_e164(N), Ans) end, Ns).

to_npanxxxxxx_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"1234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_npanxxxxxx(N), Ans) end, Ns).

to_1npanxxxxxx_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"11234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_1npanxxxxxx(N), Ans) end, Ns).

to_integer_test() ->
    Good = [42, 4.2, "42", <<"42">>],
    Bad = [ an_atom, "4.2", <<"4.2">>],
    lists:foreach(fun(G) -> ?assertEqual(is_integer(to_integer(G)), true) end, Good),
    lists:foreach(fun(B) ->
			  ok = try to_integer(B) catch _:_ -> ok end
		  end, Bad).

-endif.
