-module(dbus_hex_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_export_all).
-compile(export_all).

groups() ->
  [
   {all, [parallel], [can_hex_encode,
                      can_hex_decode,
                      decode_stop_at_cr,
                      decode_stop_at_lf,
                      decode_stop_at_space]}
  ].

all() ->
  [{group, all}].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

can_hex_encode() ->
  [{doc, "When encode a string, then return hex string."}].
can_hex_encode(_Config) ->
  ?assertEqual(<<>>, dbus_hex:encode(<<>>)),
  ?assertEqual(<<"61">>, dbus_hex:encode(<<"a">>)),
  ?assertEqual(<<"6161">>, dbus_hex:encode(<<"aa">>)).

can_hex_decode() ->
  [{doc, "When decode a string, then return the ASCII string."}].
can_hex_decode(_Config) ->
  ?assertEqual(<<>>, dbus_hex:decode(<<>>)),
  ?assertEqual(<<"a">>, dbus_hex:decode(<<"61">>)).

decode_stop_at_cr() ->
  [{doc, "Given a string with a cr, when decode it, then return the string prior to it."}].
decode_stop_at_cr(_Config) ->
  ?assertEqual(<<"a">>, dbus_hex:decode(<<"61", $\n, "61">>)).

decode_stop_at_lf() ->
  [{doc, "Given a string with a crlf, when decode it, then return the string prior to it."}].
decode_stop_at_lf(_Config) ->
  ?assertEqual(<<"a">>, dbus_hex:decode(<<"61", $\n, $\r, "61">>)),
  ?assertEqual(<<"a">>, dbus_hex:decode(<<"61", $\r, "61">>)).

decode_stop_at_space() ->
  [{doc, "Given a string with a space, when decode it, then return the string prior to it."}].
decode_stop_at_space(_Config) ->
  ?assertEqual(<<"a">>, dbus_hex:decode(<<"61", $\s, $\r, "61">>)).
