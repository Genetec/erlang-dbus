-module(dbus_constants_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_export_all).
-compile(export_all).

groups() ->
  [{all, [parallel], [
                      introstpect,
                      unknown_method,
                      other_binary
                     ]}].

all() ->
  [
   {group, all}
  ].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

introstpect() ->
  [{doc, "Given the introspect binary, when convert it to an atom, then return the atom."}].
introstpect(_Config) ->
  ?assertEqual('Introspect', dbus_constants:to_atom(<<"Introspect">>)).

unknown_method() ->
  [{doc, "Given the unknown method error, when convert it to an atom, then return the atom."}].
unknown_method(_Config) ->
  ?assertEqual('org.freedesktop.DBus.Error.UnknownMethod',
               dbus_constants:to_atom(<<"org.freedesktop.DBus.Error.UnknownMethod">>)).

other_binary() ->
  [{doc, "Given an other binary, when convert it to an atom, then return the binary."}].
other_binary(_Config) ->
  ?assertEqual(<<"any_other">>, dbus_constants:to_atom(<<"any_other">>)).
