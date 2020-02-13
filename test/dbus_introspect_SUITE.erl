-module(dbus_introspect_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("dbus/include/dbus.hrl").
-compile(nowarn_export_all).
-compile(export_all).

groups() ->
  [{read, [parallel], [
                       can_read_valid_xml,
                       find_valid_interface,
                       find_valid_interface_atom,
                       find_missing_interface,
                       find_method,
                       find_missing_method,
                       find_missing_interface_method,
                       find_signal,
                       find_missing_signal,
                       find_missing_interface_signal
                      ]}].

all() ->
  [{group, read}].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

can_read_valid_xml() ->
  [{doc, "Given a valid introspect xml file, when read it, then return the node."}].
can_read_valid_xml(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),

  Node = dbus_introspect:from_xml(Path),

  ?assertEqual(<<"/com/example/sample_object0">>, Node#dbus_node.name).

find_valid_interface() ->
  [{doc, "Given an interface as binary, when found it, then return the interface."}].
find_valid_interface(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {ok, Iface} = dbus_introspect:find_interface(Node, <<"com.example.SampleInterface0">>),

  ?assertEqual(<<"com.example.SampleInterface0">>, Iface#dbus_iface.name).

find_valid_interface_atom() ->
  [{doc, "Given an interface name as atom, when found it, then return the interface."}].
find_valid_interface_atom(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {ok, Iface} = dbus_introspect:find_interface(Node, 'com.example.SampleInterface0'),

  ?assertEqual(<<"com.example.SampleInterface0">>, Iface#dbus_iface.name).

find_missing_interface() ->
  [{doc, "Given a missing interface, when search for it, then return an error."}].
find_missing_interface(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {error, Error} = dbus_introspect:find_interface(Node, 'com.example.SampleInterface'),

  ?assertEqual({'org.freedesktop.DBus.UnknownInterface', ['com.example.SampleInterface']}, Error).

find_method() ->
  [{doc, "Given an available method in an interface, when found it, then return it."}].
find_method(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {ok, Method} = dbus_introspect:find_method(Node, 'com.example.SampleInterface0', 'Frobate'),

  ?assertEqual(<<"Frobate">>, Method#dbus_method.name).

find_missing_method() ->
  [{doc, "Given a missing method in an interface, when search for it, then return an error."}].
find_missing_method(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {error, Error} = dbus_introspect:find_method(Node, 'com.example.SampleInterface0', 'Frobate1'),

  ?assertEqual({'org.freedesktop.DBus.UnknownMethod', {['Frobate1'], 'com.example.SampleInterface0', Node}},
               Error).

find_missing_interface_method() ->
  [{doc, "Given a missing interface, when search for a method, then return an error."}].
find_missing_interface_method(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {error, Error} = dbus_introspect:find_method(Node, 'com.example.SampleInterface', 'Frobate'),

  ?assertEqual({'org.freedesktop.DBus.UnknownInterface', ['com.example.SampleInterface']}, Error).

find_signal() ->
  [{doc, "Given an available signal in an interface, when found it, then return it."}].
find_signal(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {ok, Method} = dbus_introspect:find_signal(Node, 'com.example.SampleInterface0', 'Changed'),

  ?assertEqual(<<"Changed">>, Method#dbus_signal.name).

find_missing_signal() ->
  [{doc, "Given a missing signal in an interface, when search for it, then return an error."}].
find_missing_signal(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {error, Error} = dbus_introspect:find_signal(Node, 'com.example.SampleInterface0', 'Changed1'),

  ?assertEqual({'org.freedesktop.DBus.UnknownSignal', {['Changed1'], 'com.example.SampleInterface0', Node}},
               Error).

find_missing_interface_signal() ->
  [{doc, "Given a missing interface, when search for a signal, then return an error."}].
find_missing_interface_signal(Config) ->
  Path = filename:join([?config(data_dir, Config), "valid.xml"]),
  Node = dbus_introspect:from_xml(Path),

  {error, Error} = dbus_introspect:find_signal(Node, 'com.example.SampleInterface', 'Changed'),

  ?assertEqual({'org.freedesktop.DBus.UnknownInterface', ['com.example.SampleInterface']}, Error).
