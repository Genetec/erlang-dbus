-module(dbus_bus_connection_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("dbus/include/dbus.hrl").
-compile(nowarn_export_all).
-compile(export_all).

all() ->
  [
   get_system_bus_id,
   session_unix_path,
   session_unix_dir,
   session_tcp_connect,
   multiple_connection,
   unsupported_connection,
   unsupported_mix,
   unexport_variable,
   can_connect,
   can_reuse_same_connection,
   can_call,
   can_cast,
   can_close
  ].

init_per_suite(Config) ->
  meck:new(dbus_os_wrapper, [no_link]),
  meck:new(dbus_peer_connection, [no_link]),
  meck:new(dbus_proxy, [no_link]),
  Config.

end_per_suite(Config) ->
  meck:unload(),
  Config.

init_per_testcase(_, Config) ->
  meck:reset(dbus_proxy),
  Config.

end_per_testcase(_, Config) ->
  Config.

get_system_bus_id() ->
  [{doc, "When get the system bus id, then return a unix type with the proper path."}].
get_system_bus_id(_Config) ->
  Id = dbus_bus_connection:get_bus_id(system),

  ?assertMatch(#bus_id{scheme = unix, options = [{path, "/var/run/dbus/system_bus_socket"}]}, Id).

session_unix_path() ->
  [{doc, "Given the env variable exported, when contains a unix domain connection, then return the id."}].
session_unix_path(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["DBUS_SESSION_BUS_ADDRESS"], "unix:path=/var/run/bus.sock"}]),

  Id = dbus_bus_connection:get_bus_id(session),

  ?assertMatch(#bus_id{scheme = unix, options = [{path, "/var/run/bus.sock"}]}, Id).

session_unix_dir() ->
  [{doc, "Given the env variable exported, when contains a unix domain dir, then return the id."}].
session_unix_dir(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["DBUS_SESSION_BUS_ADDRESS"], "unix:dir=/var/run/bus.sock"}]),

  Id = dbus_bus_connection:get_bus_id(session),

  ?assertMatch(#bus_id{scheme = unix, options = [{dir, "/var/run/bus.sock"}]}, Id).

session_tcp_connect() ->
  [{doc, "Given the env variable exported, when contains a tcp connection, then return the id."}].
session_tcp_connect(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["DBUS_SESSION_BUS_ADDRESS"], "tcp:host=127.0.0.1,port=9090"}]),

  Id = dbus_bus_connection:get_bus_id(session),

  ?assertMatch(#bus_id{scheme = tcp, options = [{port, 9090}, {host, "127.0.0.1"}]}, Id).

multiple_connection() ->
  [{doc, "Given the env variable exported, when contains multiple connection, then return the first one."}].
multiple_connection(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["DBUS_SESSION_BUS_ADDRESS"], "unix:path=/var/run/bus.sock;unix:path=/var/run/bus2.sock"}]),

  Id = dbus_bus_connection:get_bus_id(session),

  ?assertMatch(#bus_id{scheme = unix, options = [{path, "/var/run/bus.sock"}]}, Id).

unsupported_connection() ->
  [{doc, "Given the env variable exported, when contains an unsupported connection, then return the first one."}].
unsupported_connection(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["DBUS_SESSION_BUS_ADDRESS"], "unixexec:path=/usr/bin/exec"}]),

  Id = dbus_bus_connection:get_bus_id(session),

  ?assertMatch({unsupported, [#bus_id{scheme = unixexec, options = [{path, "/usr/bin/exec"}]}]}, Id).

unsupported_mix() ->
  [{doc, "Given the env variable exported, when contains an unsupported and a supported connection, then return the supported."}].
unsupported_mix(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["DBUS_SESSION_BUS_ADDRESS"], "unixexec:path=/usr/bin/exec;unix:path=/var/run/bus.sock"}]),

  Id = dbus_bus_connection:get_bus_id(session),

  ?assertMatch(#bus_id{scheme = unix, options = [{path, "/var/run/bus.sock"}]}, Id).

unexport_variable() ->
  [{doc, "Given the env variable exported, when contains an unsupported and a supported connection, then return the supported."}].
unexport_variable(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["DBUS_SESSION_BUS_ADDRESS"], false}]),

  ?assertError(function_clause, dbus_bus_connection:get_bus_id(session)).

can_connect() ->
  [{doc, "Given no already connection establish, when connect, then call hello and return ok."}].
can_connect(_Config) ->
  meck:expect(dbus_peer_connection, start_link, [{['_'], {ok, {dbus_peer_connection, pid}}}]),
  meck:expect(dbus_peer_connection, auth, [{[pid], {ok, undefined}}]),
  meck:expect(dbus_peer_connection, set_controlling_process, [{[pid, pid2], ok}]),
  meck:expect(dbus_proxy, start_link, [{['_', 'org.freedesktop.DBus', <<"/">>, '_'], {ok, pid2}}]),
  meck:expect(dbus_proxy, call, [{[pid2, 'org.freedesktop.DBus', 'Hello', []], {ok, <<":1,0">>}}]),

  Ret = dbus_bus_connection:connect(system),

  ?assertEqual({ok, {dbus_bus_connection, pid2}}, Ret),
  ?assert(meck:called(dbus_proxy, call, [pid2, 'org.freedesktop.DBus', 'Hello', []])).

can_reuse_same_connection() ->
  [{doc, "Given a connection already establish, when connect, then reuse the same connection id."}].
can_reuse_same_connection(_Config) ->
  meck:expect(dbus_peer_connection, start_link, [{['_'], {ok, {dbus_peer_connection, pid}}}]),
  meck:expect(dbus_peer_connection, auth, [{[pid], {ok, <<":1.0">>}}]),
  meck:expect(dbus_peer_connection, set_controlling_process, [{[pid, pid2], ok}]),
  meck:expect(dbus_proxy, start_link, [{['_', 'org.freedesktop.DBus', <<"/">>, '_'], {ok, pid2}}]),

  Ret = dbus_bus_connection:connect(system),

  ?assertEqual({ok, {dbus_bus_connection, pid2}}, Ret),
  ?assertNot(meck:called(dbus_proxy, call, '_')).

can_call() ->
  [{doc, "Given a dbus_connection, when call on it, then call the proxy."}].
can_call(_Config) ->
  meck:expect(dbus_proxy, call, fun(_, _) -> ok end),

  ok = dbus_bus_connection:call({dbus_bus_connection, pid}, #dbus_message{}),
  ok = dbus_bus_connection:call({other_connection, pid}, #dbus_message{}),

  ?assertEqual(2, meck:num_calls(dbus_proxy, call, '_')).

can_cast() ->
  [{doc, "Given a dbus_connection, when cast on it, then cast the proxy."}].
can_cast(_Config) ->
  meck:expect(dbus_proxy, cast, fun(_, _) -> ok end),

  ok = dbus_bus_connection:cast({dbus_bus_connection, pid}, #dbus_message{}),
  ok = dbus_bus_connection:cast({other_connection, pid}, #dbus_message{}),

  ?assertEqual(2, meck:num_calls(dbus_proxy, cast, '_')).


can_close() ->
  [{doc, "Given a dbus_connection, when close it, then stop the proxy."}].
can_close(_Config) ->
  meck:expect(dbus_proxy, stop, fun(_) -> ok end),

  ok = dbus_bus_connection:close({dbus_bus_connection, pid}),
  ok = dbus_bus_connection:close({other_connection, pid}),

  ?assertEqual(2, meck:num_calls(dbus_proxy, stop, '_')).
