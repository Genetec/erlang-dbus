-module(dbus_bus_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("dbus.hrl").
-compile(nowarn_export_all).
-compile(export_all).

all() ->
  [
   dbus_connect,
   dbus_connect_ignore,
   dbus_connection_error,
   export_service,
   unexport_service,
   remote_service,
   remote_service_cached,
   link_to_remote_service
  ].

init_per_suite(Config) ->
  meck:new(dbus_bus_connection, [no_link]),
  meck:new(dbus_proxy, [no_link]),
  meck:new(dbus_remote_service, [no_link]),
  Config.

end_per_suite(Config) ->
  meck:unload(),
  Config.

init_per_testcase(_, Config) ->
  meck:reset(dbus_remote_service),

  meck:expect(dbus_bus_connection, connect, fun(_) -> {ok, {dbus_bus_connection, pid}} end),
  meck:expect(dbus_proxy, call, fun(_, _, _, _) -> {ok, msg} end),
  meck:expect(dbus_remote_service, start_link, fun(_, _, _) -> {ok, pid} end),
  Config.

end_per_testcase(_, Config) ->
  Config.


dbus_connect() ->
  [{doc, "Given an available bus id, when connect, then start the gen_server."}].
dbus_connect(_Config) ->
  Id = #bus_id{},

  {ok, Pid} = dbus_bus:connect(Id),

  ?assert(is_pid(Pid)).

dbus_connect_ignore() ->
  [{doc, "Given a connection ignored, when connect, then does not start the gen_server."}].
dbus_connect_ignore(_Config) ->
  meck:expect(dbus_bus_connection, connect, fun(_) -> ignore end),
  Id = #bus_id{},

  Ret = dbus_bus:connect(Id),

  ?assertEqual(ignore, Ret).

dbus_connection_error() ->
  [{doc, "Given a connection error, when connect, then return the error."}].
dbus_connection_error(_Config) ->
  Old = process_flag(trap_exit, true),
  meck:expect(dbus_bus_connection, connect, fun(_) -> {error, an_error} end),
  Id = #bus_id{},

  Ret = dbus_bus:connect(Id),

  ?assertEqual({error, an_error}, Ret),
  process_flag(trap_exit, Old).

export_service() ->
  [{doc, "When export a service, then request a name on DBus."}].
export_service(_Config) ->
  Id = #bus_id{},
  {ok, Pid} = dbus_bus:connect(Id),

  ok = dbus_bus:export_service(Pid, 'my_service'),

  ?assert(meck:called(dbus_proxy, call, [undefined, 'org.freedesktop.DBus', 'RequestName', ['my_service', 0]])).

unexport_service() ->
  [{doc, "When unexport a service, then release a name on DBus."}].
unexport_service(_Config) ->
  Id = #bus_id{},
  {ok, Pid} = dbus_bus:connect(Id),

  ok = dbus_bus:unexport_service(Pid, 'my_service'),

  ?assert(meck:called(dbus_proxy, call, [undefined, 'org.freedesktop.DBus', 'ReleaseName', ['my_service']])).

remote_service() ->
  [{doc, "Given a remote service not yet started, when get it for the first time, then start it."}].
remote_service(_Config) ->
  Id = #bus_id{},
  {ok, Pid} = dbus_bus:connect(Id),

  {ok, ServicePid} = dbus_bus:get_service(Pid, 'my_service'),

  ?assertEqual(pid, ServicePid).

remote_service_cached() ->
  [{doc, "Given a remote service already started, when get it again, then return the same service and do not start it."}].
remote_service_cached(_Config) ->
  Id = #bus_id{},
  {ok, Pid} = dbus_bus:connect(Id),
  {ok, ServicePid} = dbus_bus:get_service(Pid, 'my_service'),

  {ok, ServicePid2} = dbus_bus:get_service(Pid, 'my_service'),

  ?assertEqual(ServicePid, ServicePid2),
  ?assertEqual(1, meck:num_calls(dbus_remote_service, start_link, '_')).

link_to_remote_service() ->
  [{doc, "When get a remote service, then the calling process is linked to the dbus_bus server."}].
link_to_remote_service(_Config) ->
  Id = #bus_id{},
  {ok, Pid} = dbus_bus:connect(Id),
  unlink(Pid),

  Worker = fun() ->
               process_flag(trap_exit, true),
               {ok, _} = dbus_bus:get_service(Pid, 'my_service'),
               exit(Pid, {error, error}),
               receive
                 {'EXIT', Pid, R} ->
                   ?assertEqual({error, error}, R)
               end,
               ok
           end,

  {Pid2, Ref} = spawn_monitor(Worker),

  receive
    {'DOWN', Ref, process, Pid2, normal} ->
      ok
  after 1000 ->
      ct:fail("Process did not died")
  end.
