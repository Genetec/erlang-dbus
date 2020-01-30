-module(dbus_bus_reg_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("dbus/include/dbus.hrl").
-compile(nowarn_export_all).
-compile(export_all).

-define(A_BUS_ID, #bus_id{}).

all() ->
  [
   get_bus,
   get_bus_second_time,
   expect_bus_id,
   release_registered_bus,
   release_non_registered_bus,
   release_not_a_pid,
   export_service_name,
   cast_message_busses
  ].

init_per_suite(Config) ->
  meck:new(dbus_bus, [no_link]),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_, Config) ->
  meck:reset(dbus_bus),

  meck:expect(dbus_bus, connect, fun(_) -> make_pid() end),
  meck:expect(dbus_bus, stop, fun(_) -> ok end),
  meck:expect(dbus_bus, export_service, fun(_, _) -> ok end),
  meck:expect(dbus_bus, cast, fun(_, _) -> ok end),

  {ok, _} = dbus_bus_reg:start_link(),
  Config.

end_per_testcase(_, Config) ->
  Config.

get_bus() ->
  [{doc, "Given no bus started, when get a bus for the first time, then start it."}].
get_bus(_Config) ->
  {ok, Pid} = dbus_bus_reg:get_bus(?A_BUS_ID),

  ?assert(is_pid(Pid)),
  ?assert(meck:called(dbus_bus, connect, [?A_BUS_ID])).

get_bus_second_time() ->
  [{doc, "Given the bus already started, when get a bus, then do not start it again."}].
get_bus_second_time(_Config) ->
  {ok, Pid} = dbus_bus_reg:get_bus(?A_BUS_ID),

  {ok, Pid2} = dbus_bus_reg:get_bus(?A_BUS_ID),

  ?assertEqual(Pid, Pid2),
  ?assertEqual(1, meck:num_calls(dbus_bus, connect, [?A_BUS_ID])).

expect_bus_id() ->
  [{doc, "Given a know bus, when get the bus with it name, then raise an error."}].
expect_bus_id(_Config) ->
  ?assertError(function_clause, dbus_bus_reg:get_bus(system)).

release_registered_bus() ->
  [{doc, "Given a registered bus, when release it, then stop it."}].
release_registered_bus(_Config) ->
  {ok, Pid} = dbus_bus_reg:get_bus(?A_BUS_ID),

  ok = dbus_bus_reg:release_bus(Pid),

  ?assert(meck:called(dbus_bus, stop, [Pid])).

release_non_registered_bus() ->
  [{doc, "Given a non registered bus, when release it, the return an error."}].
release_non_registered_bus(_Config) ->
  {ok, OtherPid} = make_pid(),

  Error = dbus_bus_reg:release_bus(OtherPid),

  ?assertEqual({error, not_registered}, Error).

release_not_a_pid() ->
  [{doc, "Given a bus id, when try to release it, then raise an error."}].
release_not_a_pid(_Config) ->
  ?assertError(function_clause, dbus_bus_reg:release_bus(?A_BUS_ID)).

export_service_name() ->
  [{doc, "Given a registered bus, when export a service name, then it is exported for him."}].
export_service_name(_Config) ->
  {ok, _} = dbus_bus_reg:get_bus(?A_BUS_ID),

  ok = dbus_bus_reg:export_service(service, 'Name'),

  ?assert(meck:called(dbus_bus, export_service, ['_', 'Name'])).

cast_message_busses() ->
  [{doc, "Given a registered bus, when cast a message, then cast it on the bus."}].
cast_message_busses(_Config) ->
  {ok, Pid} = dbus_bus_reg:get_bus(?A_BUS_ID),

  ok = dbus_bus_reg:cast(#dbus_message{}),

  meck:wait(dbus_bus, cast, [Pid, '_'], 1000).


make_pid() ->
  {ok, spawn_link(fun() -> timer:sleep(5000) end)}.
