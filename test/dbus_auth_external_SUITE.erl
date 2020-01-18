-module(dbus_auth_external_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_export_all).
-compile(export_all).

all() ->
  [
   external_init_no_cookie,
   external_init_with_cookie,
   external_init_with_cookie_list,
   external_init_with_cookie_int
  ].

init_per_suite(Config) ->
  ok = application:load(dbus),
  Config.

end_per_suite(Config) ->
  ok = application:unload(dbus),
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

external_init_no_cookie() ->
  [{doc, "Given no cookie configure, when init external authentication, then return the user 1000 has hex."}].
external_init_no_cookie(_Config) ->
  {ok, Payload} = dbus_auth_external:init(),

  ?assertEqual(<<"EXTERNAL 31303030">>, Payload).

external_init_with_cookie() ->
  [{doc, "Given a cookie set as binary, when init external, then add the cookie has his."}].
external_init_with_cookie(_Config) ->
  application:set_env(dbus, external_cookie, <<"0">>),

  {ok, Payload} = dbus_auth_external:init(),

  ?assertEqual(<<"EXTERNAL 0">>, Payload).

external_init_with_cookie_list() ->
  [{doc, "Given a cookie set as list, when init external, then add the cookie has his."}].
external_init_with_cookie_list(_Config) ->
  application:set_env(dbus, external_cookie, "0"),

  {ok, Payload} = dbus_auth_external:init(),

  ?assertEqual(<<"EXTERNAL 0">>, Payload).

external_init_with_cookie_int() ->
  [{doc, "Given a cookie set as integer, when init external, then add the cookie has his."}].
external_init_with_cookie_int(_Config) ->
  application:set_env(dbus, external_cookie, 0),

  {ok, Payload} = dbus_auth_external:init(),

  ?assertEqual(<<"EXTERNAL 0">>, Payload).
