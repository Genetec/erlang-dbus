-module(dbus_auth_cookie_sha1_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_export_all).
-compile(export_all).

all() ->
  [
   sha1_init
  ].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

sha1_init() ->
  [{doc, "Given the USER env var exported, when init authentication, then return the user name hez encoded."}].
sha1_init(_Config) ->
  {continue, Payload, _} = dbus_auth_cookie_sha1:init(),

  ?assertEqual(<<"DBUS_COOKIE_SHA1 0">>, Payload).
