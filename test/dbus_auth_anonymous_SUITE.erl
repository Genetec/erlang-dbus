-module(dbus_auth_anonymous_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_export_all).
-compile(export_all).

-define(A_BINARY, <<>>).
-define(A_TERM, term).

all() ->
  [
   anonymous_init,
   anonymous_challange
  ].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  Config.

anonymous_init() ->
  [{doc, "When init the anonymous authentication, then return anonymous."}].
anonymous_init(_Config) ->
  ?assertEqual({ok, <<"ANONYMOUS">>}, dbus_auth_anonymous:init()).

anonymous_challange() ->
  [{doc, "When challenge on anonymous, then return an error."}].
anonymous_challange(_Config) ->
  ?assertEqual({error, invalid_challenge}, dbus_auth_anonymous:challenge(?A_BINARY, ?A_TERM)).
