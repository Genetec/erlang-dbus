-module(dbus_auth_cookie_sha1_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_export_all).
-compile(export_all).

all() ->
  [
   sha1_init,
   sha1_no_user,
   sha1_challange,
   sha1_no_cookie_file,
   sha1_empty_cookie_file,
   sha1_malforme_cookie,
   sha1_unmatch_cookie
  ].

init_per_suite(Config) ->
  meck:new(dbus_os_wrapper, [no_link]),
  Config.

end_per_suite(Config) ->
  meck:unload(),
  Config.

init_per_testcase(_, Config) ->
  meck:new(file, [unstick, passthrough]),
  meck:expect(dbus_os_wrapper, getenv, [{["USER"], "me"},
                                        {["HOME"], "/a/path"}]),

  meck:expect(file, open, [{[<<"/a/path/.dbus-keyrings/org_freedesktop_general">>, '_'], {ok, fd}},
                           {['_', '_'], meck:passthrough()}]),

  meck:expect(file, read_line, [{[fd], {ok, <<"0 10 00">>}},
                                {['_'], meck:passthrough()}]),

  meck:expect(file, close, [{[fd], ok},
                            {['_'], meck:passthrough()}]),
  Config.

end_per_testcase(_, Config) ->
  meck:unload(file),
  Config.

sha1_init() ->
  [{doc, "Given the USER env var exported, when init authentication, then return the user name hez encoded."}].
sha1_init(_Config) ->
  {continue, Payload, waiting_challenge} = dbus_auth_cookie_sha1:init(),

  ?assertEqual(<<"DBUS_COOKIE_SHA1 6d65">>, Payload).

sha1_no_user() ->
  [{doc, "Given the USER env var not exported, when init authentication, then return an error."}].
sha1_no_user(_Config) ->
  meck:expect(dbus_os_wrapper, getenv, [{["USER"], false}]),

  Error = dbus_auth_cookie_sha1:init(),

  ?assertEqual({error, invalid_user}, Error).

sha1_challange() ->
  [{doc, "Given the cookie file read, when send the response, then send the response as hex endoced."}].
sha1_challange(_Config) ->
  Challenge = dbus_hex:encode(<<"org_freedesktop_general 0 1">>),

  {ok, Response} = dbus_auth_cookie_sha1:challenge(Challenge, waiting_challenge),

  Decoded = dbus_hex:decode(Response),
  ?assertMatch([_, _], binary:split(Decoded, [<<$\s>>])).

sha1_no_cookie_file() ->
  [{doc, "Given no cookie file, when try the challange, then return an error."}].
sha1_no_cookie_file(_Config) ->
  Challenge = dbus_hex:encode(<<"org_freedesktop_general 0 1">>),
  meck:expect(file, open, [{[<<"/a/path/.dbus-keyrings/org_freedesktop_general">>, '_'], {error, enoent}},
                           {['_', '_'], meck:passthrough()}]),

  Error = dbus_auth_cookie_sha1:challenge(Challenge, waiting_challenge),

  ?assertEqual({error, no_cookie}, Error).

sha1_empty_cookie_file() ->
  [{doc, "Given an empty cookie file, when try the challange, then return an error."}].
sha1_empty_cookie_file(_Config) ->
  Challenge = dbus_hex:encode(<<"org_freedesktop_general 0 1">>),
  meck:expect(file, read_line, [{[fd], eof},
                                {['_'], meck:passthrough()}]),

  Error = dbus_auth_cookie_sha1:challenge(Challenge, waiting_challenge),

  ?assertEqual({error, no_cookie}, Error).

sha1_malforme_cookie() ->
  [{doc, "Given the cookie file read, when a cookie line is malformed, then return an error."}].
sha1_malforme_cookie(_Config) ->
  Challenge = dbus_hex:encode(<<"org_freedesktop_general 0 1">>),
  meck:expect(file, read_line, [{[fd], {ok, <<"0 10 00 other">>}},
                                {['_'], meck:passthrough()}]),

  Error = dbus_auth_cookie_sha1:challenge(Challenge, waiting_challenge),

  ?assertEqual({error, no_cookie}, Error).

sha1_unmatch_cookie() ->
  [{doc, "Given no cookie matching the request, when the cookie are read, then retunr an error."}].
sha1_unmatch_cookie(_Config) ->
  Challenge = dbus_hex:encode(<<"org_freedesktop_general 0 1">>),
  meck:expect(file, read_line, [{[fd], meck:seq([{ok, <<"1 10 00">>}, eof])},
                                {['_'], meck:passthrough()}]),

  Error = dbus_auth_cookie_sha1:challenge(Challenge, waiting_challenge),

  ?assertEqual({error, no_cookie}, Error).
