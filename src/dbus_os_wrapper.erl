-module(dbus_os_wrapper).
-export([getenv/1]).

getenv(Env) ->
  os:getenv(Env).
