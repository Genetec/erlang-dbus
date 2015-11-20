%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  26 Jan 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(dbus_client_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-include("dbus.hrl").

-define(SCRIPT, "example-service.py").
-define(SERVICE, <<"net.lizenn.dbus.SampleService">>).
-define(IFACE, <<"net.lizenn.dbus.SampleInterface">>).

%%%
%%% CT callbacks
%%%
-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%%%
%%% Test cases
%%%
-export([
         connect_system/1,
         connect_session/1,
         connect_service/1,
         walk_node/1,
         interface/1,
         call_method/1,
         signal_all/1,
         signal/1
        ]).

-define(dbus_session_tcp_anonymous, 
		{"session_tcp_anonymous.conf", "tcp:host=localhost,bind=*,port=55555,family=ipv4"}).
-define(dbus_session_unix_anonymous, 
		{"session_unix_anonymous.conf", "unix:path=/tmp/dbus-test"}).
-define(dbus_session_unix_external,
		{"session_unix_external.conf", "unix:path=/tmp/dbus-test"}).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
	application:ensure_all_started(dbus),
    Config.

end_per_suite(Config) ->
    application:stop(dbus),
	Config.

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     {group, connect_tcp_anonymous}
	,{group, connect_unix_anonymous}
	,{group, connect_unix_external}
    ,{group, service}
    ].


groups() ->
    [
     {connect, [parallel, {repeat, 5}], [ connect_system, connect_session ]}
	,{connect_tcp_anonymous, [], [ connect_session ]}
	,{connect_unix_anonymous, [], [ connect_session ]}
	,{connect_unix_external, [], [ connect_session ]}
    ,{service, [parallel, {repeat, 1}], [
                                         connect_service
                                        ,walk_node
                                        ,interface
                                        ,call_method
                                        ,signal_all
                                        ,signal
                                        ]}
    ].


init_per_group(connect, Config) ->
    Config;
init_per_group(connect_tcp_anonymous, Config) ->
	start_dbus(Config, ?dbus_session_tcp_anonymous);
init_per_group(connect_unix_anonymous, Config) ->
	start_dbus(Config, ?dbus_session_unix_anonymous);
init_per_group(connect_unix_external, Config) ->
	start_dbus(Config, ?dbus_session_unix_external);
init_per_group(_Name, Config) ->
	Config0 = start_dbus(Config, ?dbus_session_unix_external),
	ServicePath = get_data_path(?SCRIPT, Config0),
    Pid = spawn(fun () -> os:cmd(ServicePath) end),
    [ {service_pid, Pid}, {connect, true} | Config0 ].


end_per_group(connect, _Config) ->
	{return_group_result, ok};
end_per_group(Group, Config) 
  when Group =:= connect_tcp_anonymous;
	   Group =:= connect_unix_anonymous;
	   Group =:= connect_unix_external ->
    stop_dbus(Config),
	{return_group_result, ok};
end_per_group(_Name, Config) ->
	exit(?config(service_pid, Config), kill),
	stop_dbus(Config),
	{return_group_result, ok}.


init_per_testcase(_, Config) ->
    case ?config(connect, Config) of
        true ->
            {ok, Bus} = dbus_bus_connection:connect(session),
            [ {bus, Bus} | Config ];
        _ -> Config
    end.

end_per_testcase(_, Config) ->
    case ?config(connect, Config) of
        true -> dbus_connection:close(?config(bus, Config));
        _ -> ok
    end,
    ok.

%%%
%%% Test cases
%%%
connect_session(_Config) ->
	?assertEqual(["/tmp/dbus-test"], filelib:wildcard("/tmp/dbus-test")),
    {ok, Bus} = dbus_bus_connection:connect(session),
    ?assertMatch(ok, dbus_bus_connection:close(Bus)).

connect_system(_Config) ->
    {ok, Bus} = dbus_bus_connection:connect(system),
    ?assertMatch(ok, dbus_bus_connection:close(Bus)).

connect_service(Config) ->
	?assertEqual(["/tmp/dbus-test"], filelib:wildcard("/tmp/dbus-test")),
    {ok, Service} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE),
    ?assert(is_pid(Service)),
    ok.

walk_node(Config) ->
    {ok, S} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE),
    [CPath] = dbus_proxy:children(S),
    ?assertMatch(<<"/root">>, CPath),
    {ok, Child} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, CPath),
    ?assertMatch([<<"/root/child2">>, <<"/root/child1">>], dbus_proxy:children(Child)),
    ok.

interface(Config) ->
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    ?assertMatch(true, dbus_proxy:has_interface(O, ?IFACE)),
    ?assertMatch(false, dbus_proxy:has_interface(O, <<"toto">>)),
    ok.        

call_method(Config) ->
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    ?assertMatch({ok,[<<"Hello World">>,<<" from example-service.py">>]},
                 dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, ["plop"])),
    ?assertMatch({error, {'org.freedesktop.DBus.InvalidParameters', _}},
                 dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, [])),
    ?assertMatch({error, {'org.freedesktop.DBus.UnknownMethod', _}},
                 dbus_proxy:call(O, ?IFACE, <<"bad_method">>, [])),
    ?assertMatch({error, {'org.freedesktop.DBus.UnknownInterface', _}},
                 dbus_proxy:call(O, <<"net.lizenn.dbus.BadInterface">>, <<"HelloWorld">>, [])),
    ok.

signal_all(Config) ->
    Fun = fun (_Sender, IfaceName, <<"SampleSignal">>, Path, _Args, Pid) ->
                  ?debug("### Received signal: SampleSignal"),
                  ?assertMatch({?IFACE, <<"/root">>},
                               {IfaceName, Path}),
                  Pid ! got_signal;
              (_Sender, IfaceName, <<"SampleSignal2">>, Path, _Args, Pid) ->
                  ?debug("### Received signal: SampleSignal2"),
                  ?assertMatch({?IFACE, <<"/root">>},
                               {IfaceName, Path}),
                  Pid ! got_signal2
          end,
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    dbus_proxy:connect_signal(O, {Fun, self()}),
    dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, ["plop"]),
    Wait = fun([], _F) ->
                   ok;
              ([ Signal | Tail ], F) ->
                   receive Signal -> F(Tail, F)
                   after 100 -> ?assert(false)
                   end
           end,
    Wait([ got_signal, got_signal2 ], Wait),
    ok.


signal(Config) ->
    Fun = fun (_Sender, IfaceName, <<"SampleSignal">>, Path, _Args, Pid) ->
                  ?assertMatch({?IFACE, <<"/root">>},
                               {IfaceName, Path}),
                  Pid ! got_signal;
              (_Sender, ?IFACE, <<"SampleSignal2">>, _Path, _Args, Pid) ->
                  Pid ! got_signal2
          end,
    {ok, O} = dbus_proxy:start_link(?config(bus, Config), ?SERVICE, <<"/root">>),
    dbus_proxy:connect_signal(O, ?IFACE, <<"SampleSignal">>, {Fun, self()}),
    dbus_proxy:call(O, ?IFACE, <<"HelloWorld">>, ["plop"]),
    Wait = fun ([], [], _F) ->
                   ok;
               ([ Signal | Signals ], [], F) ->
                   receive Signal -> F(lists:delete(Signal, Signals), [], F)
                   after 100 -> ?assert(false)
                   end;        
               ([], [ BadSignal | BadSignals ], F) ->
                   receive BadSignal -> ?assert(false)
                   after 100 -> F([], BadSignals, F)
                   end;        
               ([ Signal | Signals ], [ BadSignal | BadSignals ], F) ->
                   receive Signal -> F(lists:delete(Signal, Signals), [ BadSignal | BadSignals ], F);
                           BadSignal -> ?assert(false)
                   after 100 -> F([ Signal | Signals ], BadSignals, F)
                   end
           end,
    Wait([got_signal], [got_signal2], Wait),
    ok.


%%%
%%% Priv
%%%
get_data_path(Path, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, Path]).

start_dbus(Config, {DBusConfig, DBusEnv}) ->
	File = get_data_path(DBusConfig, Config),
	Pid = spawn(fun() -> os:cmd("dbus-daemon --config-file=" ++ File) end),
	os:putenv("DBUS_SESSION_BUS_ADDRESS", DBusEnv),
	[ {dbus, Pid}, {dbus_env, DBusEnv} | Config ].

stop_dbus(Config) ->
	exit(?config(dbus, Config), kill),
	proplists:delete(dbus_env, 
					 proplists:delete(dbus, Config)).
