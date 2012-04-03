-module(datamill_reclaimer_ssh_server).

-behaviour(ssh_channel).

%% API
-export([start/0]).

%% Callbacks
-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2, code_change/3]).


-include("datamill_reclaimer_config.hrl").
-record(state, {cm, channel, pending :: binary()}).


%%=============================================================================
%% API
%%=============================================================================

start() ->
    ok = crypto:start(),
    ok = ssh:start(),

    ok = do_ensure_ssh_key({?PATH_FILE__SSH_KEY,    ?OS_CMD__SSH_KEY_GEN}),
    ok = do_ensure_ssh_key({?PATH_FILE__SSH_HOSTKEY,?OS_CMD__SSH_HOSTKEY_GEN}),

    SubsysOpts = [],
    SubsysName = "ssh_subsystem@datamill_reclaimer",
    SubsysSpec = {SubsysName, {?MODULE, [SubsysOpts]}},

    DaemonIP   = {127,0,0,1},
    DaemonPort = 22222,
    DaemonOpts = [
        {system_dir, ?PATH_DIR__DATA_SSH},
        {user_dir,   ?PATH_DIR__DATA_SSH},
        {subsystems, [SubsysSpec]},
        {nodelay,    true}
    ],

    ssh:daemon(DaemonIP, DaemonPort, DaemonOpts).


%%=============================================================================
%% Callbacks
%%=============================================================================

%%-----------------------------------------------------------------------------
init(_Options) ->
    {ok, #state{pending = <<>>}}.


%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
handle_ssh_msg({ssh_cm, _ConMgr, {data, _ChanID, 0, Data}}, State) ->
    io:format("***** BEGIN DATA: ~n~p~n ***** END DATA ~n", [Data]),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConMgr, {eof, ChanID}}, State) ->
    {stop, ChanID, State};

handle_ssh_msg({ssh_cm, _ConMgr, {signal, _, _}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _ConMgr, {exit_signal, ChanID, _, Error, _}}, State) ->
    ok = io:format("Connection closed by peer ~n Error ~p~n", [Error]),
    {stop, ChanID, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChanID, 0}}, State) ->
    {stop, ChanID, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChanID, Status}}, State) ->
    ok = io:format("Connection closed by peer ~n Status ~p~n", [Status]),
    {stop, ChanID, State}.


%%-----------------------------------------------------------------------------
handle_msg({ssh_channel_up, ChanID, ConMgr}, State) ->
    {ok,  State#state{channel=ChanID, cm=ConMgr}}.


%%-----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%=============================================================================
%% Internal
%%=============================================================================

do_ensure_ssh_key({Path, Command}) ->
    do_ensure_ssh_key(key_exists, filelib:is_file(Path), Path, Command).


do_ensure_ssh_key(key_exists, true, _, _) ->
    ok;
do_ensure_ssh_key(key_exists, false, Path, Command) ->
    do_ensure_ssh_key(key_dir, filelib:ensure_dir(Path), Command).


do_ensure_ssh_key(key_dir, ok, Command) ->
    do_ensure_ssh_key(key_gen, datamill_reclaimer_lib:os_cmd(Command));
do_ensure_ssh_key(key_dir, Error, _) ->
    {error, Error}.


do_ensure_ssh_key(key_gen, {ok,   _Output}) ->
    ok;
do_ensure_ssh_key(key_gen, {error, Reason}) ->
    {error, Reason}.
