-module(datamill_stacker).
-export([main/1, do_harvest/2]).


-define(TIMEOUT, 5000).
-define(SUBSYSTEM, "ssh_subsystem@datamill_reclaimer").
-define(PATH_DIR__HOME,     os:getenv("HOME")).
-define(PATH_DIR__DATA,     filename:join([?PATH_DIR__HOME, ".data-mill"])).
-define(PATH_DIR__DATA_SSH, filename:join([?PATH_DIR__DATA, "ssh"])).


%%-----------------------------------------------------------------------------
main([PathToReapersConfigJSON|_]) ->
    {Reapers, QueueDir} = get_reapers(file:read_file(PathToReapersConfigJSON)),

    ok = filelib:ensure_dir(filename:join(QueueDir, "dummyfile")),
    ok = crypto:start(),
    ok = ssh:start(),

    Harvesters = [
        spawn_monitor(?MODULE, do_harvest, [R, QueueDir]) || R <- Reapers
    ],

    wait_for_completions(Harvesters).


%%-----------------------------------------------------------------------------
do_harvest({Reaper}, QueueDir) ->
    {ok, Hostname} = inet:gethostname(),
    Name    = binary_to_list(proplists:get_value(<<"name">>, Reaper)),
    Command = binary_to_list(proplists:get_value(<<"command">>, Reaper)),

    Timestamp = timestamp(),
    OutputRaw = os:cmd(Command),

    FileName = string:join([Hostname, Name, Timestamp], "--")++".out.gz",
    FilePath = filename:join(QueueDir, FileName),

    ok = file:write_file(FilePath, OutputRaw, [compressed]),

    OutputStructBin = term_to_binary({FileName, OutputRaw}, [{compressed, 9}]),

    do_send(OutputStructBin).


%%-----------------------------------------------------------------------------
do_send(Data) ->
    ServerAddr = "127.0.0.1",
    ServerPort = 22222,
    ConnectOptions = [
        {user_dir, ?PATH_DIR__DATA_SSH},
        {nodelay, true},
        {silently_accept_hosts, true}
    ],

    {ok, ConnRef} = ssh:connect(ServerAddr, ServerPort, ConnectOptions),
    {ok, ChannId} = ssh_connection:session_channel(ConnRef, infinity),
    success = ssh_connection:subsystem(ConnRef, ChannId, ?SUBSYSTEM, ?TIMEOUT),
    ok = ssh_connection:send(ConnRef, ChannId, 0, Data, ?TIMEOUT).


%%-----------------------------------------------------------------------------
wait_for_completions(Harvesters) ->
    wait_for_completions(Harvesters, 0).

wait_for_completions([], ExitCode) -> halt(ExitCode);
wait_for_completions(Harvesters, ExitCode) ->
    receive
        {'DOWN', Ref, _, PID, normal} ->
            wait_for_completions(lists:delete({PID, Ref}, Harvesters), ExitCode);

        {'DOWN', Ref, _, PID, Info} ->
            io:format("~p~n", [Info]),
            wait_for_completions(lists:delete({PID, Ref}, Harvesters), ExitCode+1)
    end.


%%-----------------------------------------------------------------------------
get_reapers({ok, ReapersConfigJSON}) ->
    try ejson:decode(ReapersConfigJSON) of
        {ReapersConfig} ->
            QueueDir = proplists:get_value(<<"queue_dir">>, ReapersConfig),
            Reapers  = proplists:get_value(<<"reapers">>, ReapersConfig),
            {Reapers, binary_to_list(QueueDir)}
    catch
        Error ->
            {error, io_lib:format("COULD NOT DECODE JSON: ~p~n", [Error])}
    end.


%%-----------------------------------------------------------------------------
timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = {date(), time()},
    {_, _, MicroSecond} = now(),
    Fields = [Year, Month, Day, Hour, Minute, Second, MicroSecond],

    Decimal4 = "~4.10.0B",  % 4-digit decimal number
    Decimal2 = "~2.10.0B",  % 2-digit decimal number
    DecimalX = "~.10.0B",   % any-number-of-digits decimal number
    SepDate = "-",
    SepTime = ":",

    FormatDate = string:join([Decimal4, Decimal2, Decimal2], SepDate),
    FormatTime = string:join([Decimal2, Decimal2, Decimal2], SepTime),

    Format = FormatDate++"--"++FormatTime++"."++DecimalX,

    io_lib:format(Format, Fields).
