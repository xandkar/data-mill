-module(datamill_stacker).
-export([main/1, do_harvest/2]).


%%-----------------------------------------------------------------------------
main([PathToReapersConfigJSON|_]) ->
    {Reapers, QueueDir} = get_reapers(file:read_file(PathToReapersConfigJSON)),

    ok = filelib:ensure_dir(filename:join(QueueDir, "dummyfile")),

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

    FileName = string:join([Hostname, Name, Timestamp], "--")++".out",
    FilePath = filename:join(QueueDir, FileName),
    ok = file:write_file(FilePath++".gz", OutputRaw, [compressed]).


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
