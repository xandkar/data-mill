-module(dmill_stacker).
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
    Timestamp = timestamp(),
    Name    = binary_to_list(proplists:get_value(<<"name">>, Reaper)),
    Command = binary_to_list(proplists:get_value(<<"command">>, Reaper)),
    OutputRaw = os:cmd(Command),
    FilePath = filename:join(QueueDir, Name++"--"++Timestamp++".out"),
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

    Format = "~4.10.0B-~2.10.0B-~2.10.0B--~2.10.0B:~2.10.0B:~2.10.0B.~.10.0B",
    Fields = [Year, Month, Day, Hour, Minute, Second, MicroSecond],
    io_lib:format(Format, Fields).
