-module(datamill_reclaimer_lib).

-export(
    [
        os_cmd/1
    ]
).


%%%============================================================================
%%% API
%%%============================================================================

os_cmd(Command) ->
    PortOptions = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
    PortID = open_port({spawn, Command}, PortOptions),
    {ExitCode, Output} = pickup_port_output(PortID, []),

    case ExitCode of
        0 -> {ok,    Output};
        _ -> {error, Output}
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

pickup_port_output(PortID, DataAcc) ->
    receive
        {PortID, {data, Data}} ->
            pickup_port_output(PortID, [Data|DataAcc]);

        {PortID, eof} ->
            port_close(PortID),
            receive
                {PortID, {exit_status, ExitCode}} ->
                    Output = lists:flatten(lists:reverse(DataAcc)),
                    {ExitCode, Output}
            end
    end.
