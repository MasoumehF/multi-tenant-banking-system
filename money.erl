-module(money).
-export([start/1]).

start(Args) ->
    CustomerFile = lists:nth(1, Args),
    BankFile = lists:nth(2, Args),
    {ok, CustomerList} = file:consult(CustomerFile),
    {ok, BankList} = file:consult(BankFile),

    MasterPid = self(),
    register(master, MasterPid),

    BankPids = lists:map(
      fun({BankName, Amount}) ->
          Pid = spawn(bank, init, [BankName, Amount]),
          maybe_register(BankName, Pid),
          {BankName, Amount}
      end, BankList),

    io:format("~n** The financial market is opening for the day **~n"),
    io:format("~nStarting transaction log...~n~n"),

    % Pass the same registered BankPids to customer
    lists:foreach(
      fun({CustName, Objective}) ->
          spawn(customer, init, [CustName, Objective, BankPids])
      end, CustomerList),

    loop(CustomerList, BankPids, []).

loop(Customers, Banks, Logs) ->
    receive
        {request_log, CustName, BankName, Amount} ->
            io:format("? ~p requests a loan of ~p dollar(s) from the ~p bank~n",
                      [CustName, Amount, BankName]),
            loop(Customers, Banks, Logs);

        {reply_log, BankName, CustName, approved, Amount} ->
            io:format("$ The ~p bank approves a loan of ~p dollar(s) to ~p~n",
                      [BankName, Amount, CustName]),
            loop(Customers, Banks, Logs);

        {reply_log, BankName, CustName, denied, Amount} ->
            io:format("$ The ~p bank denies a loan of ~p dollar(s) to ~p~n",
                      [BankName, Amount, CustName]),
            loop(Customers, Banks, Logs);

        {done, CustName, Objective, Received} ->
            NewLogs = [{CustName, Objective, Received} | Logs],
            if length(NewLogs) =:= length(Customers) ->
                print_summary(NewLogs, Banks);
               true -> loop(Customers, Banks, NewLogs)
            end
    end.

print_summary(CustomerLogs, Banks) ->
    io:format("~n** Banking Report **~n~nCustomers:~n"),
    TotalObjective = lists:foldl(fun({_, O, _}, Acc) -> O + Acc end, 0, CustomerLogs),
    TotalReceived = lists:foldl(fun({_, _, R}, Acc) -> R + Acc end, 0, CustomerLogs),
    lists:foreach(
      fun({Name, Obj, Recv}) ->
          io:format("~p: objective ~p, received ~p~n", [Name, Obj, Recv])
      end, CustomerLogs),
    io:format("-----~nTotal: objective ~p, received ~p~n", [TotalObjective, TotalReceived]),

    io:format("~nBanks:~n"),
    lists:foreach(
      fun({BankName, Original}) ->
          Balance = bank:get_balance(BankName),
          io:format("~p: original ~p, balance ~p~n", [BankName, Original, Balance])
      end, Banks),
    TotalBank = lists:foldl(fun({_, O}, Acc) -> O + Acc end, 0, Banks),
    io:format("-----~nTotal: original ~p, loaned ~p~n", [TotalBank, TotalReceived]),
    io:format("~n~nThe financial market is closing for the day...~n~n").

maybe_register(Name, Pid) ->
    case whereis(Name) of
        undefined -> register(Name, Pid);
        _ -> ok
    end.
