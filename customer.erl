-module(customer).
-export([init/3]).

init(Name, Objective, Banks) ->
    timer:sleep(200),
    rand:seed(exsplus, {erlang:monotonic_time(), erlang:phash2(Name), Objective}),
    loop(Name, Objective, 0, Banks).

loop(Name, 0, Received, _) ->
    master ! {done, Name, Received, Received};

loop(Name, Remaining, Received, []) ->
    master ! {done, Name, Remaining + Received, Received};

loop(Name, Remaining, Received, Banks) ->
    timer:sleep(rand:uniform(91) + 9),
    MaxReq = min(50, Remaining),
    Amount = rand:uniform(MaxReq),
    BankTuple = lists:nth(rand:uniform(length(Banks)), Banks),
    {BankName, _} = BankTuple,
    BankPid = whereis(BankName),

    case BankPid of
        undefined ->
            % Skip bank and try remaining
            NewBanks = lists:filter(fun({N, _}) -> N =/= BankName end, Banks),
            loop(Name, Remaining, Received, NewBanks);

        _ ->
            BankPid ! {loan_request, self(), Name, Amount},
            master ! {request_log, Name, BankName, Amount},
            receive
                {loan_reply, BankName, approved, Amount} ->
                    loop(Name, Remaining - Amount, Received + Amount, Banks);
                {loan_reply, BankName, denied, Amount} ->
                    NewBanks = lists:filter(fun({N, _}) -> N =/= BankName end, Banks),
                    loop(Name, Remaining, Received, NewBanks)
            end
    end.
