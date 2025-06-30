-module(bank).
-export([init/2, get_balance/1]).

init(Name, Amount) ->
    loop(Name, Amount).

loop(Name, Balance) ->
    receive
        {loan_request, From, CustName, Amount} ->
            if Amount =< Balance ->
                From ! {loan_reply, Name, approved, Amount},
                master ! {reply_log, Name, CustName, approved, Amount},
                loop(Name, Balance - Amount);
            true ->
                From ! {loan_reply, Name, denied, Amount},
                master ! {reply_log, Name, CustName, denied, Amount},
                loop(Name, Balance)
            end;

        {get_balance, From} ->
            From ! {balance, Balance},
            loop(Name, Balance)

    after 5000 ->
        ok
    end.

get_balance(BankName) ->
    BankPid = whereis(BankName),
    case BankPid of
        undefined ->
            0; % If bank is somehow unregistered
        _ ->
            BankPid ! {get_balance, self()},
            receive
                {balance, B} -> B
            after 1000 ->
                0
            end
    end.
