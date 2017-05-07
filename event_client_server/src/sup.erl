-module(sup).
-export([start/2, start_link/2]).

start(Mod, Args) ->
  spawn(fun() -> init({Mod, Args}) end).

start_link(Mod, Args) ->
  spawn_link(fun() -> init({Mod, Args}) end).

init({Mod, Args}) ->
  process_flag(trap_exit, true),
  loop({Mod, start_link, Args}).

loop({M, F, A})->
  Pid = apply(M, F, A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown);
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
      loop({M, F, A})
  end.
