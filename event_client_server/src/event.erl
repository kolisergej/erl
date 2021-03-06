-module(event).
-export([start/2, start_link/2, cancel/1]).
-record(state, {
  server,
  name="",
  to_go=0
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%Interfaces%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(EventName, DateTime) ->
  spawn(fun() -> init(self(), EventName, DateTime) end).

start_link(EventName, DateTime) ->
  spawn_link(fun() -> init(self(), EventName, DateTime) end).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, _Reason} ->
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%Implementaion%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Server, EventName, DateTime) ->
  io:format("Started ~p event process, timeout: ~w~n", [EventName, DateTime]),
  loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
    after T * 1000 ->
      if
        Next =:= [] ->
          io:format("~p event process timeout~n", [S#state.name]),
          Server ! {done, S#state.name};
        Next =/= [] ->
          loop(S#state{to_go=Next})
      end
  end.

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(Timeout={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(Timeout) -
    calendar:datetime_to_gregorian_seconds(Now),
  Secs = if
    ToGo > 0 ->
      ToGo;
    true ->
      0
    end,
  normalize(Secs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
