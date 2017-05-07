-module(evserv).
-export([
  start/0,
  start_link/0,
  terminate/0,
  subscribe/1,
  add_event/3,
  cancel/1,
  listen/1
]).

-record(events, {
  name="",
  description="",
  pid,
  timeout={{1970,1,1},{0,0,0}}
}).

-record(state, {
  events,
  clients
}).

start() ->
  register(?MODULE, Pid = spawn(fun() -> init() end)),
  Pid.

start_link() ->
  register(?MODULE, Pid = spawn_link(fun() -> init() end)),
  Pid.

terminate() ->
  ?MODULE ! shutdown.

init()->
  loop(#state{
    events = orddict:new(),
    clients = orddict:new()
  }).

loop(S = #state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Pid),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});

    {Pid, MsgRef, {add, Name, Description, Timeout}} ->
      case valid_datetime(Timeout) of
        true ->
          EventPid = event:start_link(Name, Timeout),
          NewEvents = orddict:store(Name, #events{
            name = Name,
            description = Description,
            pid = EventPid,
            timeout = Timeout
          }, S#state.events),
          Pid ! {MsgRef, ok},
          loop(S#state{events=NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          loop(S)
      end;


    {Pid, Ref, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
        {ok, E} ->
          event:cancel(E#events.pid),
          orddict:erase(Name, S#state.events);
        error ->
          S#state.events
      end,
      Pid ! {Ref, ok},
      loop(S#state{events=Events});

    {done, Name} ->
      case orddict:find(Name, S#state.events) of
        {ok, E} ->
          io:format("Subscriber found, sending ~w~n", [E#events.name]),
          send_to_clients({done, E#events.name, E#events.description}, S#state.clients),
          NewEvents = orddict:erase(Name, S#state.events),
          loop(S#state{events=NewEvents});
        error ->
          loop(S)
      end;

    shutdown ->
      exit(shutdown);

    {'DOWN', Ref, _Pid, _Reason} ->
      loop(S#state{clients = orddict:erase(Ref, S#state.clients)});

    code_change ->
      ?MODULE:loop(S);

    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)
  end.

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause ->
      false
  end;

valid_datetime(_) -> false.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when H >= 0, H < 24,
                          M >= 0, M < 60,
                          S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.

send_to_clients(Msg, Clients) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, Clients).

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Description, Timeout)->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
  receive
    {Ref, Msg} ->
      Msg
  after 5000 ->
    {error, timeout}
  end.

cancel(Name)->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} ->
      ok
    after 5000 ->
      {error, timeout}
  end.

listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay*1000 ->
    []
  end.
