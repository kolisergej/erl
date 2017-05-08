-module(server).
-behavior(gen_server).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).

%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
  gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats)->
  if
    Cats =:= [] ->
      io:fwrite("empty ~w~n", [Cats]),
      {reply, make_cat(Name, Color, Description), Cats};

    Cats =/= [] -> % got to empty the stock
      io:fwrite("non empty ~w~n", [Cats]),
      {reply, hd(Cats), tl(Cats)}
  end;

handle_call(terminate, _From, Cats) ->
  io:fwrite("handle call terminate~n"),
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats)->
  {noreply, [Cat | Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unexcpected message: ~p~n", [Msg]),
  {noreply, Cats}.

terminate(normal, Cats) ->
  io:fwrite("terminate~n"),
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Private functions
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.

