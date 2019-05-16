-module(ping_pong).
-author("bluvalor").

%% API
-export([start/0, stop/0, play/1, start_board/2, ping/1, pong/0]).


ping(X) ->
  receive
    {play, 0, _}   -> timer:sleep(500), io:format("ping ~B, total: ~B~n", [0, X]), ping(X);
    {play, N, Pid} -> timer:sleep(500), io:format("ping ~B, total: ~B~n", [N, X + N]), Pid ! {play, N - 1, self()}, ping(X + N);
    stop           -> done
  after
    10000 -> done
  end.


pong() ->
  receive
    {play, 0, _}   -> timer:sleep(500), io:format("pong ~B~n", [0]), pong();
    {play, N, Pid} -> timer:sleep(500), io:format("pong ~B~n", [N]), Pid ! {play, N - 1, self()}, pong();
    stop           -> done
  after
    10000 -> done
  end.


start_board(Ping, Pong) ->
  receive
    {play, N} -> Ping ! {play, N, Pong}, start_board(Ping, Pong);
    stop      -> Ping ! stop, Pong ! stop
  end.


start() ->
  Ping = spawn(?MODULE, ping, [0]),
  Pong = spawn(?MODULE, pong, []),
  register(board_state, spawn(?MODULE, start_board, [Ping, Pong])).


stop() -> board_state ! stop.


play(N) -> board_state ! {play, N}.