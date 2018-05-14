-module(erl2048_lib).

-export([init/1]).
-export([print/1]).
-export([move/2, won_or_lost/1, has_changed/1]).
-export([to_list/1]).
-export([new_tile/1, last_added/1]).

-type coord() :: pos_integer().
-type coords() :: {X :: coord(), Y :: coord()}.
-type tile_value() :: pos_integer().
-type tile() :: null | tile_value().
-type state() :: #{
        coords() => tile(),
        size => pos_integer(),
        last_added => coords(),
        changed => boolean()
       }.

-type move_dir() :: right | left | up | down.

-define(SIZE, 4).
-define(FOUR_TO_TWO_RATIO, 0.1).

%% ----------------------------------------------------------
%% API
%% ----------------------------------------------------------

-spec init(TilePreset :: [ {coords(), tile_value()} ] | undefined) -> state().
init(TilePreset) ->
    Size = ?SIZE,
    Dims = lists:seq(1, Size),
    AllTiles = [ {X, Y} || X <- Dims, Y <- Dims ],
    State0 = #{ size => Size, changed => false, last_added => undefined },
    State =
    lists:foldl(fun(Coord, Board) ->
                        Board#{ Coord => null }
                end, State0, AllTiles),
    case TilePreset of
        undefined ->
            new_tile(new_tile(State));
        _ ->
            lists:foldl(
              fun({ Coords, Value }, StateAcc) ->
                      StateAcc#{ Coords := Value }
              end, State, TilePreset)
    end.

-spec print(state()) -> any().
print(#{ size := Size } = State) ->
    CoordSeq = lists:seq(1, Size),
    FormatString = lists:flatten(lists:duplicate(Size, "~5.w") ++ "~n"),
    lists:foreach(fun(Y) ->
                          Row = [ maps:get({X, Y}, State) || X <- CoordSeq ],
                          io:format(FormatString, Row)
                  end, CoordSeq).

-spec move(state(), move_dir()) -> state().
move(#{ size := Size } = State0, Dir) ->
    State1 = State0#{ changed => false },
    {InitX, XInnerStep, XOuterStep, InitY, YInnerStep, YOuterStep} =
    case Dir of
        right -> { Size, -1, 0, 1, 0, 1 };
        left -> { 1, 1, 0, 1, 0, 1 };
        up -> { 1, 0, 1, 1, 1, 0 };
        down -> { 1, 0, 1, Size, -1, 0 }
    end,
    lists:foldl(fun(N, StateAcc) ->
                        X = InitX + (N - 1) * XOuterStep,
                        Y = InitY + (N - 1) * YOuterStep,
                        move(StateAcc, X, X + XInnerStep, XInnerStep, Y, Y + YInnerStep, YInnerStep)
                end, State1, lists:seq(1, Size)).

-spec won_or_lost(state()) -> won | lost | neither.
won_or_lost(#{ size := Size } = State) ->
    Dims = lists:seq(1, Size),
    AllCoords = [ {X, Y} || X <- Dims, Y <- Dims ],
    case lists:any(fun(Coords) ->
                           Value = maps:get(Coords, State),
                           Value /= null andalso Value >= 128
                   end, AllCoords) of
        true ->
            won;
        false ->
            case get_empty(State) == [] andalso not any_mergable(State) of
                true -> lost;
                _ -> neither
            end
    end.

-spec to_list(state()) -> [{coords(), tile()}].
to_list(#{ size := Size } = State) ->
    lists:flatmap(
      fun(X) ->
              [ {{X, Y}, maps:get({X, Y}, State)} || Y <- lists:seq(1, Size) ]
      end, lists:seq(1, Size)).

-spec new_tile(state()) -> state().
new_tile(State0)  ->
    Empty = get_empty(State0),
    Nth = rand:uniform(length(Empty)),
    Coords = lists:nth(Nth, Empty),
    Value = new_tile_value(),
    State0#{ Coords := Value, last_added => Coords }.

-spec last_added(state()) -> {coords(), tile()}.
last_added(#{ last_added := LastAdded } = State) ->
    {LastAdded, maps:get(LastAdded, State)}.

-spec has_changed(state()) -> boolean().
has_changed(#{ changed := Changed }) ->
    Changed.

%% ----------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------

-spec new_tile_value() -> tile_value().
new_tile_value() ->
    case rand:uniform() < ?FOUR_TO_TWO_RATIO of
        true -> 4;
        false -> 2
    end.

-spec get_empty(state()) -> [coords()].
get_empty(#{ size := Size } = State) ->
    Dims = lists:seq(1, Size),
    lists:filter(fun(Coords) ->
                         maps:get(Coords, State) == null
                 end, [ {X, Y} || X <- Dims, Y <- Dims ]).

-spec move(State :: state(), X :: coord(), XToCheck :: coord(), XStep :: integer(),
           Y :: coord(), YToCheck :: coord(), YStep :: integer()) -> state().
move(#{ size := Size } = State, _X, XToCheck, _XStep, _Y, YToCheck, _YStep)
  when XToCheck < 1; XToCheck > Size; YToCheck < 1; YToCheck > Size ->
    State;
move(State, X, X, XStep, Y, Y, YStep) ->
    move(State, X, X + XStep, XStep, Y, Y + YStep, YStep);
move(State, X, XToCheck, XStep, Y, YToCheck, YStep) ->
    case {maps:get({X, Y}, State), maps:get({XToCheck, YToCheck}, State)} of
        {_, null} ->
            move(State, X, XToCheck + XStep, XStep, Y, YToCheck + YStep, YStep);
        {Val, Val} ->
            NState = State#{ {XToCheck, YToCheck} := null,
                             {X, Y} := Val * 2,
                             changed => true },
            move(NState, X + XStep, X + 2 * XStep, XStep, Y + YStep, Y + 2 * YStep, YStep);
        {null, Val} when Val /= null ->
            NState = State#{ {XToCheck, YToCheck} := null,
                             {X, Y} := Val,
                             changed => true },
            move(NState, X, X + XStep, XStep, Y, Y + YStep, YStep);
        {_, _} ->
            %% {X, Y} and ToCheck are pointing to different, non-null values
            move(State, X + XStep, XToCheck, XStep, Y + YStep, YToCheck, YStep)
    end.

-spec any_mergable(state()) -> boolean().
any_mergable(#{ size := Size } = State) ->
    Dims = lists:seq(1, Size),
    AllCoords = [ {X, Y} || X <- Dims, Y <- Dims ],
    lists:any(fun(Coords) -> has_mergable_neighbour(Coords, State) end, AllCoords).

-spec has_mergable_neighbour(coords(), state()) -> boolean().
has_mergable_neighbour({X, Y} = Coords, State) ->
    Neighbours = [
                  maps:get({X - 1, Y}, State, undefined),
                  maps:get({X + 1, Y}, State, undefined),
                  maps:get({X, Y - 1}, State, undefined),
                  maps:get({X, Y + 1}, State, undefined)
                 ],
    lists:member(maps:get(Coords, State), Neighbours).

