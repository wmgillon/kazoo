%% -*- coding: utf-8 -*-
-module(sup_complete).

%% sup_complete: autocomplete SUP commands

-export([ print/1
        , new/1
        , complete/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API

new (SUP) ->
    Sup = group_args(SUP),
    G = digraph:new([acyclic, private]),
    root = digraph:add_vertex(G, root, modules(SUP)),
    lists:foreach(
      fun ({{M, _F}=MF, Asz}) ->
             %% MF={M,F} : Uniquely represent a function
              Vs = [M, MF, Asz],
              _1 = [V = digraph:add_vertex(G, V) || V <- Vs],
              _2 = lists:foldl(fun (V,PV) -> digraph:add_edge(G, PV, V), V end, root, Vs)
                  %% ,io:format("done with ~s:~s\n", [M,_F])
                  %% ,io:format("_1 ~p\n_2 ~p\n", [_1,_2])
      end, Sup),
    %% io:format("Vs: ~p\nEs: ~p\n", [digraph:vertices(G), digraph:edges(G)]),
    G.

print (G) ->
    {root, Ms} = digraph:vertex(G, root),
    io:format("Ms ~p\n", [Ms]),
    lists:foreach(
      fun (M) ->
              io:format("~s\n", [M]),
              MFs = digraph:out_neighbours(G, M),
              lists:foreach(
                fun ({_,F}=MF) ->
                        io:format("\t~s\n", [F]),
                        [Asz] = digraph:out_neighbours(G, MF),
                        lists:foreach(fun (As) -> io:format("\t\t~p\n", [As]) end, Asz)
                end, MFs)
      end, Ms).


-type path() :: [module() | atom() | [string()]].
-spec complete(digraph(), path()) ->
                      {ok, {module(), atom(), [string()]}} |
                      {more, path()} |
                      {error, _}.

complete (G, Key) ->
    complete(G, Key, [root]).
complete (G, [], [root]=Path) -> %% Modules
    {more, more(G, Path)};
complete (G, [V|Vs], [root]=Path) ->
    Ms = more(G, Path),
    NextPath = Path ++ [V],
    case Vs =:= [] of
        true  -> {more, more(G, NextPath)};
        false ->
            case lists:member(V, Ms) of
                false -> {error, {no_such_module,V}};
                true  -> complete(G, Vs, NextPath)
            end
    end;
complete (G, [V|Vs], [root,M]) ->  %% Functions
    MF = {M, V},
    case digraph:vertex(G, MF) of
        false -> {error, {no_such_function,M,V}};
        _Exists -> complete(G, Vs, [root,M,MF])
    end;
complete (G, W, [root,M,{M,F}]=Path) ->  %% Asz :: plural of As (= Arguments)
    Vs = case W of %%FIXME
             [] -> [];
             [ActualVs] -> ActualVs
         end,
    Asz = more(G, Path),
    case split(Vs, Asz) of
        error -> {error, {wrong_arguments,Vs}};
        [] -> {ok, {M,F,Vs}};
        AszLeft -> {more, AszLeft}
    end.

%% Internals

split (As, Asz) -> split(length(As), Asz, []).
split (NAs, [As|Asz], Acc) ->
    try lists:nthtail(NAs, As) of
        []   -> split(NAs, Asz, Acc);
        Rest -> split(NAs, Asz, [Rest|Acc])
    catch error:function_clause -> error
    end;
split (_, [], Acc) -> Acc.

more (G, [root]) ->  %% Ms
    lists:usort(digraph:out_neighbours(G, root));
more (G, [root,M]) ->  %% Fs
    lists:sort([F || {_,F} <- digraph:out_neighbours(G, M)]);
more (G, [root,_,{_,_}=MF]) ->  %% Asz
    case digraph:out_neighbours(G, MF) of
        [] -> [];
        [Asz] -> lists:sort(Asz)
    end;
more (G, [root,M,F]) ->
    more(G, [root,M,{M,F}]).

group_args (SUP) ->
    Dict = lists:foldl(
             fun ({M,F,As}, D) ->
                     dict:append({M,F}, As, D)
             end, dict:new(), SUP),
    [{MF,lists:usort(Asz)} || {MF,Asz} <- dict:to_list(Dict)].

modules (SUP) ->
    lists:usort([M || {M,_,_} <- SUP]).

%% Tests

-ifdef(TEST).

sup () ->
    [ {m1, f, ["A"]}
    , {m1, f, ["B"]}
    , {m2, f, []}
    , {m3, g, ["A", "B"]}
    , {m3, g, ["Bla", "foo", "Quux"]}
    ].

sup_test_ () ->
    G = new(sup()),
    [ ?_assertEqual({more, [m1,m2,m3]}, complete(G, []))
    , ?_assertEqual({more, [f]}, complete(G, [m1]))
    , ?_assertEqual({more,[["Bla","foo","Quux"],["A","B"]]}, complete(G, [m3,g]))
    , ?_assertEqual({more,[["foo","Quux"],["B"]]}, complete(G, [m3,g,["bar"]]))
    ].

-endif.

%% End of Module.
