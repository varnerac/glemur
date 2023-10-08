-module(cover_ffi).

-export([compile_beam_directory/1, analyse_to_file/0, imported_modules/0]).

compile_beam_directory(Dir) ->
    DirStr = binary_to_list(Dir),
    CompileResults = cover:compile_beam_directory(DirStr),
    io:format(standard_error, "Cover compiled ~p~n", [CompileResults]),
    CompileResults2 =
        lists:map(fun(X) ->
                     case X of
                         {ok, _} -> ok;
                         {already_cover_compiled, no_beam_found, _} -> ok;
                         _ -> error
                     end
                  end,
                  CompileResults),
    case lists:uniq(CompileResults2) of
        [ok] ->
            {ok, nil};
        _ ->
            {error, nil}
    end.
analyse_to_file() ->
    CoverageDir = "build/coverage/",
    file:del_dir(CoverageDir),
    file:make_dir(CoverageDir),
    Result = cover:export(CoverageDir ++"glemur.coverdata"),
    cover:analyse_to_file([html, {outdir, CoverageDir}]),
    cover:analyse_to_file([{outdir, CoverageDir}]),
    case Result of
        ok ->
            {ok, nil};
        _ ->
            {error, nil}
    end.

imported_modules() ->
    [atom_to_binary(IM) || IM <- cover:imported_modules()].
