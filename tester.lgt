:- initialization((
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(unknown_entities, silent),
    logtalk_load(lgtunit(loader)),
    % logtalk_load('tests', [hook(lgtunit)]),
    logtalk_load('expr_tests', [hook(lgtunit)]),
    expr::run
)).