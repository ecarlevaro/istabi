Error in eval(parse_only(code), envir = envir) : Calls: .main ...
.handleSimpleError -> h -> <Anonymous> -> cnd_signal -> signal_abort
Traceback:
22: stop(fallback)
21: signal_abort(cnd)
20: cnd_signal(entraced)
19: rlang::entrace(e)
18: h(simpleError(msg, call))
17: .handleSimpleError(function (e) 
    if (xfun::pkg_available("rlang", "1.0.0")) rlang::entrace(e), 
        "object 'NFIXPARAMS' not found", base::quote(eval(parse_only(code), 
            envir = envir)))
16: eval(parse_only(code), envir = envir)
15: eval(parse_only(code), envir = envir)
14: withVisible(eval(parse_only(code), envir = envir))
13: hook_eval(code[i], envir)
12: inline_exec(block)
11: in_dir(input_dir(), expr)
10: in_input_dir(inline_exec(block))
9: call_inline(x)
8: process_group(group)
7: withCallingHandlers(if (tangle) process_tangle(group) else process_group(group), 
       error = function(e) if (xfun::pkg_available("rlang", "1.0.0")) rlang::entrace(e))
6: xfun:::handle_error(withCallingHandlers(if (tangle) process_tangle(group) else process_group(group), 
       error = function(e) if (xfun::pkg_available("rlang", "1.0.0")) rlang::entrace(e)), 
       function(loc) {
           setwd(wd)
           write_utf8(res, output %n% stdout())
           paste0("\nQuitting from lines ", loc)
       }, if (labels[i] != "") sprintf(" [%s]", labels[i]), get_loc)
5: process_file(text, output)
4: knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet)
3: rmarkdown::render(input = input, output_format = output_format, 
       knit_root_dir = knit_root_dir, params = params, run_pandoc = FALSE, 
       envir = env)
2: execute(params$input, params$format, params$tempDir, params$libDir, 
       params$dependencies, params$cwd, params$params, params$resourceDir, 
       params$handledLanguages, params$markdown)
1: .main()
