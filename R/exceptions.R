### Error Control -------------------------------------------------------



# Run an expression with Error recovery and Logging support
# http://stackoverflow.com/a/24884348/1305020
# Ren withJavaLogging : withErrorControl
# changed "calls" to "call" at: trace = limitedLabels(c(calls, attr(obj, "call")))
# changed "> 0" to "> 1" and "trace[length(trace):1]" to "trace[(length(trace) - 1):1]"
#     at : if (!simpleMessage && length(trace) > 1) {
#              trace = trace[(length(trace) - 1):1]
# Resources
#     http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/index.shtml
#     http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/pmd.shtml



#' Error recovery and logging wrapper
#'
#' Run expression with error recovery and logging support
#'
#' @param expr            The expression to run
#' @param silentSuccess   Whether to hide messages on success
#' @param stopIsFatal     Whether to stop the process when an error occurs
#'
#' @return                Whatever the expression returns
#' @export
#'
withErrorControl = function(expr, silentSuccess = FALSE, stopIsFatal = TRUE) {
    hasFailed = FALSE
    messages = list()
    warnings = list()
    logger = function(obj) {
        # Change behaviour based on type of message
        level = sapply(class(obj), switch, debug = "DEBUG", message = "INFO", warning = "WARN", caughtError = "ERROR",
                       error = if (stopIsFatal) "FATAL" else "ERROR", "")
        level = c(level[level != ""], "ERROR")[1]
        simpleMessage = switch(level, DEBUG=,INFO=TRUE, FALSE)
        quashable     = switch(level, DEBUG=,INFO=,WARN=TRUE, FALSE)

        # Format message
        time  = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
        txt   = conditionMessage(obj)
        if (!simpleMessage) txt = paste(txt, "\n", sep = "")
        msg = paste(time, level, txt, sep = " ")
        calls = sys.calls()
        calls = calls[1:(length(calls) - 1)]

        trace = limitedLabels(c(calls, attr(obj, "call")))

        if (!simpleMessage && length(trace) > 1) {
            trace = trace[(length(trace) - 1):1]
            msg = paste(msg, "  ", paste("at", trace, collapse = "\n  "), "\n", sep = "")
        }

        # Output message
        if (silentSuccess && !hasFailed && quashable) {
            messages <<- append(messages, msg)
            if (level == "WARN") warnings <<- append(warnings, msg)
        } else {
            if (silentSuccess && !hasFailed) {
                cat(paste(messages, collapse = ""))
                hasFailed <<- TRUE
            }
            cat(msg)
        }

        # Muffle any redundant output of the same message
        optionalRestart = function(r) { res = findRestart(r); if (!is.null(res)) invokeRestart(res) }
        optionalRestart("muffleMessage")
        optionalRestart("muffleWarning")
    }
    vexpr = withCallingHandlers(withVisible(expr),
                                debug = logger, message = logger, warning = logger, caughtError = logger, error = logger)
    if (silentSuccess && !hasFailed) {
        cat(paste(warnings, collapse = ""))
    }
    if (vexpr$visible) vexpr$value else invisible(vexpr$value)
}








