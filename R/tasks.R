# Helpers for defining tasks and tasklists
# Companion to my cronr package




#' Add a task to TaskList
#'
#' Add a task to our Tasks structure
#'
#' @param TaskList  An existing list to add tasks to
#' @param schedule  A shorthand period to recurrently run this task
#' @param FUN       The function to run
#' @param args      Optional arguments to FUN
#' @param method    Whether to run in the parent process or in a parallel R process
#' 
#' @return          The updated TaskList list 
#' 
#' @export
task_add <- function(TaskList, schedule, FUN, args, method = "fork") {
    task <- list(schedule = schedule, job = FUN)
    if(! missing(args))
        task$args <- args
    task$method <- method
    TaskList[[length(TaskList) + 1]] <- task
    TaskList
}



#' Change a task in a TaskList
#'
#' Change the definition of a Task and return the updated TaskList
#'
#' @param task_id   The id of the task you want to change
#' @param TaskList  An existing TaskList list
#' @param schedule  A shorthand period to recurrently run this task
#' @param FUN       The function to run
#' @param args      Optional arguments to FUN
#' @param method    Whether to run in the parent process or in a parallel R process
#' 
#' @return          The updated TaskList list 
#' 
#' @export
task_change <- function(task_id, TaskList, schedule, FUN, args, method) {
    if(missing(task_id)) {
        warning("you have to supply a task_id to this function")
        return(TaskList)
    }
    task <- TaskList[[task_id]]
    if(! missing(schedule)) task$schedule <- schedule
    if(! missing(FUN))      task$job      <- FUN
    if(! missing(args))     task$args     <- args
    if(! missing(method))   task$method   <- method
    TaskList
}



#' Extract the schedules vector
#'
#' Get the schedules defined in a TaskList
#'
#' @param TaskList   An existing list which has some defined schedules
#' @param fieldName  The fieldName which holds the schedules
#' 
#' @return           The vector of schedules
#' 
#' @export
task_list_schedules <- function(TaskList, fieldName = "schedule") {
    sapply(TaskList, "[[", fieldName)
}




#' Reset a task list
#'
#' Reset a task list - just return an empty list
#'
#' @param TaskList   The TaskList list to reset
#' 
#' @return           The empty list
#' 
#' @export
task_list_reset <- function(TaskList) {
    list()
}




#' A TaskList handler
#'
#' A task handler which supports TaskList lists
#'
#' @param task_id    The id of the task to "handle"
#' @param timestamp  The timestamp this task was scheduled to run
#' @param TasksInfo  The list (TaskList) with the task metadata
#' 
#' @return           Depends on the function run by the task and the method used (internal/fork)
#' 
#' @export
task_handler <- function(task_id, timestamp, TasksInfo, ...) {
    #message("Running task: ", task_id, " at:", as.numeric(Sys.time()), " - scheduled:", timestamp)
    task <- TasksInfo[[task_id]]
    job  <- task$job
    if(! is.function(job) && ! is.function(get0(job)) && ! exists(as.character(quote(job)), mode = "function", where = sys.frame(-1L))) {
        warning("task_handler: Function ", as.character(quote(job)), " for task ", task_id, " does not exist.")
        #return(NULL)
    }

    args <- list()
    if(!is.null(task$args)) {
        args <- task$args
    }
    args <- lapply(args, function(x) x <- if(x %in% c("%timestamp", "%task_id")) get(sub("%", "", x)) else x)

    if(!is.null(task$method) && task$method %in% c("fork", "mcparallel", "systemR", "tcl")) {
        task_fork(job, args, task$method)
    } else {
        task_run_internal(job, args)
    }
}




# Run a task in the same process - but with error traceback and recovery
task_run_internal <- function(job, args, method = "internal") {
    try(withErrorControl({
        do.call(job, args)
    }, stopIsFatal = FALSE))
}





# Fork a task in a parallel R process
task_fork <- function(job, args, method = "mcparallel") {
    if(method == "tcl" && requireNamespace("tcltk2")) {
        env <- parent.frame()
        f <- function() { x <- do.call(job, args, envir = env); x }
        p <- tcltk2::tclAfter(1, f)
        # p <- tcltk2::tclTaskSchedule(1000, function() eval(do.call(job, args), envir = env), id = as.character(quote(job)), redo = FALSE)  # scoping..
    } else if(method == "systemR") {
        Rbin <- file.path(R.home("bin"), "R")
        argstr  <- paste(names(args), args, sep = " = ", collapse = ", ")
        cmd <- paste0(Rbin, " --slave --vanilla -e ", '"', job, "(", argstr, ")" )
        system(cmd, intern = FALSE, wait = FALSE)
    } else {
        f <- function() { x <- do.call(job, args); x }
        # mc.interactive = NA : inherit the interactive flag from the parent process
        p <- parallel::mcparallel(expr = f(),
                                  name = as.character(quote(job)),
                                  silent = FALSE, mc.interactive = FALSE, detached = TRUE)
        parallel::mccollect(wait = FALSE, timeout = 0, intermediate = FALSE)
    }
}





# Just a dummy function for testing task_fork
task_test <- function(x, y, ...) {
    message(sum(x, y, ...))
    sum(x, y, ...)
}











### Forking helpers ------------------------------------------




#' Zombie processes killer-function generator
#'
#' Return a C function compiled with the inline package to kill zombie processes
#' ref: http://stackoverflow.com/a/25393438/1305020
#'
#' @export
make_fun_kill_zombies <- function() {
    if(!requireNamespace("inline"))
        return(NULL)
    includes <- '#include <sys/wait.h>'
    code     <- 'int wstat; while (waitpid(-1, &wstat, WNOHANG) > 0) {};'
    fun      <- inline::cfunction(body = code, includes = includes, convention = '.C')
    return(fun)
}







# Experimental forking function modelled after mcparallel
my_fork <- function(job, args, silent = FALSE, detached = FALSE) {
    f <- parallel:::mcfork(estranged = detached)
    env <- parent.frame()
    if (inherits(f, "masterProcess")) {
        on.exit(parallel:::mcexit(1L, structure("fatal error in wrapper code", class = "try-error")))
        if (isTRUE(silent))
            parallel:::closeStdout(TRUE)
        if (detached) {
            on.exit(parallel:::mcexit(1L))
            do.call(job, args, envir = env)
            parallel:::mcexit(0L)
        }
        parallel:::sendMaster(try(do.call(job, args, envir = env), silent = FALSE))
        parallel:::mcexit(0L)
    }
    f$name <- as.character(quote(job))[1L]
    class(f) <- c("parallelJob", class(f))
    f
}



# Experimental forking function
my_fork2 <- function(job, args, silent = FALSE, detached = FALSE) {
    if(! exists(".ForkCL")) {
        .ForkCL <<- parallel::makeForkCluster(nnodes = 1, timeout = 10800, outfile = "")
        parallel::setDefaultCluster(.ForkCL)
    }
    env <- parent.frame()
    f <- function() { x <- do.call(job, args, envir = env); print(x) }
    parallel::clusterCall(fun = f)
}









