# Main processing functions

#' Late runner
#'
#' Run an analytical pipeline in scheduled batches
#'
#' @param retriever        The function which is going to retrieve new data
#' @param batch_process    The function responsible for processing this batch
#' @param forward_process  The function which will handle post-processing tasks
#' @param ...              Extra arguments to be passed to the handler
#'
#' @examples
#' # Call crond with late() as the main handler and 
#' # passing extra arguments (late handlers)
#' crond(c("5m:10s"), my_tasks, "callback", "late", 
#'       "retrieve_dt", "batch_process", "forward")
#'
#' @export
late <- function(schedules, retriever, batch_process, forward_process, ...) {
	retriever     <- match.fun(retriever)
	batch_process <- match.fun(batch_process)
	retriever     <- match.fun(forward_process)
	
	# Start retriever
	retriever(...)
	# Start batch_process
	batch_process(...)	
	# Start forward_process
	forward_process(...)
	return(1)
}
