# Mischellaneous utility functions that do not fit anywhere else
# Copyright 2013 - 2014, Ilias Kotinas, <henfiber at gmail com>



### FILES -----------------------------------------------------

#' Check if input is directory
#'
#' Check if input is a directory
#'
#' @param x  The name of the file system object to check
#' 
#' @return Logical (TRUE) if x is a directory
#' 
#' @export
is_directory <- function(x) file.info(x)$isdir




#' Check if readable
#'
#' Check if file system object x is readable
#'
#' @param x  The name of the file system object to check
#' 
#' @return Logical (TRUE) if x is readable
#' 
#' @export
is_readable  <- function(x) file.access(x, 4) == 0




#' Check if directory is leaf directory
#'
#' Check if directory is a leaf directory, i.e. has not descendant directories
#'
#' @param d  The directory to check
#' 
#' @return Logical (TRUE) if directory d is a leaf
#' 
#' @export
dir.isLeaf <- function(d) {
    length(list.dirs(d, recursive = FALSE, full.names = FALSE)) == 0
}



#' Check if directory has files
#'
#' Check if the directory has any files recursively
#'
#' @param d          The directory to check
#' @param recursive  Whether to check recursively or just in the first level
#' 
#' @return Logical (TRUE) if directory d has any file
#' 
#' @export
dir.hasFiles <- function(d, recursive = TRUE) {
    files <- list.files(d, recursive = recursive, full.names = TRUE, include.dirs = FALSE, all.files = TRUE)
    for(f in files)
        if(! file.info(f)$isdir) return(TRUE)
    return(FALSE)
}




#' Remove empty directories
#'
#' Remove empty directories under a specific path
#'
#' @param dpath  The path under which this function should look for empty directories
#' 
#' @export
dir.removeEmpty <- function(dpath) {
    dirs <- list.dirs(dpath, full.names = TRUE, recursive = TRUE)
    dirs <- dirs[sapply(dirs, function(x) dir.isLeaf(x))]
    dirs <- dirs[sapply(dirs, function(x) !dir.hasFiles(x))]

    if(length(dirs) > 0) {
        #message("The following dirs will be deleted: ", paste(dirs, collapse = ", "))
        unlink(dirs, recursive = T)
    }
}




#' Check if file is recent
#'
#' Check if file was modified less than interval seconds
#'
#' @param fpath     The file to check
#' @param interval  The interval in seconds the file may have changed within
#' 
#' @return Logical (TRUE) if file has recently changed
#' 
#' @export
file_modified_less_than <- function(fpath, interval = 60) {
    if(! file.exists(fpath)) 
		return(invisible(NULL))
    secs_before <- as.integer(difftime(Sys.time(), file.mtime(fpath), units = "secs"))
    return(secs_before < interval)
}




