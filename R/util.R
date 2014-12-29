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








#' Robust saveRDS
#'
#' A saveRDS with atomic write and backup support
#'
#' @param dt                     The object to save
#' @param fpath                  The path to save the object
#' @param backup_on_overwrite    Whether to keep a backup when overwriting an existing file
#' @param allowZero              Whether to save zero length objects
#' @param ...                    The dots are passed to saveRDS
#'
#' @return                       TRUE if successful, a negative value if it fails
#' @export
#'
saveRDS_robust <- function(dt, fpath, backup_on_overwrite = TRUE, allowZero = FALSE, ...) {

    if(missing(dt) || is.null(dt))
        return(invisible(-1))
    if(!allowZero && is.data.frame(dt) && nrow(dt) == 0) {
        warning("The input object has 0 rows. Aborting. Run with allowZero = TRUE to allow writing a zero records object.")
        return(invisible(-1))
    }

    # Not supporting vectors. We are expecting a single file path here
    fpath <- fpath[1]

    # Define temporary (atomic update) and backup path (just to be safe)
    tmppath <- paste0(fpath, ".tmp")
    bakpath <- paste0(fpath, ".bak")


    # Take a backup of the previous model if one exists
    if(backup_on_overwrite && file.exists(fpath)) {
        file.copy(fpath, bakpath, overwrite = TRUE)
    }

    # Save the frame to a temporary file
    saveRDS(dt, tmppath, ...)

    # Now we can replace the old model with the new one in an atomic (?) rename
    if(file.rename(tmppath, fpath))
        return(invisible(TRUE))
    else {
        warning("Could not properly save the object. Check ", fpath, ".", call. = TRUE, immediate. = TRUE)
        file.remove(tmppath)
        return(invisible(-2))
    }
}





#' Robust readRDS
#'
#' Read an RDS file with some checking and restoring from a backup
#'
#' @param fpath                 Where to get the rds file from
#' @param restore_from_backup   Whether to restore from backup and replace a corrupt file
#'
#' @return                      The object or NULL if the read fails
#' 
#' @export
readRDS_robust <- function(fpath, restore_from_backup = TRUE) {

    # Not supporting vectors. We are expecting a single file path here
    fpath <- fpath[1]
    existing <- file.exists(fpath)

    if(existing)
        dt <- tryCatch(readRDS(fpath), error = function(w) w)

    if (!existing || "simpleError" %in% class(dt)) {
        warning("Could not load the object from file : ", fpath, ". ",
                "Testing if a backup file exists.")

        bakpath <- paste0(fpath, ".bak")
        if(!file.exists(bakpath)) {
            warning("Could not find a backup file named: ", bakpath)
            return(invisible(NULL))
        } else {
            dt <- tryCatch(readRDS(bakpath), error = function(w) w)

            if ("simpleError" %in% class(dt)) {
                warning("An attempt to load the object from backup file : ", bakpath,
                        " also failed! Check ", fpath, ".", immediate. = TRUE, call. = TRUE)
                return(invisible(NULL))
            }

            message("Loading from backup file : ", bakpath, " was successful!")

            # Restore from backup
            if(restore_from_backup) {
                if(!file.copy(bakpath, fpath, overwrite = TRUE))
                    warning("Restoring the original file from the backup failed. Check ", fpath)
            }

            return(invisible(dt))
        }
    }
    invisible(dt)
}










