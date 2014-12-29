### Time-oriented Caching ------------------------------------------------------




#' Get cache state for a specific range
#'
#' @param range      A list in the format list(gte = ..., lte = ...) expressing the date range to check in cache
#' @param tc         The sampling period of the cache
#' @param data_path  The data path to look at
#'
#' @return           A list with hits and misses for files and dates
#'
#' @examples
#' cache_state_24h  <- cache_state_range_recent(range = range_recent_tc(N = 24), 
#'                                              tc = "hour", data_path = "data/cache")
#'
#' @export
cache_state_range_recent <- function(range, tc = "hour", data_path = "data/cache") {

    if(missing(range) || is.null(range))
        stop("a specific range is required in cache_list_dates")

    unit_ranges <- seq.POSIXt(range$gte, range$lte, by = tc)

    # Construct paths from date sequence
    paths <- sapply(unit_ranges, function(u) {
        unit_cache <- format.POSIXct(u, "%Y/%m/%d/%H")
        d_dir  <- file.path(data_path, unit_cache)
        d_path <- file.path(d_dir, "dt.rds")
    })

    # Return list of existing files and missing dates
    existing <- file.exists(paths)
    list(files_hit = paths[existing], files_miss = paths[!existing],
         dates_hit = unit_ranges[existing], dates_miss = unit_ranges[!existing])
}











#' Load cache for a specific range
#'
#' @param range 
#' @param data_path 
#' @param tc 
#'
#' @return
#' @export
cache_load <- function(range, data_path = "data/cache", tc = "hour") {
    if(missing(range) || is.null(range))
        stop("a specific range is required in cache_load")

    cache_state <- cache_state_range_recent(range = range, tc = tc, data_path = data_path)
    cache_files <- cache_state$files_hit

    rbindlist(lapply(cache_files, function(x) {
        readRDS_robust(x)
    }), use.names = TRUE, fill = TRUE)
}







#' Remove cache data older than ttl seconds
#'
#' @param ttl            ttl in seconds
#' @param data_path      Where to look for expired cache entries
#' @param archive_path   A path where to copy expired cache files (for archiving)
#' @param remove_empty   Whether to remove empty directories at the end
#'
#' @return               invisible(NULL)
#' @export
cache_purge_expired <- function(ttl = 2592000, data_path = "data/cache", archive_path = NULL, remove_empty = TRUE) {
    cache_dirs <- list.dirs(data_path, full.names = FALSE, recursive = TRUE)
    tnow       <- Sys.time()

    # Get a logical vector of cache dirs that have expired
    expired_dirs <- sapply(cache_dirs, function(x) {
        d_dir    <- file.path(data_path, x)

        # We only want leaf directories
        if(!dir.isLeaf(d_dir))
            return(FALSE)

        # Get the date represented by the path
        cache_dir_date <- tryCatch(strptime(x, "%Y/%m/%d/%H"), error = function(w) w)

        if(!is.na(cache_dir_date) && as.numeric(difftime(tnow, cache_dir_date, units = "secs")) > ttl)
            return(TRUE)
        return(FALSE)
    })
    expired_dirs <- cache_dirs[expired_dirs]
    if(length(expired_dirs) == 0)
        return(invisible(NULL))

    # Now archive and delete expired directories
    sapply(expired_dirs, function(x) {
        d_dir    <- file.path(data_path, x)

        if(!is.null(archive_path)) {
            backup_dir <- file.path(archive_path, x)
            if(!dir.exists(backup_dir))
                tryCatch(dir.create(backup_dir, recursive = TRUE), error = function(w) w)
            tryCatch(file.copy(list.files(path = d_dir, full.names = T), backup_dir, overwrite = TRUE),
                     error = function(w) w)
        }
        tryCatch(unlink(d_dir, recursive = TRUE), error = function(w) w)
        message(d_dir, " has been purged from cache")
    })

    # Remove any left empty directories
    if(remove_empty) {
        sapply(expired_dirs, function(x) {
            d_parent <- file.path(data_path, sub("/[0-9]+$", "", x))
            if(dir.exists(d_parent) && !dir.hasFiles(d_parent)) {
                tryCatch(unlink(d_parent, recursive = TRUE), error = function(w) w)
                message("Empty parent cache directory ", d_parent, " has been deleted")
            }
        })
    }

    invisible(NULL)
}











#' Split dataset on factor and saved parts to files
#'
#' Split dataset DT on subfolders defined by the distinct values of split_field
#'
#' @param DT            The data frame to split
#' @param data_path     The data path to save the parts of the dataset
#' @param split_field   The field name to use for the splitting
#'
#' @return
#' @export
split_data_on_field <- function(DT, data_path = "data/cache", split_field) {
    if(missing(split_field) || is.null(split_field))
        stop("A split_field must be provided")

    levs <- if(class(DT[[split_field]]) == "factor")
        levels(DT[[split_field]]) else unique(DT[[split_field]])

    for(lev in levs) {
        tmp <- droplevels(DT[DT[[split_field]] == lev, ])
        if(nrow(tmp) == 0) next  # skip saving datasets for empty levels

        lev_dir  <- file.path(data_path, lev)
        if(!dir.exists(lev_dir))
            dir.create(lev_dir, recursive = TRUE)
        lev_path <- file.path(lev_dir, "dt.rds")

        message("Saving ", lev_path, " with ",  nrow(tmp), " rows")
        saveRDS_robust(dt = tmp, fpath = lev_path,
                       backup_on_overwrite = FALSE, allowZero = TRUE)
    }
}






#' Split and save dataset by date
#'
#' Split dataset DT on subfolders defined by its date (Y/m/d/H format)
#'
#' @param DT            The data frame to split
#' @param data_path     The data path to save the parts of the dataset
#' @param date_field    The name of the date field to use for splitting
#' @param date_unit     The date unit to use for rounding to complete intervals
#'
#' @return
#' @export
split_data_on_date <- function(DT, data_path = "data/cache", date_field,
                               date_unit = c("hour", "day", "Month",
                                             "year", "week", "minute")) {
    if(missing(date_field) || is.null(date_field))
        stop("A date_field must be provided")

    date_unit <- tolower(match.arg(date_unit))
    dates     <- unique(floor_date(DT[[date_field]], date_unit))
    tmp <- lapply(dates, function(d) {
        #message(d)
        d_dir  <- file.path(data_path, format.POSIXct(d, "%Y/%m/%d/%H"))
        d_path <- file.path(d_dir, "dt.rds")
        tmp <- droplevels(DT[floor_date(DT[[date_field]], date_unit) == d, ])
        if(nrow(tmp) > 0) {
            if(!dir.exists(d_dir))
                dir.create(d_dir, recursive = TRUE)
            message("Saving ", d_path, " with ",  nrow(tmp), " rows")
            saveRDS_robust(dt = tmp, fpath = d_path,
                           backup_on_overwrite = FALSE, allowZero = TRUE)
        }
    })
    invisible(NULL)
}






#' Split and save dataset by date and field
#'
#' Split dataset DT on both a timestamp field and a categorical field
#'
#' @param DT 
#' @param data_path 
#' @param split_field 
#' @param date_field 
#' @param date_unit 
#'
#' @return
#' @export
#'
split_data_on_field_and_date <- function(DT, data_path = "data/cache",
                                         split_field, date_field,
                                         date_unit = c("hour", "day", "Month",
                                                       "year", "week", "minute")) {
    if(missing(split_field) || is.null(split_field))
        stop("A split_field must be provided")
    if(missing(date_field) || is.null(date_field))
        stop("A date_field must be provided")

    date_unit <- tolower(match.arg(date_unit))

    # Get unique dates and field levels
    dates <- unique(floor_date(DT[[date_field]], date_unit))
    levs <- if(class(DT[[split_field]]) == "factor")
        levels(DT[[split_field]]) else unique(DT[[split_field]])

    # Build index for splitting based on field value
    idx_field <- list()
    for(lev in levs) {
        idx_field[[lev]] <- which(DT[[split_field]] == lev)
    }

    # Iterate over distinct dates
    tmp <- lapply(dates, function(d) {
        idx_d <- which(floor_date(DT[[date_field]], date_unit) == d)

        for(lev in levs) {
            idx_both <- intersect(idx_field[[lev]], idx_d)
            if(length(idx_both) > 0) {
                d_dir  <- file.path(data_path, lev, format.POSIXct(d, "%Y/%m/%d/%H"))
                d_path <- file.path(d_dir, "dt.rds")
                if(!dir.exists(d_dir))
                    dir.create(d_dir, recursive = TRUE)
                message("Saving ", d_path, " with ",  length(idx_both), " rows")
                tmp <- droplevels(DT[idx_both, ])
                saveRDS_robust(dt = tmp, fpath = d_path,
                               backup_on_overwrite = FALSE, allowZero = TRUE)
            }
        }
        invisible(NULL)
    })
    invisible(NULL)
}






#' Save the model to a file using a robust function
#'
#' @param dt_model 
#' @param fpath 
#' @param backup_on_overwrite  
#'
#' @return 
#' @export
#'
model_save <- function(dt_model, fpath, backup_on_overwrite = TRUE) {

    res <- saveRDS_robust(dt = dt_model, fpath = fpath, backup_on_overwrite = TRUE, allowZero = FALSE)

    if(!identical(res, TRUE))
        warning("Problem saving the model. Check ", fpath, call. = TRUE, immediate. = TRUE)

    return(invisible(NULL))
}



#' Load the model from a file using readRDS robust
#'
#' @param fpath  the file path to the saved model
#'
#' @return       the model
#' @export
model_load <- function(fpath) {
    dt_model <- readRDS_robust(fpath = fpath, restore_from_backup = TRUE)
    invisible(dt_model)
}






