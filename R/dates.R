### DATES -----------------------------------------------------
# Copyright 2013 - 2014, Ilias Kotinas, <henfiber at gmail com>


# set 3 digits for milliseconds - default is not set which means 0
options(digits.secs = 3)
options(lubridate.fasttime = TRUE)




### Helpers --------------------------------------------------------------------

#' as.POSIXct wrapper
#'
#' as.POSIXct wrapper to convert seconds since epoch to datetime class
#'
#' @param x  The integer which stores the seconds
#' 
#' @return   A POSIXct object
#' 
#' @export
from_seconds <- function(x) {
    as.POSIXct(x, origin="1970-01-01")
}




#' Extended options for the floor_date function
#'
#' Currently just added Workday
#'
#' @param x               The datetime object
#' @param unit            The extended unit (including workday) to pass in floor_date
#' @param workday_bounds  The workday time-of-day bounds to use
#' 
#' @return  A datetime object
#' 
#' @export
#' @importFrom lubridate floor_date hour hours seconds minutes  days weeks years
floor_date_extended <- function (x, unit = c("second", "minute", "hour",
                                             "Workday", "quarter",
                                             "day", "week",
                                             "Month", "year"),
                                 workday_bounds=c(8,20)) {
    if (!length(x)) return(x)
    unit <- tolower(match.arg(unit))
    if(unit == "workday") {
        dur <- workday_bounds[2] - workday_bounds[1]
        start = workday_bounds[1]
        # hours_to_shift : ((h-8) %/% 12) * 12 + 8  --  membership: ((h+4) %/% 12) %% 2
        h <- ((hour(x) - start) %/% dur) * dur + start
        floor_date(x, "day") + hours(h)
    } else
        floor_date(x, unit)
}





#' Extended period function
#'
#' Currently just added Workday
#'
#' @param num             How many units of that period
#' @param units           Which time unit 
#' @param ...             The dots are passed to period
#' @param workday_bounds  The workday time-of-day bounds to use
#' 
#' @return  A period object
#' 
#' @export
#' @importFrom lubridate period
period_extended <- function (num = NULL, units = "second", ..., workday_bounds = c(8,20)) {
    if(units == "Workday") {
        dur <- workday_bounds[2] - workday_bounds[1]
        units = "hours"
        num   = num * dur
    }
    units <- tolower(units)
    period(num, units, ...)
}







### TZ Wrappers -----------------------------------------------------------

# Get the current time - converted in UTC
now_utc <- function() {
    lubridate::now(tzone = "UTC")
}

# Return the current hour in UTC (minutes and seconds are truncated)
now_hour_utc <- function() {
    lubridate::floor_date(now_utc(), "h")   #as.POSIXct(trunc(as.POSIXlt(Sys.time(), tz = "UTC"), "hours"))
}

# Return the current day in UTC (hours, minutes and seconds are truncated)
today_utc <- function() {
    lubridate::today(tzone = "UTC")
}

# Return the previous day (hours, minutes and seconds are truncated)
yesterday <- function(tz = "") {
    lubridate::today(tz) - lubridate::days(1)
}

# Return the previous day in UTC (hours, minutes and seconds are truncated)
yesterday_utc <- function() {
    lubridate::yesterday(tz = "UTC")
}












### Date abbreviations -------------------------------------------------------------------------


#' Expand abbreviated period expression
#'
#' Expand a shorthand abbreviation like "-1m" to a list with a sign, value and unit
#'
#' @param abbr            The abbreviated period expression
#' 
#' @return   A list with the sign, value, unit, expanded unit and expression
#' 
#' @export
expand_rel_time <- function(abbr) {
    abbr <- gsub(" ", "", abbr, fixed = TRUE) # remove spaces
    sign <- ""
    if (isTRUE(grepl("^[+-]", abbr))) {
        sign <- substr(abbr, 1, 1)
        abbr <- substring(abbr, 2)
    }

    match_value <- regexpr("^[0-9]+[.]?[0-9]*", abbr)
    if (length(regmatches(abbr, match_value)) > 0) {
        value <- as.integer(regmatches(abbr, match_value)[[1]])
        unit  <- paste(regmatches(abbr, match_value, invert = T)[[1]], collapse = "")
    } else {
        unit  <- abbr
        value <- 1
    }

    # We cannot use tolower(unit) since the same letter "m" is used for both minutes and Months
    unit_expanded <- character(1)
    if (unit == "m")                     {
        unit_expanded <- "minutes"
    } else if (unit %in% c("h", "H"))    {
        unit_expanded <- "hours"
    } else if (unit %in% c("s", "S"))    {
        unit_expanded <- "seconds"
    } else if (unit %in% c("ms", "MS"))  {
        unit_expanded <- "milliseconds"
    } else if (unit %in% c("d", "D"))    {
        unit_expanded <- "days"
    } else if (unit == "M")              {
        unit_expanded <- "months"
    } else if (unit %in% c("y", "Y"))    {
        unit_expanded <- "years"
    } else if (unit %in% c("w", "W"))    {
        unit_expanded <- "weeks"
    } else if (unit %in% c("ns", "NS"))  {
        unit_expanded <- "nanoseconds"
    } else
        warning(paste0("time unit \"", unit, "\" not supported for expansion"))

    return(
        list(
            sign = sign,
            value = value,
            unit = unit,
            unit_expanded = unit_expanded,
            expr_expanded = paste0(sign, value, " ", unit_expanded)
        )
    )
}








### Ranges -----------------------------------------------------------------------


#' Range formatter - seconds
#'
#' Format timestamps in range to integer seconds
#'
#' @param r  The range list with datetime objects
#' 
#' @return   The new range with integers expressing the seconds
#' 
#' @export
fmt_range_seconds <- function(r) {
    lapply(r, function(x)  {
        if("character" %in% class(x))
            as.numeric(lubridate::parse_date_time(x, "y-m-d H:M:S"))
        else
            as.numeric(x)
        })
}


#' Print a range
#'
#' Compactly print a time range
#'
#' @param r  The range list to print
#' 
#' @export
print_range <- function(r) {
    paste(lapply(r, function(x) {
        if(class(x) == "integer" || class(x) == "numeric")
            format(from_seconds(x), format = "%Y-%m-%d %H:%M:%OS3 %Z")
        else
            format(x, format = "%Y-%m-%d %H:%M:%OS3 %Z")
    }), collapse = " - ")
}





#' Construct Ranges of datetimes
#'
#' @param tc         The primary period to use (for recent time)
#' @param N          How many units of that period
#' @param what       Whether to produce a seasonal range sequence or "recent" period range
#' @param sc         The seasonal period (only used if what = "seasonal")
#' @param complete   Whether to truncate the reference time to the start of the period (i.e. it is complete)
#' @param tz         Optionally define a custom timezone
#' @param anchor     The reference time to use (now by default)
#'
#' @return           a single range (recent period) or sequence of ranges (seasonal)
#'
#' @export
#' @importFrom lubridate now floor_date hour hours seconds minutes  days weeks years
range_recent_tc <- function(tc = c("hour", "sec", "min", "Workday", "day", "week", "Month", "quarter", "year"),
                            N=24,
                            what = c("recent", "seasonal"),
                            sc = c("day", "hour", "Workday", "week", "Month", "quarter", "year"),
                            complete = TRUE,
                            tz = "UTC",
                            anchor,
                            right_open = TRUE) {
    tc   <- match.arg(tc) # time context
    sc   <- match.arg(sc) # seasonal context
    what <- match.arg(what)

    # Set anchor
    if(missing(anchor) || is.null(anchor)) anchor <- lubridate::now(tz)
    if(complete) anchor <- floor_date_extended(anchor, tc)

    if(what == "recent") {
        from <- switch (tc,
                "sec"     = anchor - seconds(N),
                "min"     = anchor - minutes(N),
                "hour"    = anchor - hours(N),
                "day"     = anchor - days(N),
                "Workday" = anchor - hours(12*N),
                "week"    = anchor - weeks(N),
                "Month"   = anchor - months(N),
                "quarter" = anchor - months(3*N),  # ((month(anchor) - 1) %/% 3) * 3 + 1
                "year"    = anchor - years(N)
        )
        to <- if(right_open) anchor - seconds(0.001) else anchor
        return(list(gte = from, lte = to))
    } else {
        to <- switch (sc,
                "sec"     = anchor - seconds(N:0),
                "min"     = anchor - minutes(N:0),
                "hour"    = anchor - hours(N:0),
                "day"     = anchor - days(N:0),
                "Workday" = anchor - hours((12*N):0),
                "week"    = anchor - weeks(N:0),
                "Month"   = anchor - months(N:0),
                "year"    = anchor - years(N:0)
        )
        from = to - period_extended(1, tc)
        if(right_open)
            to <- to - seconds(0.001)
        return(lapply(1:length(from), function(i) list(gte = from[i], lte = to[i])))
    }
}





# Get the range for the last complete N tc periods
range_last_tc <- function(tc = c("h", "m", "d", "w", "M", "y"), N = 1, anchor, complete = TRUE) {
    tc <- match.arg(tc)

    # Set anchor
    if(missing(anchor) || is.null(anchor)) anchor <- lubridate::now()
    if(complete) anchor <- floor_date_extended(anchor, tc)
    now_h <- anchor
    switch (tc,
        "m" = list(gte = (now_h - lubridate::minutes(N)), lte = now_h),
        "h" = list(gte = (now_h - lubridate::hours(N)),   lte = now_h),
        "d" = list(gte = (now_h - lubridate::days(N)),    lte = now_h),
        "w" = list(gte = (now_h - lubridate::weeks(N)),   lte = now_h),
        "M" = list(gte = (now_h - lubridate::months(N)),  lte = now_h),
        "y" = list(gte = (now_h - lubridate::years(N)),   lte = now_h)
    )
}






#' Get range from shorthand expression
#'
#' Get a range back from a period expression (e.g. "24h") and an anchor timestamp
#'
#' @param period_expr  The shorthand period expression
#' @param anchor       The reference time to use
#'
#' @return             The range constructed from this expression
#'
#' @export
range_from_expr <- function(period_expr = "720h", anchor = NULL) {
    # expand_rel_time(period_expr) returns a list with names: unit, value, unit_expanded, sign
    with(expand_rel_time(period_expr), {
        range_recent_tc(unit, N = value, anchor = anchor)
    })
}




#' Ranges from datetime vector
#'
#' Get a list of ranges out of a vector with POSIXct objects
#'
#' @param timevector  The vector of datetime objects
#' @param tc          the time unit to use for constructing the interval
#'
#' @return            The list of ranges 
#'
#' @export
ranges_from_dates <- function(timevector, tc = "hour") {
	# TODO: support other time-contexts than just hour - see compute_relative
    lapply(timevector, function(r) list(gte = r[1],
                                        lte = r[1] + hours(1) - seconds(0.001)))
}





