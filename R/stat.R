### Statistics ---------------------------------------------------




#' Incremental average calculation
#' 
#' Online algorithm for updating a given average on new value
#' TODO: check for numerical stability
#'
#' @param avg     The current average value
#' @param cnt     The count of elements before the new value
#' @param val     The new value
#' @param na.rm   Whether to remove NA values
#'
#' @return        The updated average
#' @export
avg_update <- function(avg, cnt, val, na.rm = TRUE) {
    if (na.rm)
        res <- ifelse(is.na(val), avg, avg + (val - avg) / (cnt + 1))  # ((avg_old * cnt) + val) / (cnt + 1)
    else
        res <- avg + (val - avg) / (cnt + 1)
    res
}



# Vectorized Pairwise standard deviation calculation
# Not that it would not be possible to do that with sd(c(x, y)) since that would only work pair-wise for single numbers, not vectors
pair.sd <- function(x, y) {
    abs(x - y) * 0.70710678118654746   # abs(x-y) / sqrt(2)  or  # sqrt( (x - y)^2 / 2.0 )
}




#' Incremental Standard deviation calculation
#' 
#' Online algorithm for updating a given std when a new value is added
#' http://stats.stackexchange.com/questions/24878/computation-of-new-standard-deviation-using-old-standard-deviation-after-change
#' Algorithms for calculating variance : http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
#' TODO: check for numerical stability
#'
#' @param stdev    The current standard deviation
#' @param avg      The current average value
#' @param cnt      The number of elements before the new value
#' @param val      The new value
#' @param na.rm    Whether to remove NA values
#'
#' @return         The new standard deviation
#' @export
sd_update <- function(stdev, avg, cnt, val, na.rm = TRUE) {
    res   <- rep(NA_real_, times = length(val))
    res   <- ifelse(cnt > 1,
                    sqrt(((cnt - 1) / cnt) * (stdev^2) + ((val - avg)^2) / (cnt + 1)),
                    ifelse(cnt == 1,
                           pair.sd(avg, val),
                           NA_real_))
    if (na.rm)
        res <- ifelse(is.na(val), stdev, res)
    res
}


