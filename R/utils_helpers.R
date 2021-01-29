#' Get Largest streak from POSIXct vector
#' 
#' i.e. most consecutive days in a row (Implementation is quick and dirty
#'  probably not the most efficient one, one pass through unique days)
#'
#' @param time_vec POSIXct vector (sorted, length > 1 and no missing values)
#'
#' @return numeric value the largest streak
#'
#' @author Emanuel Sommer
get_largest_streak <- function(time_vec) {
  checkmate::assert_posixct(time_vec, any.missing = FALSE, sorted = TRUE, min.len = 2)
  unique_days_num <- unique(
    as.numeric(as.Date(time_vec))
  )
  longest_streak <- 0
  current_streak <- 1
  for (i in 2:length(unique_days_num)) {
    if ((unique_days_num[i] - unique_days_num[i - 1]) == 1) {
      current_streak <- current_streak + 1
    } else {
      current_streak <- 1
    }
    if (current_streak > longest_streak) {
      longest_streak <- current_streak
    }
  }
  longest_streak
}
