#' prep the data after loading
#' 
#' removes unnecessary and adds useful columns to the input data
#' 
#' Remove:
#' -  source column
#' 
#' Add:
#' - `n_words` word count per message
#' - `n_emojis` emoji count per message
#' - `wday` weekday (integer, 1 corresponds to Sunday) 
#'
#' @param rwa_data output tibble of `rwhatsapp::rwa_read()`
#'
#' @return augmented tibble (remove: source, add: n_words, n_emojis, wday)
#' @export
#' 
#' @import dplyr
#' @author Emanuel Sommer / Eleftheria Papavasileiou
prep_data <- function(rwa_data) {
  # input checks
  rwa_data <- try(select(rwa_data, "time", "author", "text",
                         "source", "emoji", "emoji_name"))
  checkmate::assert_data_frame(rwa_data)
  
  # remove messages from NA authors
  rwa_data <- rwa_data[!is.na(rwa_data$author),]
  # remove source and add useful columns
  rwa_data %>%
    select(-source) %>%
    mutate(
      n_words = stringr::str_count(text, pattern = "\\W+"), # count words
      n_emojis = purrr::map_dbl(emoji, length), # count emojis
      wday = lubridate::wday(time) # numeric (1 is Sunday)
    ) %>%
    arrange(time)
}


  
  