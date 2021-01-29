
#' Barplot portraying the chat activity by weekday
#'
#' @param data tibble provided by the `prep_data()` function
#'
#' @returnggplot2 object
#' 
#' @import ggplot2
#' @import dplyr
#' @author Emanuel Sommer
plot_weekday_activity <- function(data) {
  data %>%
    mutate(wday_ord = ordered(wday,
                              levels = c("2", "3", "4", "5", "6", "7", "1"),
                              labels = c("Monday", "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday",
                                         "Sunday"))) %>%
    group_by(wday_ord) %>%
    summarize(n_mess = n(),
              n_words = sum(n_words), 
              .groups = 'drop') %>%
    mutate(mess_rel = n_mess / nrow(data),
           words_rel = n_words / sum(data$n_words)) %>%
    select(wday_ord, mess_rel, words_rel) %>%
    tidyr::pivot_longer(-wday_ord,
                        names_to = "type",
                        values_to = "rel_value") %>%
    ggplot(aes(x = wday_ord, y = rel_value, fill = type)) +
    geom_col(position = "dodge") +
    scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday",
                                "Thursday", "Friday", "Saturday",
                                "Sunday")) +
    scale_fill_manual(name = "",
                      values = c("#58E370", "#3C252B"),
                      labels = c("Messages", "Words")) +
    labs(x = "", y = "Relative frequency") + 
    theme_classic() +
    theme(legend.position = "bottom")
}




#' Barplot displaying the chat activity for a specific day of the week
#'
#' @param data tibble provided by the `prep_data()` function
#' @param days character vector of weekdays ("Monday", "Tuesday", "Wednesday",
#'  "Thursday", "Friday", "Saturday", "Sunday")
#'
#' @return ggplot2 object
#' 
#' @import dplyr
#' @import ggplot2
#' @author Emanuel Sommer
plot_day_activity <- function(
  data, 
  days = c("Monday", "Tuesday", "Wednesday",
          "Thursday", "Friday", "Saturday",
          "Sunday")) {
  valid_days <- c("Sunday", "Monday", "Tuesday", "Wednesday",
                  "Thursday", "Friday", "Saturday")
  checkmate::assert_character(days, any.missing = FALSE,
                              unique = TRUE, max.len = 7)
  stopifnot(all(days %in% valid_days))
  num_day <- vapply(days, function(day) {
    which(day == valid_days)
  }, FUN.VALUE = numeric(1))
  
  data %>%
    mutate(hour = as.factor(lubridate::hour(time))) %>%
    filter(wday %in% num_day) %>%
    group_by(hour) %>%
    summarize(n_mess = n(),
              n_words = sum(n_words), 
              .groups = 'drop') %>%
    mutate(mess_rel = n_mess / nrow(data),
           words_rel = n_words / sum(data$n_words)) %>%
    select(hour, mess_rel, words_rel) %>%
    tidyr::pivot_longer(-hour,
                        names_to = "type",
                        values_to = "rel_value") %>%
    ggplot(aes(x = hour, y = rel_value, fill = type)) +
    geom_col(position = "dodge") +
    scale_x_discrete(limits = paste(0:23)) +
    scale_fill_manual(name = "",
                      values = c("#58E370", "#3C252B"),
                      labels = c("Messages", "Words")) +
    labs(x = "Hour of the day", y = "Relative frequency") + 
    theme_classic() +
    theme(legend.position = "bottom")
}


# barplot total words by users

# barplot total messages by user

# boxplots emojis/message and words/message x= users

# plot barchart flipped top k emojis (author, k in [3,20]?)