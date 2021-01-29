
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
                      values = c("#58E370", "#DE793B"),
                      labels = c("Messages", "Words")) +
    labs(x = "", y = "Relative frequency") + 
    theme_classic() +
    theme(legend.position = "bottom")
}



# density plots (weekday, size_kernel) active chatting

# barplot total words by users

# barplot total messages by user

# boxplots emojis/message and words/message x= users

# plot barchart flipped top k emojis (author, k in [3,20]?)