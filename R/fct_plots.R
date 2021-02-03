
#' Barplot portraying the chat activity by weekday
#'
#' @param data tibble provided by the `prep_data()` function
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @import dplyr
#' @author Emanuel Sommer
plot_weekday_activity <- function(data) {
  data %>%
    mutate(wday_ord = ordered(wday,
      levels = c("2", "3", "4", "5", "6", "7", "1"),
      labels = c(
        "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday",
        "Sunday"
      )
    )) %>%
    group_by(wday_ord) %>%
    summarize(
      n_mess = n(),
      n_words = sum(n_words),
      .groups = "drop"
    ) %>%
    mutate(
      mess_rel = n_mess / nrow(data),
      words_rel = n_words / sum(data$n_words)
    ) %>%
    select(wday_ord, mess_rel, words_rel) %>%
    tidyr::pivot_longer(-wday_ord,
      names_to = "type",
      values_to = "rel_value"
    ) %>%
    ggplot(aes(x = wday_ord, y = rel_value, fill = type)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_x_discrete(limits = c(
      "Monday", "Tuesday", "Wednesday",
      "Thursday", "Friday", "Saturday",
      "Sunday"
    )) +
    scale_fill_manual(
      name = "",
      values = c("#58E370", "#3C252B"),
      labels = c("Messages", "Words")
    ) +
    labs(x = "", y = "Relative frequency") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 16),
          text = element_text(size = 16))
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
                              days = c(
                                "Monday", "Tuesday", "Wednesday",
                                "Thursday", "Friday", "Saturday",
                                "Sunday"
                              )) {
  valid_days <- c(
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
  )
  checkmate::assert_character(days,
    any.missing = FALSE,
    unique = TRUE, max.len = 7
  )
  stopifnot(all(days %in% valid_days))
  num_day <- vapply(days, function(day) {
    which(day == valid_days)
  }, FUN.VALUE = numeric(1))

  data %>%
    mutate(hour = as.factor(lubridate::hour(time))) %>%
    filter(wday %in% num_day) %>%
    group_by(hour) %>%
    summarize(
      n_mess = n(),
      n_words = sum(n_words),
      .groups = "drop"
    ) %>%
    mutate(
      mess_rel = n_mess / nrow(data),
      words_rel = n_words / sum(data$n_words)
    ) %>%
    select(hour, mess_rel, words_rel) %>%
    tidyr::pivot_longer(-hour,
      names_to = "type",
      values_to = "rel_value"
    ) %>%
    ggplot(aes(x = hour, y = rel_value, fill = type)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_x_discrete(limits = paste(0:23)) +
    scale_fill_manual(
      name = "",
      values = c("#58E370", "#3C252B"),
      labels = c("Messages", "Words")
    ) +
    labs(x = "Hour of the day", y = "Relative frequency") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 16),
          text = element_text(size = 16))
}



#' Barplot displaying the relative frequencies of the users
#'
#' @param data tibble provided by the `prep_data()` function
#'
#' @return ggplot2 object
#' @import ggplot2
#' @import dplyr
#'
#' @author Emanuel Sommer
plot_total_words <- function(data) {
  data %>%
    group_by(author) %>%
    summarize(
      n_words = sum(n_words),
      n_mess = n(),
      .groups = "drop"
    ) %>%
    mutate(
      n_rel_words = n_words / sum(data$n_words),
      n_rel_mess = n_mess / nrow(data)
    ) %>%
    select(author, n_rel_words, n_rel_mess) %>%
    tidyr::pivot_longer(-author,
      names_to = "type",
      values_to = "rel_value"
    ) %>%
    ggplot(aes(x = author, y = rel_value, fill = type)) +
    geom_col(position = "dodge", alpha = 0.8) +
    labs(x = "", y = "Relative frequencies") +
    scale_fill_manual(
      name = "",
      values = c("#58E370", "#3C252B"),
      labels = c("Messages", "Words")
    ) +
    coord_flip() +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 16),
          text = element_text(size = 16))
}

#' PLot time against messages sent per day (scatterplot + smoothing line for trends)
#'
#' @param data tibble provided by the `prep_data()` function
#'
#' @return plotly object
#' @import ggplot2
#' @import dplyr
#'
#' @author Emanuel Sommer
plot_ts_mess_per_day <- function(data) {
  color_ramp <- grDevices::colorRampPalette(c(
    "#58E370", "#EBE126",
    "#DE793B", "#A84448",
    "#3C252B"
  ))
  
  plot_gg <- data %>%
    mutate(day = as.Date(time)) %>%
    group_by(day, author) %>%
    summarize(n_mess = n(),
              .groups = 'drop') %>%
    rename(Date = day, Messages = n_mess, Author = author) %>%
    ggplot(aes(x = Date, y = Messages, col = Author)) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE, method = "loess", formula = 'y ~ x') +
    labs(x = "", y = "Messages per day") +
    scale_color_manual(
      name = "",
      values = color_ramp(length(unique(data$author)))
    ) +
    scale_y_log10() +
    theme_classic() +
    theme(
      text = element_text(size = 13),
      legend.text = element_text(size = 13),
      axis.text = element_text(size = 13)
    ) 
  plotly::ggplotly(plot_gg)
}


#' Density  plot of emojis or words per message (distinct users by fill/color)
#'
#' @param data tibble provided by the `prep_data()` function
#' @param emo logical whether to plot emojis or words per message
#' @param bw bandwidth of the gaussian kernels (default: `stats::bw.nrd0()`)
#'
#' @return ggplot2 object
#' @import ggplot2
#' @import dplyr
#'
#' @author Emanuel Sommer
plot_emoji_words_per_mess_dens <- function(data, emo = TRUE, bw = NULL) {
  color_ramp <- grDevices::colorRampPalette(c(
    "#58E370", "#EBE126",
    "#DE793B", "#A84448",
    "#3C252B"
  ))
  type <- ifelse(emo, "n_emojis", "n_words")
  bw <- ifelse(is.null(bw), stats::bw.nrd0(data[[type]]), bw)
  axis_text <- ifelse(emo, "Emoji", "Words")
  data %>%
    select(c("author", type)) %>%
    tidyr::pivot_longer("author") %>%
    ggplot(aes_string(x = type, fill = "value", col = "value")) +
    geom_density(alpha = 0.2, bw = bw) +
    labs(x = paste(axis_text, "per message"), y = "") +
    scale_fill_manual(
      name = "",
      values = color_ramp(length(unique(data$author)))
    ) +
    scale_color_manual(
      name = "",
      values = color_ramp(length(unique(data$author)))
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 16),
      legend.text = element_text(size = 16),
      axis.text = element_text(size = 16),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}


#' Boxplot of emojis or words per message (distinct users)
#'
#' @param data tibble provided by the `prep_data()` function
#' @param emo logical whether to plot emojis or words per message
#'
#' @return ggplot2 object
#' @import ggplot2
#' @import dplyr
#'
#' @author Emanuel Sommer
plot_emoji_words_per_mess_box <- function(data, emo = TRUE) {
  color_ramp <- colorRampPalette(c(
    "#58E370", "#EBE126",
    "#DE793B", "#A84448",
    "#3C252B"
  ))
  type <- ifelse(emo, "n_emojis", "n_words")
  axis_text <- ifelse(emo, "Emoji", "Words")
  data %>%
    select(c("author", type)) %>%
    tidyr::pivot_longer("author") %>%
    ggplot(aes_string(y = type, x = "value")) +
    geom_boxplot(alpha = 0.8, fill = "#58E370", col = "#3C252B") +
    labs(y = paste(axis_text, "per message"), x = "") +
    coord_flip() +
    theme_classic() +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 16))
}


#' Barchart to display the frequency of the top 10 used emojis of a given user
#'
#' @param data tibble provided by the `prep_data()` function
#' @param authors character or factor vector conatining valid authors from the data$author column
#'
#' @return ggplot2 object
#' @import ggplot2
#' @import dplyr
#'
#' @author Emanuel Sommer
plot_top10_emojis <- function(data, authors) {
  stopifnot(all(authors %in% unique(data$author)))
  filtered_data <- data %>%
    filter(author %in% authors)
  emojis <- unlist(filtered_data$emoji_name)
  emojis <- stringr::str_remove(emojis, ":.*")
  tibble(emojis = emojis) %>%
    group_by(emojis) %>%
    summarize(
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(n)) %>%
    slice_head(n = 10) %>%
    left_join(emo::jis, by = c("emojis" = "name")) %>%
    distinct(emojis, .keep_all = TRUE) %>%
    select(emoji, n) %>%
    stats::na.omit() %>%
    ggplot(aes(x = forcats::fct_reorder(emoji, n, .desc = FALSE), y = n)) +
    geom_col(fill = "#3C252B", col = "#3C252B", alpha = 0.8) +
    labs(x = "", y = "Frequency") +
    coord_flip() +
    theme_classic() +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.text.y = ggtext::element_markdown(size = 22))

  # without emo package:
  # filtered_data <- data %>%
  #   filter(author %in% authors)
  # emojis <- unlist(filtered_data$emoji)
  # tibble(emojis = emojis) %>%
  #   group_by(emojis) %>%
  #   summarize(n = n(),
  #             .groups = 'drop') %>%
  #   arrange(desc(n)) %>%
  #   slice_head(n = 10) %>%
  #   ggplot(aes(x = forcats::fct_reorder(emojis, n, .desc = FALSE), y = n)) +
  #   geom_col(fill = "#3C252B", col = "#3C252B", alpha = 0.8) +
  #   labs(x = "", y = "Frequency") +
  #   coord_flip() +
  #   theme_classic()
}
