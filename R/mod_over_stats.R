#' over_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_over_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("total_mess"), width = 0)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("streak"), width = 0)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("time_perc"), width = 0)
      )
    ),
    fluidRow(
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("total_time"), width = 0)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("total_words"), width = 0)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("media_voice"), width = 0)
      )
    ),
    fluidRow(
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("words_per_mess"), width = 0)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("mess_per_day"), width = 0)
      ),
      column(
        width = 4,
        shinydashboard::valueBoxOutput(ns("words_per_day"), width = 0)
      )
    )
  )
}
    
#' over_stats Server Function
#'
#' @noRd 
mod_over_stats_server <- function(input, output, session, r){
  ns <- session$ns
  
  # total duration of chat in days
  output$total_time <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    time_in_days <- as.numeric(
      as.Date(max(r$data()[["time"]])) - as.Date(min(r$data()[["time"]]))
    ) + 1
    shinydashboard::valueBox(time_in_days,
                             subtitle = "Duration in days", icon = icon("clock"),
                             color = "yellow", width = NULL
    )
  })
  
  # percentage of days chatted
  output$time_perc <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    time_in_days <- as.numeric(
      as.Date(max(r$data()[["time"]])) - as.Date(min(r$data()[["time"]]))
    ) + 1
    days_chatted <- length(unique(as.Date(r$data()[["time"]])))
    shinydashboard::valueBox(round(days_chatted / time_in_days, 2) * 100,
                             subtitle = "% days chatted", icon = icon("history"),
                             color = "yellow", width = NULL
    )
  })
  
  # total amount of messages
  output$total_mess <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    shinydashboard::valueBox(nrow(r$data()),
                             subtitle = "Total messages", icon = icon("envelope"),
                             color = "yellow", width = NULL
    )
  })
  
  # total amount of words
  output$total_words <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    shinydashboard::valueBox(sum(r$data()[["n_words"]]),
                             subtitle = "Total words", icon = icon("keyboard"),
                             color = "yellow", width = NULL
    )
  })
  
  # best streak (consecutive days chatted)
  output$streak <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    shinydashboard::valueBox(get_largest_streak(r$data()[["time"]]),
                             subtitle = "Longest streak", icon = icon("fire-alt"),
                             color = "yellow", width = NULL
    )
  })
  
  # total messages per day
  output$mess_per_day <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    time_in_days <- as.numeric(
      as.Date(max(r$data()[["time"]])) - as.Date(min(r$data()[["time"]]))
    ) + 1
    shinydashboard::valueBox(round(nrow(r$data()) / time_in_days, 3),
                             subtitle = "Messages per day", icon = icon("envelope"),
                             color = "yellow", width = NULL
    )
  })
  
  # total words per day
  output$words_per_day <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    time_in_days <- as.numeric(
      as.Date(max(r$data()[["time"]])) - as.Date(min(r$data()[["time"]]))
    ) + 1
    shinydashboard::valueBox(round(sum(r$data()[["n_words"]]) / time_in_days, 3),
                             subtitle = "Words per day", icon = icon("keyboard"),
                             color = "yellow", width = NULL
    )
  })
  
  # words per message
  output$words_per_mess <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    shinydashboard::valueBox(round(sum(r$data()[["n_words"]]) / nrow(r$data()),
                                   2),
                             subtitle = "Words per message",
                             icon = icon("arrows-alt-h"),
                             color = "yellow", width = NULL
    )
  })
  
  # sum of media and voicemails
  output$media_voice <- shinydashboard::renderValueBox({
    shiny::req(r$data())
    shinydashboard::valueBox(sum(r$data()[["text"]] == "<Medien ausgeschlossen>"),
                             subtitle = "Total media and voicemails",
                             icon = icon("microphone-alt"),
                             color = "yellow", width = NULL
    )
  })
}
    
## To be copied in the UI
# mod_over_stats_ui("over_stats_ui")
    
## To be copied in the server
# callModule(mod_over_stats_server, "over_stats_ui", r)
 
