#' multi_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_multi_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        collapsible = TRUE,
        title = "Add a chat to the comparison table below",
        # tags$div(
        #   style = "text-align: center;color: #3C252B;font-weight: bold;",
        #   tags$h2(
        #     "Add a chat to the comparison table below"
        #   )
        # ),
        fileInput(ns("file_in"),label = "Choose chat.txt file",
                  accept = c(".txt")),
        tags$strong("Note:"), 
        tags$br(),
        " - If no file is uploaded the table is useless.",
        tags$br(),
        " - The file must be a '.txt' file of a whatsapp chat.",
        textInput(ns("chatname"),
                  label = "Name of the chat (required)",
                  value = ""),
        actionButton(ns("submit_chat"),
                     label = "Submit chat to comparison",
                     icon = icon("check"))
      ),
      shinydashboard::box(
        width = 12,
        title = "",
        tags$div(
          style = "text-align: center;color: #3C252B;font-weight: bold;",
          tags$h2(
            "Comparison table"
          )
        ),
        DT::dataTableOutput(ns("multi_comp_table")),
        tags$br(),
        actionButton(ns("reset_table"),
                     label = "Reset table",
                     icon = icon("redo"))
      )
    )
  )
}
    
#' multi_table Server Function
#'
#' @noRd 
mod_multi_table_server <- function(input, output, session, r){
  ns <- session$ns
  
  observeEvent(input$submit_chat, {
    if(input$chatname != "" & !is.null(input$file_in)) {
      temp_data <- prep_data(
        rwhatsapp::rwa_read(input$file_in$datapath)
      )
      new_table_elements <- tibble::tibble(name = input$chatname)
      time_in_days <- as.numeric(
        as.Date(max(temp_data[["time"]])) - as.Date(min(temp_data[["time"]]))
      ) + 1
      new_table_elements$messages_per_day <- round(nrow(temp_data) / 
                                                     time_in_days, 3)
      new_table_elements$words_per_day <- round(sum(temp_data[["n_words"]]) / 
                                                  time_in_days, 3)
      new_table_elements$streak <- get_largest_streak(
        temp_data[["time"]])
      new_table_elements$words_per_message <- round(
        sum(temp_data[["n_words"]]) / nrow(temp_data),3)
      new_table_elements$duration <- time_in_days
      new_table_elements$messages <- nrow(temp_data)
      new_table_elements$words <- sum(temp_data[["n_words"]])
      
      if(is.null(r$multi_table)) {
        r$multi_table <- new_table_elements
      } else {
        r$multi_table <- r$multi_table %>%
          dplyr::bind_rows(new_table_elements) %>%
          dplyr::arrange(desc(messages_per_day))
      }
    }
  })
  
  observeEvent(input$reset_table, {
    r$multi_table <- NULL
  })
  
  output$multi_comp_table <- DT::renderDataTable({
    r$multi_table
  })
}
    
## To be copied in the UI
# mod_multi_table_ui("multi_table_ui")
    
## To be copied in the server
# callModule(mod_multi_table_server, "multi_table_ui", r = r)
 
