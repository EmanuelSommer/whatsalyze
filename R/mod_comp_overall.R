#' comp_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comp_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2),
      column(
        8,
        tags$br(),
        shinydashboard::box(
          title = "",
          width = NULL,
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Message and word frequencies"
            )
          ),
          plotOutput(ns("comp_overall_plot"))
        ),
        shinydashboard::box(
          title = "",
          width = NULL,
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "The conversation starter"
            ),
            tags$h4(
              emo::ji_glue("After at least 5 days of awkward silence :smirking_face:")
            )
          ),
          plotOutput(ns("comp_convstarter_plot"))
        ),
        shinydashboard::box(
          title = "",
          width = NULL,
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Last man standing"
            ),
            tags$h4(
              emo::ji_glue("At least 4 days of alone :pensive:")
            )
          ),
          plotOutput(ns("comp_last_man_standing_plot"))
        ),
        shinydashboard::box(
          title = "",
          width = NULL,
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Messages per day over time"
            )
          ),
          plotly::plotlyOutput(ns("ts_mess_per_day_plot"))
        )
      ),
      column(2)
    )
  )
}
    
#' comp_overall Server Function
#'
#' @noRd 
mod_comp_overall_server <- function(input, output, session, r){
  ns <- session$ns
  output$comp_overall_plot <- renderPlot({
    shiny::req(r$data())
    plot_total_words(r$data())
  })
  output$comp_convstarter_plot <- renderPlot({
    shiny::req(r$data())
    plot_conversation_starter(r$data())
  })
  output$comp_last_man_standing_plot <- renderPlot({
    shiny::req(r$data())
    plot_last_man_standing(r$data())
  })
  output$ts_mess_per_day_plot <- plotly::renderPlotly({
    shiny::req(r$data())
    plot_ts_mess_per_day(r$data())
  })
}
    
## To be copied in the UI
# mod_comp_overall_ui("comp_overall_ui")
    
## To be copied in the server
# callModule(mod_comp_overall_server, "comp_overall_ui", r = r)
 
