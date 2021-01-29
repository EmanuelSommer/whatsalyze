#' over_act UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_over_act_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          tags$div(
            style = "text-align: left;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Activity by weekday"
            )
          ),
          plotOutput(ns("act_by_weekday"))
        )
      ),
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          tags$div(
            style = "text-align: left;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Activity by hour and days"
            )
          ),
          plotOutput(ns("act_by_day")),
          selectInput(ns("days"),
                      label = "Days to analyze",
                      choices = c("Monday", "Tuesday", "Wednesday",
                                  "Thursday", "Friday", "Saturday",
                                  "Sunday"),
                      multiple = TRUE,
                      selected = "Monday")
        )
      )
    )
  )
}
    
#' over_act Server Function
#'
#' @noRd 
mod_over_act_server <- function(input, output, session, r){
  ns <- session$ns
  output$act_by_weekday <- renderPlot({
    shiny::req(r$data())
    plot_weekday_activity(r$data())
  })
  output$act_by_day <- renderPlot({
    shiny::req(r$data(),
               input$days)
    plot_day_activity(r$data(), days = input$days)
  })
}
    
## To be copied in the UI
# mod_over_act_ui("over_act_ui")
    
## To be copied in the server
# callModule(mod_over_act_server, "over_act_ui", r)
 
