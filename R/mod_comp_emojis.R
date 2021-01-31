#' comp_emojis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comp_emojis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2),
      column(
        8,
        tags$br(),
        shinydashboard::box(
          title = NULL,
          width = NULL,
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              emo::ji_glue(":trophy: Top 10 emojis! :trophy:")
            )
          ),
          uiOutput(ns("select_author_ui")),
          plotOutput(ns("top10_emojis_plot"))
        )
      ),
      column(2)
    )
  )
}
    
#' comp_emojis Server Function
#'
#' @noRd 
mod_comp_emojis_server <- function(input, output, session, r){
  ns <- session$ns
  
  output$select_author_ui <- renderUI({
    shiny::req(r$data())
    selectInput(ns("selected_authors"),
                label = "Who do you choose?",
                choices = as.character(unique(r$data()$author)),
                selected = as.character(unique(r$data()$author)),
                multiple = TRUE
                )
  })
  output$top10_emojis_plot <- renderPlot({
    shiny::req(r$data(), input$selected_authors)
    plot_top10_emojis(r$data(),
                      authors = input$selected_authors)
  })
}
    
## To be copied in the UI
# mod_comp_emojis_ui("comp_emojis_ui")
    
## To be copied in the server
# callModule(mod_comp_emojis_server, "comp_emojis_ui", r)
 
