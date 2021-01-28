#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session ) {
  # List the first level callModules here
  data <- reactive({
    if (!is.null(input$file)) {
      prep_data(
        rwhatsapp::rwa_read(input$file$datapath)
      )
    }
  })
  output$total_words <- renderValueBox({
    valueBox(value = sum(data()$n_words),
             "Total words",
             icon = icon("thumbs-up"),
             color = "aqua")
  })
}
