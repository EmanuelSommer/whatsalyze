#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session ) {
  # List the first level callModules here
  
  ##########################################################
  #               file input / data read
  ##########################################################
  # Show Modal after opening the application for file input:
  showModal(
    choose_file_input_dialog(initial = TRUE)
  )
  # Show Modal to update file input when using the corresponding action button
  observeEvent(input$inputbutton, handlerExpr = {
    showModal(
      choose_file_input_dialog()
    )
  })
  
  # global reactive values for all modules
  r <- reactiveValues(
    data = NULL
  )
  # main colors: "#58E370" "#EBE126" "#DE793B" "#A84448" "#3C252B"
  r$data <- reactive({
    if (!is.null(input$file)) {
      prep_data(
        rwhatsapp::rwa_read(input$file$datapath)
      )
    }
  })
  
  ##########################################################
  #                   Analysis
  ##########################################################
  # Overview section
  callModule(mod_over_stats_server, "over_stats_ui", r = r)
  callModule(mod_over_act_server, "over_act_ui", r = r)
  # Compare them! section
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
  
  callModule(mod_comp_messages_server, "comp_messages_ui", r = r)
  callModule(mod_comp_emojis_server, "comp_emojis_ui", r = r)
  
}
