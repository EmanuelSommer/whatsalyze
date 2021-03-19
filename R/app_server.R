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
    data = NULL,
    multi_table = NULL
  )
  
  # allow the user to use test data
  r$data <- eventReactive(input$use_test_data, {
    test_chat_data
  })
  
  # main colors: "#58E370" "#EBE126" "#DE793B" "#A84448" "#3C252B"
  r$data <- eventReactive(input$upload_button, {
    if (input$use_test_data) {
      test_chat_data
    } else {
      if (!is.null(input$file)) {
        prep_data(
          rwhatsapp::rwa_read(input$file$datapath)
        )
      }
    }
  })
  
  
  ##########################################################
  #                   Analysis
  ##########################################################
  # Overview section
  callModule(mod_over_stats_server, "over_stats_ui", r = r)
  callModule(mod_over_act_server, "over_act_ui", r = r)
  # Compare them! section
  callModule(mod_comp_overall_server, "comp_overall_ui", r = r)
  callModule(mod_comp_messages_server, "comp_messages_ui", r = r)
  callModule(mod_comp_emojis_server, "comp_emojis_ui", r = r)
  # multi compare section
  callModule(mod_multi_table_server, "multi_table_ui", r = r)
}
