#' Input file modalDialog chooser
#' 
#' Helper to get a consistent dialog modal for the fileInput
#' + Action button to use test chat data (server logic in main server file)
#'
#' @param initial logical indicating wheter it is the initial fileInput or an update
#'
#' @return shiny::modalDialog object
#' @author Emanuel Sommer
choose_file_input_dialog <- function(initial = FALSE) {
  title_mess <- ifelse(initial,
                       emo::ji_glue(":waving_hand: Hello there!
                                    Please upload the file to analyze. :waving_hand:"),
                       emo::ji_glue("Update the file to analyze if you wish
                                    :person_shrugging:"))
  shiny::modalDialog(
    title = title_mess,
    fileInput("file",label = "Choose chat.txt file",
              width = "100%",
              accept = c(".txt")),
    tags$strong("Notes:"),
    tags$br(),
    " - If no file is uploaded the application is useless.",
    tags$br(),
    " - The file must be a '.txt' file of a whatsapp chat.",
    tags$br(),
    shiny::checkboxInput("use_test_data", 
                        label = "Use random test data"),
    shiny::actionButton("upload_button",
                        width = "100%",
                        label = "Upload data",
                        icon = icon("file-upload")),
    footer = modalButton("Close")
  )
}
