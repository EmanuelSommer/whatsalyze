#' comp_messages UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comp_messages_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$br(),
    fluidRow(
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          title = "Words per message",
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Boxplot"
            )
          ),
          plotOutput(ns("words_per_mess_box")),
          tags$hr(),
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Density estimate"
            )
          ),
          uiOutput(ns("bw_select_words")),
          plotOutput(ns("words_per_mess_dens"))
        )
      ),
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          title = "Emojis per message",
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Boxplot"
            )
          ),
          plotOutput(ns("emojis_per_mess_box")),
          tags$hr(),
          tags$div(
            style = "text-align: center;color: #3C252B;font-weight: bold;",
            tags$h2(
              "Density estimate"
            )
          ),
          uiOutput(ns("bw_select_emojis")),
          plotOutput(ns("emojis_per_mess_dens"))
        )
      )
    )
  )
}
    
#' comp_messages Server Function
#'
#' @noRd 
mod_comp_messages_server <- function(input, output, session, r){
  ns <- session$ns
  # kernel bandwidth selectors
  output$bw_select_words <- renderUI({
    shiny::req(r$data())
    numericInput(ns("bw_words"),
                 label = "Change kernel bandwidth",
                 value = round(stats::bw.nrd0(log10(r$data()$n_words + 1e-10)),5),
                 min = 0, step = 0.1)
  })
  output$bw_select_emojis <- renderUI({
    shiny::req(r$data())
    numericInput(ns("bw_emojis"),
                 label = "Change kernel bandwidth",
                 value = round(stats::bw.nrd0(log10(r$data()$n_emojis + 1e-10)),5),
                 min = 0, step = 0.1)
  })
  # density plots:
  output$words_per_mess_dens <- renderPlot({
    shiny::req(r$data(), input$bw_words)
    plot_emoji_words_per_mess_dens(r$data(),
                              emo = FALSE,
                              bw = input$bw_words)
  })
  output$emojis_per_mess_dens <- renderPlot({
    shiny::req(r$data(), input$bw_emojis)
    plot_emoji_words_per_mess_dens(r$data(),
                              emo = TRUE,
                              bw = input$bw_emojis)
  })
  # boxplots:
  output$words_per_mess_box <- renderPlot({
    shiny::req(r$data())
    plot_emoji_words_per_mess_box(r$data(),
                                   emo = FALSE)
  })
  output$emojis_per_mess_box <- renderPlot({
    shiny::req(r$data())
    plot_emoji_words_per_mess_box(r$data(),
                                   emo = TRUE)
  })
}
    
## To be copied in the UI
# mod_comp_messages_ui("comp_messages_ui")
    
## To be copied in the server
# callModule(mod_comp_messages_server, "comp_messages_ui", r)
 
