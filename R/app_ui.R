#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shiny.info::busy("spinner"),
    dashboardPage(
      skin = "black",
      dashboardHeader(
        title = "Whatsalyze",
        dropdownMenu(
          type = "notifications",
          icon = icon("link"),
          headerText = "Links",
          notificationItem("Source code on Github", icon = icon("github"),
                           href = "https://github.com/EmanuelSommer/whatsalyze")
        )
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem(text = "Overview", tabName = "over_stats",
                   icon = icon("glasses")
          ),
          menuItem(text = "Compare them!", 
                   menuSubItem(text = "Overall", tabName = "comp_overall"),
                   menuSubItem(text = "By message", tabName = "comp_messages"),
                   menuSubItem(text = "Emojis", tabName = "comp_emojis"),
                   icon = icon("chart-bar")
          )
        ),
        tags$br(),
        fluidRow(
          column(2),
          column(
            8,
            actionButton(
              "inputbutton",
              label = "Upload file",
              icon = icon("upload")
            )
          ),
          column(2)
        )
      ),
      dashboardBody(
        tags$style(".small-box.bg-yellow { background-color: #58E370 !important; color: #3C252B !important; }"),
        tabItems(
          # Overview ###############################################
          tabItem(
            tabName = "over_stats",
            tags$div(
              style = "text-align: center;color: #3C252B;font-weight: bold;",
              tags$h2(
                emo::ji_glue(":detective: Let's have a look! :detective:")
              )
            ),
            tags$br(),
            mod_over_stats_ui("over_stats_ui"),
            tags$br(),
            mod_over_act_ui("over_act_ui")
          ),
          
          # Compare them  ##########################################
          tabItem(
            tabName = "comp_overall",
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
                  plotOutput("comp_overall_plot")
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
                  plotly::plotlyOutput("ts_mess_per_day_plot")
                )
              ),
              column(2)
            )
          ),
          tabItem(
            tabName = "comp_messages",
            mod_comp_messages_ui("comp_messages_ui")
          ),
          tabItem(
            tabName = "comp_emojis",
            mod_comp_emojis_ui("comp_emojis_ui")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'whatsalyze'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

