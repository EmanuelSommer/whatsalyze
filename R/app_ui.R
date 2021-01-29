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
    dashboardPage(
      skin = "black",
      dashboardHeader(
        title = "Whatsalyze",
        dropdownMenu(
          type = "notifications",
          icon = icon("link"),
          headerText = "Links",
          notificationItem("Visit my Github", icon = icon("github"),
                           href = "https://github.com/EmanuelSommer")
        )
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem(text = "Overview", 
                   menuSubItem(text = "Stats", tabName = "over_stats"),
                   menuSubItem(text = "Activity", tabName = "over_act"),
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
            mod_over_stats_ui("over_stats_ui")
          ),
          tabItem(
            tabName = "over_act",
            h2("activity plots")
          ),
          
          # Compare them  ##########################################
          tabItem(
            tabName = "comp_overall",
            h2("Overall stats compare plots")
          ),
          tabItem(
            tabName = "comp_messages",
            h2("Compare per messages plots")
          ),
          tabItem(
            tabName = "comp_emojis",
            h2("Compare emojis plots (choose user input)")
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

