#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash 
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Define this page as a dashboard page to signal we're using the     dashboard page format
    dashboardPage(
      
      title = "Oser",
      fullscreen = TRUE,
      header = dashboardHeader(
        title = "Oser"
      ),
      # Create our navigation menu that links to each of the tabs we defined
      sidebar = dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        sidebarUserPanel(
          image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
          name = "Welcome Onboard!"
        ),
        sidebarMenu(
          id = "tabs",
          menuItem("L'outil", icon = icon("dashboard"), startExpanded = TRUE,
                   menuSubItem(
            "Choix de l'unité", tabName = "unit", icon = icon("circle-thin")
            ),
                  menuSubItem(
            "Oser", icon = icon("circle-thin"), tabName = "oser")),
          menuItem("Tutoriels", icon = icon("bar-chart-o"), tabName = "tuto"),
          menuItem("A propos", icon = icon("th"), tabName = "apropos")
        )
      ),
      footer = dashboardFooter(
        left = "Prototype - application en développement",
        right = "2022 - Strat&co"
      ),
      controlbar = NULL,
      body = dashboardBody(
        tabItems(
          tabItem("unit", mod_choix_unit_ui("choix_unit_ui_1")),
          tabItem("oser", mod_Oser_ui("Oser_ui_1")),
          tabItem("tuto", mod_MuscleGroup_ui("MuscleGroup_ui_1")),
          tabItem("apropos", mod_Exercises_ui("Exercises_ui_1")
          )
        )

 
      )
    ))

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
      app_title = 'Oser'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

