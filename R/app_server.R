#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  
  # List the first level callModules here
 
  mod_choix_unit_server("choix_unit_ui_1")
  mod_Oser_server("Oser_ui_1")
  mod_Home_server("Home_ui_1")
  mod_MuscleGroup_server("MuscleGroup_ui_1")
  mod_Exercises_server("Exercises_ui_1")
  
  
}
