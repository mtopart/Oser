#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(insertTab, actionButton, tabsetPanel, column))
#' @noRd
app_server <- function( input, output, session ) {

  
  
  r <- reactiveValues(
    ch = NULL 
  )
  
  # List the first level callModules here
 
  mod_choix_unit_server("choix_unit_ui_1", r = r)
  mod_Oser_server("Oser_ui_1", r = r)
  mod_tutoriel_server("tutoriel_ui_1")
  mod_apropos_server("apropos_ui_1")
  mod_charges_server("charges_ui_1", r = r)
  
  
 
  
  
}
