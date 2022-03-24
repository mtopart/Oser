#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(insertTab, actionButton, tabsetPanel, column))
#' @noRd
app_server <- function( input, output, session ) {

  
  
  r <- reactiveValues()
  
  # List the first level callModules here
  
  
  # Mod distrib
  
  mod_box_distrib_server("box_distrib_ui_1",
                         graph_title = "Répartition de la production",
                         type = "production",
                         r = r)
  
  mod_box_distrib_server("box_distrib_ui_2",
                         graph_title = "Répartition du prix",
                         type = "prix",
                         r = r)
  
  mod_box_distrib_server("box_distrib_ui_3",
                         graph_title = "Répartition des charges",
                         type = "charges",
                         r = r)
  
  mod_aide_distrib_server("aide_distrib_ui_1")
  
  mod_graph_final_server("graph_final_ui_1",
                         r = r)
 
  mod_choix_unit_server("choix_unit_ui_1", r = r)
 
  mod_tutoriel_server("tutoriel_ui_1")
  mod_apropos_server("apropos_ui_1")
  
  
 
  
  
}
