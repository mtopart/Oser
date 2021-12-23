#' MuscleGroup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MuscleGroup_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Bouh")
 
  )
}
    
#' MuscleGroup Server Functions
#'
#' @noRd 
mod_MuscleGroup_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_MuscleGroup_ui("MuscleGroup_ui_1")
    
## To be copied in the server
# mod_MuscleGroup_server("MuscleGroup_ui_1")
