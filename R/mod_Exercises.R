#' Exercises UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Exercises_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Youhou")
 
  )
}
    
#' Exercises Server Functions
#'
#' @noRd 
mod_Exercises_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Exercises_ui("Exercises_ui_1")
    
## To be copied in the server
# mod_Exercises_server("Exercises_ui_1")
