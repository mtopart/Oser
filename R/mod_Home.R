#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Home_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Welcome HOME !")
 
  )
}
    
#' Home Server Functions
#'
#' @noRd 
mod_Home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Home_ui("Home_ui_1")
    
## To be copied in the server
# mod_Home_server("Home_ui_1")
