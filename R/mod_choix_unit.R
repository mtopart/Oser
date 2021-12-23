#' choix_unit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choix_unit_ui <- function(id){
  ns <- NS(id)
  tagList(
      
      fluidRow(
        
        column(4,
               box(
                 background = "primary",
                 h2("You lifted XYZ xx times!"),
                 title = "Congrats!"),
               br(), br(), br(), br()
        )),
      
      column(4,
             box(
               background = "primary",
               h2("You lifted XYZ xx times!"),
               title = "Congrats!"),
             br(), br(), br(), br()
      ),
      
      column(4,
             box(
               background = "primary",
               h2("You lifted XYZ xx times!"),
               title = "Congrats!"),
             br(), br(), br(), br()
      )
      
      
    )
  
}
    
#' choix_unit Server Functions
#'
#' @noRd 
mod_choix_unit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_choix_unit_ui("choix_unit_ui_1")
    
## To be copied in the server
# mod_choix_unit_server("choix_unit_ui_1")
