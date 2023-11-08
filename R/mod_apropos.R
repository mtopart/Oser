#' apropos UI Function
#'
#' @description A propos d Oser
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_apropos_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "A propos",
      width = 12,
      
      div(style="text-align: center;",
      tags$img(src = "www/002.jpg", width = "85%" ),
      hr(),
      tags$img(src = "www/003.jpg", width = "85%" ),
      hr(),
      tags$img(src = "www/004.jpg", width = "85%" ),
      hr(),
      tags$img(src = "www/005.jpg", width = "85%" ))
      
  ))
}
    
#' apropos Server Functions
#'
#' @noRd 
mod_apropos_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_apropos_ui("apropos_ui_1")
    
## To be copied in the server
# mod_apropos_server("apropos_ui_1")
