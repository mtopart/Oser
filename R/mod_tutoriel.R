#' tutoriel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tutoriel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    column(
      width = 12,
      
        p("Page en construction", style = "color:red ;font-size: 20px;
                                 font-style: italic"),
        br(),
      
    h3("Le choix du solde est laissé au conseiller, en fonction de ses préférences"),
    p("La signification des résultats est différente en fonction des charges rentrées dans le calculateur."),
    p(em("Pour mieux comprendre :")),
    img(src = "www/soldes.jpg", width = "95%")
 
  )))
}
    
#' tutoriel Server Functions
#'
#' @noRd 
mod_tutoriel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_tutoriel_ui("tutoriel_ui_1")
    
## To be copied in the server
# mod_tutoriel_server("tutoriel_ui_1")
