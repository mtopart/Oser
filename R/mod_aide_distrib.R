#' aide_distrib UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @rawNamespace import(shiny, except = c(insertTab, actionButton, tabsetPanel, column))
#' @importFrom shinyWidgets materialSwitch 
#' @importFrom shinyjs toggle
mod_aide_distrib_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    column( 
      12,
      materialSwitch(
        inputId = ns("aide"),
        label = strong("J'ai besoin d'aide pour comprendre comment tracer ma distribution"),
        value = FALSE,
        status = "primary",
        inline = TRUE
      ),
      
      p(id = ns("h1"), strong("Utilisation :")," Cliquez directement dans le graphique pour allouer un nombre de jetons à chaque intervalle. 
                           Cliquez juste en-dessous de la ligne du 0 de l'axe des y pour effacer une case."),
      p(id= ns("h2"), "Si un message d'erreur ", strong("rouge")," apparait, le nombre de jetons est insuffisant pour faire le calcul."), 
      br(id = ns("h3")),
      p(id = ns("h4"), strong("Pour mieux comprendre :"), "Plus la barre est haute, plus la probabilité que la valeur de l'intervalle soit atteinte dans la distribution est élevée.
                           Inversement, s'il n'y a pas de jeton dans une barre verticale, la probabilité qu'il y ait une valeur dans l'intervalle est", strong(" faible, mais possible.") 
      ),
      
      br())
 
  )
}
    
#' aide_distrib Server Functions
#'
#' @noRd 
mod_aide_distrib_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      toggle(id = "h1", condition = input$aide)
      toggle(id = "h2", condition = input$aide)
      toggle(id = "h3", condition = input$aide)
      toggle(id = "h4", condition = input$aide)
    })
 
  })
}
    
## To be copied in the UI
# mod_aide_distrib_ui("aide_distrib_ui_1")
    
## To be copied in the server
# mod_aide_distrib_server("aide_distrib_ui_1")
