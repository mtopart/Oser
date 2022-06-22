#' gestion_unites UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gestion_unites_ui <- function(id){
  ns <- NS(id)
  tagList(
    h5("Gestion des unités"),
    
    textInput(ns("echelle"), 
              label = "Echelle de travail", value = "Atelier, x ha ..."),
 
    
    textInput(ns("unit_prod"), 
              label = "Unité de la production (sur l'échelle de travail défini)", 
              value = "1 000 L, tonnes, nombre de paniers moyens... "),
    
    textInput(ns("unit_prix"), 
              label = "Unité du prix", 
              value = "\u20ac (ou k\u20ac) / 1 000 L (tonnes, paniers...) "),
    
    textInput(ns("unit_charges"), 
              label = "Unité des charges", value = "€ (ou k€) sur l'atelier, sur x ha ..."),
    
    selectInput(ns("select_solde"), label = "Solde final", 
                choices = list("Marge brute", 
                               "Marge nette", 
                               "EBE", 
                               "Autre" = 4), 
                selected = 1),
    
    textInput(ns("solde"), 
              label = "Solde"),
    
    textInput(ns("unit_solde"), 
              label = "Unité du solde", value = "€ (ou k€)"),
    
    actionButton(ns("button_unit"), icon("chevron-right"))
    

    
  )
}
    
#' gestion_unites Server Functions
#'
#' @noRd 
mod_gestion_unites_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
    observeEvent(input$select_solde == 4, {toggle("solde")})
    
    
    solde_f <- reactive({
      
      if(input$select_solde == 4){ input$solde
        } else { input$select_solde }
      
    })
    
    
    observeEvent( input$button_unit, {
      r$button_unit <- input$button_unit
      
      r$echelle <- input$echelle
      r$unit_prix <- input$unit_prix
      r$unit_prod <- input$unit_prod
      r$unit_ch <-   input$unit_charges
      r$solde <- solde_f()
      r$unit_solde <-   input$unit_solde
      
    })
    
    

    
    
 
  })
}
    
## To be copied in the UI
# mod_gestion_unites_ui("gestion_unites_1")
    
## To be copied in the server
# mod_gestion_unites_server("gestion_unites_1")
