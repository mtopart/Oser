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
        
               box(
                 title = "Choix des unités",
                 textInput(inputId = ns("unit_ech"), 
                           label = "Echelle",
                           value = ""),
                 textInput(inputId = ns("unit_prod"), 
                           label = "Production ou quantité",
                           value = ""),
                 textInput(inputId = ns("unit_prix"), 
                           label = "Prix",
                           value = ""),
                 textInput(inputId = ns("unit_charges"), 
                           label = "Charges",
                           value = ""),
                 textInput(inputId = ns("unit_solde"), 
                           label = "Solde final",
                           value = "")),
               
               
               box(
                 textOutput(ns("unit_ech_text"))
               ),
               br(), br(), br(), br()
        ))
      
  
}
    
#' choix_unit Server Functions
#'
#' @noRd 
mod_choix_unit_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    unit_ech <- reactive ({
      x <- input$unit_ech
      
      if (x == ""){
        x <- "par unité de mesure"
      } 
      x})
    
    output$unit_ech_text <- renderText(unit_ech())
    
    
    unit_prod <- reactive ({
      unit_prod <- input$unit_prod  })
    
    unit_prix <- reactive ({
      unit_prix <- input$unit_prix })
    
    unit_charges <- reactive ({
      unit_charges <- input$unit_charges  })
    
    unit_solde <- reactive ({
      unit_solde <- input$unit_solde  })
    
    


    observeEvent(input$unit_prod, {
      r$une_valeur # existe
      # et on peut creer
      r$test <- input$unit_prod
    })
    
   
 
  })
}
    
## To be copied in the UI
# mod_choix_unit_ui("choix_unit_ui_1")
    
## To be copied in the server
# mod_choix_unit_server("choix_unit_ui_1")
