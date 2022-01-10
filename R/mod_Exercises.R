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
    fluidRow(
      box(
        title = "Production et quantité",
        width = 4,
        "(par unité de mesure sur une échelle choisie)",
        p(em("Par exemple : en tonnes pour 150 ha ou 1000 L pour l'atelier vaches laitières")),
        br(),
        wellPanel(numericInput(
          inputId = ns("prod_min"),
          label = "minimale",
          value = 20),
          
          numericInput(
            inputId = ns("prod_max"),
            label = "maximale",
            value = 50)
        ),
        
        p(em("Distribution :")),
        verbatimTextOutput(ns("distrib"))
      )
    )
    
  )
}

#' Oser Server Functions
#'
#' @noRd 
mod_Exercises_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
    distrib <- reactive ({
      prod_min <- input$prod_min
      prod_max <- input$prod_max
      distribution <-runif(40, prod_min, prod_max)
      distribution 
    })  
    
    output$distrib <- renderPrint(distrib())
    
  })
}

## To be copied in the UI
# mod_Exercises_ui("Exercises_ui_1")

## To be copied in the server
# mod_Exercises_server("Exercises_ui_1")