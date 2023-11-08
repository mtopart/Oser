#' gestion_unites UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_unites_secondaire_ui <- function(id){
  ns <- NS(id)
  tagList(
    h5("Gestion des unit\u00e9s"),
    
    textInput(ns("echelle"), 
              label = "Echelle de travail"),
 
    
    textInput(ns("unit_prod"), 
              label = "Production exprim\u00e9e en"),
    
    selectInput(
      ns("unit_e"), ## val_unit_e----------
      label = "Unit\u00e9 euros",
      choices = c("\u20ac",
                  "k\u20ac")),
    
     textInput(ns("unit_prix"), 
              label = "Unit\u00e9 du prix : \u20ac ou k\u20ac par"),
    
    

    selectInput(ns("select_solde"), label = "Solde final",
                choices = list("Marge brute",
                               "Marge nette",
                               "EBE",
                               "Autre" =  "autre"),
                selected = 1),
    
    textInput(ns("val_solde"), 
              label = "Solde"),
    
    
    actionButton(ns("button_unit"), icon("chevron-right"),  label = "Valider")

    
  )
}
    
#' gestion_unites Server Functions
#'
#' @noRd 
mod_unites_secondaire_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    


    solde_f <- reactive({

      d <-  if(input$select_solde == "autre"){ input$val_solde
      } else { input$select_solde }

      paste0(d, " (en ", input$unit_e,")" )

    })

    unit_solde2 <- reactive({

      d <-  if(input$select_solde == "autre"){  input$val_solde
      } else { input$select_solde }

      d

    })
    
    
    unit_prix <- reactive({
      paste0(input$unit_e, " / ", input$unit_prix)
    })
    
    
observe({
    updateTextInput(session, "echelle", value = r$echelle) })
  
observe({
    updateTextInput(session, "unit_prod", value = r$unit_prod) })
  
observe({  
    updateTextInput(session, "unit_prix", value = r$unit_prix_p) })

observe({
    updateSelectInput(
      session,
      "select_solde",
      label = "Solde final",
      choices = list("Marge brute",
                     "Marge nette",
                     "EBE",
                     "Autre" = "autre"),
      selected = r$select_solde
    )})
    
observe({
  updateSelectInput(
      session,
      "unit_e",
      label = "Unit\u00e9 euros",
      choices = c("\u20ac",
                  "k\u20ac"),
      selected = r$unit_e
    ) })

  observe({  
    toggle("val_solde", condition = input$select_solde == "autre") 
    
})
  
  
  observe({
    updateTextInput(session, "val_solde", value = r$solde2)
    
  })
  

    observeEvent( input$button_unit, {
      r$button_unit <- input$button_unit

      r$echelle <- input$echelle
      r$unit_prix <- unit_prix()
      r$unit_prod <- input$unit_prod
      r$unit_e <-   input$unit_e
      r$unit_prix_p <- input$unit_prix
      r$select_solde <- input$select_solde
      r$solde <- solde_f()
      r$solde2 <- unit_solde2()
    })

  
 
  })
}
    
## To be copied in the UI
# mod_unites_secondaire_ui("gestion_unites_1")
    
## To be copied in the server
# mod_unites_secondaire_server("gestion_unites_1")
