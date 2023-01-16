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
    p("Page en construction - onglet non fonctionnel", style = "color:red ;font-size: 20px;
                                 font-style: italic"),
    br(),
    h5("Gestion des unités"),
    
    textInput(ns("echelle"), 
              label = "Echelle de travail"),
 
    
    textInput(ns("unit_prod"), 
              label = "Production exprimée en"),
    
    selectInput(
      ns("unit_e"), ## val_unit_e----------
      label = "Unité euros",
      choices = c("€",
                  "k€")),
    
     textInput(ns("unit_prix"), 
              label = "Unité du prix : € ou k€ par"),
    
    

    selectInput(ns("select_solde"), label = "Solde final",
                choices = list("Marge brute",
                               "Marge nette",
                               "EBE",
                               "Autre" =  "autre"),
                selected = 1),
    
    textInput(ns("val_solde"), 
              label = "Solde"),
    
    
    actionButton(ns("button_unit"), icon("chevron-right"))
    

    
  )
}
    
#' gestion_unites Server Functions
#'
#' @noRd 
mod_unites_secondaire_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    
   # observeEvent(input$select_solde == 4, {toggle("solde")})


    solde_f <- reactive({

      if(input$select_solde == 4){ input$solde
        } else { input$select_solde }

    })
    
    
    unit_prix <- reactive({
      paste0(input$unit_e, " / ", input$unit_p)
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
      label = "Unité euros",
      choices = c("€",
                  "k€"),
      selected = r$unit_e
    ) })

  observe({  
    toggle("val_solde", condition = input$select_solde == "autre") 
    updateTextInput(session, "val_solde", value = r$echelle)
    
})

# r$echelle <- input$val_echelle
# r$unit_prix <- unit_prix()
# r$unit_prix_p <- input$val_unit_p
# r$unit_prod <- input$val_unit_prod
# r$unit_e <-   input$val_unit_e
# r$solde <- unit_solde()
    

    observeEvent( input$button_unit, {
      r$button_unit <- input$button_unit

      r$echelle <- input$echelle
      r$unit_prix <- unit_prix()
      r$unit_prod <- input$unit_prod
      r$unit_e <-   input$unit_e
      r$unit_prix_p <- input$unit_p
      r$solde <- solde_f()


    })

    

    
    
 
  })
}
    
## To be copied in the UI
# mod_unites_secondaire_ui("gestion_unites_1")
    
## To be copied in the server
# mod_unites_secondaire_server("gestion_unites_1")
