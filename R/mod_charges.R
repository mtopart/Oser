#' charges UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bs4Dash box
#' @importFrom rhandsontable  rHandsontableOutput  rhandsontable renderRHandsontable hot_to_r
#' @importFrom glue glue


mod_charges_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Charges",
        width = 8,
        rHandsontableOutput(outputId = ns("tabelle")),
        textOutput(ns("texte_ch"))
        
      )
    )
  )
}
    
#' charges Server Functions
#'
#' @noRd 
mod_charges_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    Charges <- c("", "","","","", "", "", "", "", "")
    Mini <- c("", "","","","", "", "", "", "", "") %>% 
      as.numeric()
    Maxi <- c("", "","","","", "", "", "", "", "") %>% 
      as.numeric()
    
    
    
    
    df <- data.frame(Charges, 
                     Mini, 
                     Maxi)
    
    
    output$tabelle <- renderRHandsontable({
      rhandsontable(df)
    }) 
    
 
    
    
    
    
    observeEvent(eventExpr = input$tabelle, {
      df$values <- hot_to_r(input$tabelle)
      
     sum_mini <- df$values[[2]] %>% 
       sum(., na.rm = TRUE)
     
     sum_maxi <- df$values[[3]] %>% 
       sum(., na.rm = TRUE)
      
      output$texte_ch <- renderText({
     glue('Les charges varient de {sum_mini} à {sum_maxi} (€ ou k€).')  
        
      })
       
      
    })
    
    
  
  })
}
    
## To be copied in the UI
# mod_charges_ui("charges_ui_1")
    
## To be copied in the server
# mod_charges_server("charges_ui_1")
