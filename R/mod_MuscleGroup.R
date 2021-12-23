#' MuscleGroup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets
#' @import SHELF
#' 
mod_MuscleGroup_ui <- function(id){
  ns <- NS(id)
  tagList(
  
    fluidPage(
      fluidRow(
        h2("Muscle Group Progress for "),
        selectizeInput(
          inputId = ns("muscles"), 
          label = "", 
          choices = c("Back", "Hamstrings"),
          selected = "Back",
          width = '50%',
          multiple = FALSE)
      ),
      fluidRow(
        box(
          title = "Max Weight Over Time",
          width = 6,
          plotOutput(ns("plot4"))),
        box(
          title = "Top Exercises",
          width = 6,
          DT::dataTableOutput(ns('data_table2'))
        )
      )
    )
  )
}
    
#' MuscleGroup Server Functions
#'
#' @noRd 
mod_MuscleGroup_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$plot4 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })
    
    output$data_table2 <- DT::renderDT({
      shinipsum::random_DT(5, 3, "numeric")
    })
  })
}
    
## To be copied in the UI
# mod_MuscleGroup_ui("MuscleGroup_ui_1")
    
## To be copied in the server
# mod_MuscleGroup_server("MuscleGroup_ui_1")
