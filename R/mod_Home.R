#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(width = 5,
               h2("Welcome!")),
        br(),br(),br()
      ),
      fluidRow(
        column(width = 10,
               box(
                 background = "blue",
                 h2("You lifted XYZ xx times!"),
                 title = "Congrats!"),
               br(), br(), br(), br()
        )),
      fluidRow(
        box(
          title = "Top Exercises",
          width = 6,
          DT::dataTableOutput(ns('data_table'))),
        box(
          title = "Total Weight Lifted",
          width = 6,
          plotOutput(ns("plot"))
        )
      )
    )
  )
}
    
#' Home Server Functions
#'
#' @noRd 
mod_Home_server <- function(id){
  
  moduleServer(id, function(input, output, session){

    ns <- session$ns
    
    output$data_table <- DT::renderDT({
      shinipsum::random_DT(5, 3, "numeric")
    })
    
    output$plot <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })
  })
}
    
## To be copied in the UI
# mod_Home_ui("Home_ui_1")
    
## To be copied in the server
# mod_Home_server("Home_ui_1")
