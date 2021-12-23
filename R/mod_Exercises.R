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

      fluidPage(
        fluidRow(
          h2("Exercise Progress for "),
          selectizeInput(
            inputId = ns("exercises"), 
            label = "",
            # Hard coded for now, but we'll deal with this later!
            choices = c("Dumbell Bicep Curl", "Deadlift"),
            selected = "Dumbell Bicep Curl",
            width = '50%',
            multiple = FALSE)
        ),
        fluidRow(
          column(width = 10,
                 shinydashboard::box(
                   background = "blue",
                   h2("Your current projected 1 rep max is x!"),
                   title = "Congrats!")
          )),
        fluidRow(
          shinydashboard::box(
            title = "Total Weight Per Workout",
            width = 6,
            plotOutput(ns("plot2"))),
          shinydashboard::box(
            title = "Max Weight Per Workout",
            width = 6,
            plotOutput(ns("plot3"))
          )
        )
      )
    )
}
    
#' Exercises Server Functions
#'
#' @noRd 
mod_Exercises_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$plot2 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })
    
    output$plot3 <- renderPlot({
      shinipsum::random_ggplot(type = "line")
    })
    
  })
}
    
## To be copied in the UI
# mod_Exercises_ui("Exercises_ui_1")
    
## To be copied in the server
# mod_Exercises_server("Exercises_ui_1")
