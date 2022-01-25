#' apropos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_apropos_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "A propos",
      width = 12,
      strong("Outil conçu dans le cadre du projet Strat&co", em("Evaluation écononomique et accompagnement stratégique"),
             tags$img(src = "www/logo.jpg", width = "10%")),
      tags$img(src = "www/bandeau4.jpg", width = "70%"),
      tags$img(src = "www/bao.jpg", width = "70%" ),
      
      
      br(),
      
      h3("Comment utiliser Oser"),
      br(),
      p("Ce calculateur 'crash-test' permet de tester la capacité d'un atelier à encaisser une variation de production, de prix de vente ou même de charges.
    Il est à utiliser en complément d'une étude technico-économique classique.", 
        em("Et si mon prix de vente diminue de tant ? Et s'il y a un aléa climatique qui impacte mon rendement de tant ?")),
      p("Le choix du solde final est laissé librement à l'utilisateur en fonction de son besoin. 
    De plus,même si le calculateur est plus approprié à une utilisation sur une échelle atelier, il est également possible de l'adapter aux échelles système de culture ou d'élevage ou encore exploitation agricole. ")
 
  ))
}
    
#' apropos Server Functions
#'
#' @noRd 
mod_apropos_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_apropos_ui("apropos_ui_1")
    
## To be copied in the server
# mod_apropos_server("apropos_ui_1")
