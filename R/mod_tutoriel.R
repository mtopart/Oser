#' tutoriel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tutoriel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    column(
      width = 12,
      
        # p("Page en construction", style = "color:red ;font-size: 20px;
        #                           font-style: italic"),
    #     br(),
    #   
    # h3("Le choix du solde est laissé au conseiller, en fonction de ses préférences"),
    # p("La signification des résultats est différente en fonction des charges rentrées dans le calculateur."),
    # p(em("Pour mieux comprendre :")),
    # img(src = "www/soldes.jpg", width = "95%")
    
    
    
    h1("Tutoriels"),
    br(),
    p("Oser est un calculateur 'crash-test', qui permet de tester la capacité d'un atelier à encaisser une variation de production, 
    de prix de vente ou même de charges.
    Il est à utiliser en complément d'une étude technico-économique classique.", 
      em("Et si mon prix de vente diminue de tant ? Et s'il y a un aléa climatique qui impacte mon rendement de tant ?")),
    p("Le choix du solde final est laissé librement à l'utilisateur en fonction de son besoin. 
    De plus,même si Oser est plus approprié à une utilisation sur une échelle atelier, 
      il est également possible de l'adapter aux échelles système de culture ou d'élevage ou encore exploitation agricole. "),
    hr(),
    h2("Utilisation d'Oser"),
    h5(strong("Saisie des unités")),
    htmlOutput(ns("unite")),
    br(),
    h5(strong("Variables et distribution")),
    htmlOutput(ns("distribution")),
    br(),
    h5(strong("Graphiques de sortie")),
    htmlOutput(ns("graphiques")),
    br(),
    h5(strong("Enregistrement des sorties")),
    hr(),
    h2("Mieux comprendre les indicateurs économiques"),
    p("A venir"),
    hr(),
    h2("Comment fonctionne Oser ?"),
    p("A venir")
 
  )))
}
    
#' tutoriel Server Functions
#'
#' @noRd 
mod_tutoriel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$distribution <- renderUI({
  HTML(
    '<div style="width: 80%;"><div style="position: relative; padding-bottom: 56.25%;
      padding-top: 0; height: 0;"><iframe title="Tutoriel choix de la distribution" frameborder="0"
      width="1200" height="675" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
      src="https://view.genial.ly/650b071caad55800184bb5cd" type="text/html" allowscriptaccess="always" 
      allowfullscreen="true" scrolling="yes" allownetworking="all"></iframe>
      </div> </div>')
    })
 
    output$unite<- renderUI({
      HTML(
        '<div style="width: 80%;"><div style="position: relative; padding-bottom: 56.25%; padding-top: 0;
        height: 0;"><iframe title="Tutoriel saisie unités" frameborder="0" width="1200" height="675" 
        style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;" 
        src="https://view.genial.ly/650af2df6c8849001028e14f" type="text/html" allowscriptaccess="always" 
        allowfullscreen="true" scrolling="yes" allownetworking="all"></iframe> </div> </div>')
    })
    
    output$graphiques <- renderUI({
      HTML(
        '<div style="width: 80%;"><div style="position: relative; padding-bottom: 56.25%; padding-top: 0;
        height: 0;"><iframe title="Tutoriel Graphiques" frameborder="0" width="1200" height="675" 
        style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;" 
        src="https://view.genial.ly/650b073107be920019bea9a5" type="text/html" allowscriptaccess="always" allowfullscreen="true" 
        scrolling="yes" allownetworking="all"></iframe> </div> </div>')
    })
    
    
  })
}
    
## To be copied in the UI
# mod_tutoriel_ui("tutoriel_ui_1")
    
## To be copied in the server
# mod_tutoriel_server("tutoriel_ui_1")
