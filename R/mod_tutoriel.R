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
    # h3("Le choix du solde est laiss\\u00e9 au conseiller, en fonction de ses pr\\u00e9f\\u00e9rences"),
    # p("La signification des r\\u00e9sultats est diff\\u00e9rente en fonction des charges rentr\\u00e9es dans le calculateur."),
    # p(em("Pour mieux comprendre :")),
    # img(src = "www/soldes.jpg", width = "95%")
    
    
    
    h1("Tutoriels"),
    br(),
    p("Oser est un calculateur 'crash-test', qui permet de tester la capacit\\u00e9 d'un atelier \\u00e0 encaisser une variation de production, 
    de prix de vente ou m\\u00eame de charges.
    Il est \\u00e0 utiliser en compl\\u00e9ment d'une \\u00e9tude technico-\\u00e9conomique classique.", 
      em("Et si mon prix de vente diminue de tant ? Et s'il y a un al\\u00e9a climatique qui impacte mon rendement de tant ?")),
    p("Le choix du solde final est laiss\\u00e9 librement \\u00e0 l'utilisateur en fonction de son besoin. 
    De plus,m\\u00eame si Oser est plus appropri\\u00e9 \\u00e0 une utilisation sur une \\u00e9chelle atelier, 
      il est \\u00e9galement possible de l'adapter aux \\u00e9chelles syst\\u00e8me de culture ou d'\\u00e9levage ou encore exploitation agricole. "),
    hr(),
    h2("Utilisation d'Oser"),
    h5(strong("Saisie des unit\\u00e9s")),
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
    h2("Mieux comprendre les indicateurs \\u00e9conomiques"),
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
        height: 0;"><iframe title="Tutoriel saisie unit\\u00e9s" frameborder="0" width="1200" height="675" 
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
