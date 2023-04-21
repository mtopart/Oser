#' telechargement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom officer read_docx body_add_gg body_add_par 
#' @importFrom flextable body_add_flextable
#' 
mod_telechargement_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    box(
      title = "Enregistrement des données",
      width = 12,
      
      
      
    strong(style = "color:red ;font-size: 20px;
                                 font-style: italic","En construction"),
    fluidRow(
      
      column(
      6,
    textInput(ns("nom_prenom"), 
              "Nom - Prénom / Désignation")),
    
    column(
      6,
    textInput(ns("titre_analyse"), 
              "Contexte d'analyse")),
    
    
    textAreaInput(
      inputId = ns("com_hist"),
      label = NULL,
      width = '100%',
      height = '150px',
      placeholder = "Commentaires"
    ),
    
    # textAreaInput(
    #   inputId = ns("com_mat"),
    #   label = NULL,
    #   width = '100%',
    #   height = '150px',
    #   placeholder = "Commentaires"
    # ),
    
   column(
     12,

    actionButton(ns("select_graph"), 
                 "Enregistrer le graphique et les commentaires",
                 icon("save")
                 ),
    
    
    br(),
    br(),
    downloadButton(ns("dl_graph"), "Télécharger le compte-rendu")
    # ,
    # verbatimTextOutput(ns("test")),
    # plotlyOutput(ns("test2"))
  ))
    )
  )
}
    
#' telechargement Server Functions
#'
#' @noRd 
mod_telechargement_server <- function(id, 
                                      r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    


# graph_word <- function(gg_hist,
#                        tabl,
#                        doc_word){
#   
#   doc_word <- doc_word %>% 
#     body_add_par(value = "", style = "heading 1") %>% 
#     body_add_par(value = "Résultats", style = "heading 2") %>% 
#     body_add_gg(value = gg_hist,
#                 style = "Normal") %>% 
#     body_add_par(value = input$com_hist, style = "Normal") #%>% 
#     #body_add_flextable(tabl) %>% 
#     #body_add_par(value = "Récapitulatif des éléments de saisie", style = "heading 2") 
#   
# }
#     
#     doc <- reactive({
#      read_docx() %>%
#         body_add_par(value = "Sorties Oser - Compte-rendu", style = "centered") %>%
#         body_add_par(value = input$titre_analyse, style = "centered") %>% 
#         body_add_par(value = input$nom_prenom, style = "centered") 
#     })
#     
# 
# 
#     observeEvent(input$select_graph,{
#       
#       r$button_graph <- input$select_graph
#     })
#       
#       observeEvent(r$button_graph,{  
#       graph_word(gg_hist =  r$graph_save,
#                  tabl = r$tabl_save,
#                  doc_word = doc())
#     })

    
    # output$test <- renderPrint({
    #   r$graph_save
    # })
    # 
    # output$test2 <- renderPlotly({
    #   ggplotly(r$graph_save)
    #   
    # })

    
    output$dl_graph <- downloadHandler(
      filename = function() {
        paste0("graphiques", Sys.Date(), ".docx")
      },
      content = function(file) {
        print(doc(), target = file)
      }
    )
    
    
  })
}
 
## To be copied in the UI
# mod_telechargement_ui("telechargement_1")
    
## To be copied in the server
# mod_telechargement_server("telechargement_1")
