#' library(shiny)
#' library(shinyWidgets)
#' 
#' 
#' mod_telechargement_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     strong(style = "color:red ;font-size: 20px;
#'                                  font-style: italic","En construction"),
#'     actionBttn(
#'       inputId = ns("select_graph"),
#'       label = "Sélectionnez le graphique",
#'       style = "bordered",
#'       color = "success"
#'     ),
#'     br(),
#'     br(),
#'     downloadButton(ns("dl_graph"), "Télécharger le récapitulatif")
#'     # ,
#'     # verbatimTextOutput(ns("test")),
#'     # plotlyOutput(ns("test2"))
#'   )
#' }
#' 
#' #' telechargement Server Functions
#' #'
#' #' @noRd 
#' mod_telechargement_server <- function(id, 
#'                                       r){
#'   moduleServer( id, function(input, output, session){
#'     ns <- session$ns
#'     
#'     
#'     
#'     graph_word <- function(gg,
#'                            doc_word){
#'       
#'       doc_word <- doc_word %>%
#'         body_add_gg(value = gg,
#'                     style = "Normal")
#'       
#'     }
#'     
#'     doc <- reactive({
#'       read_docx() %>%
#'         body_add_par(value = "Sauvegarde des graphiques", style = "heading 1") 
#'       # %>%
#'       # 
#'       #   body_add_par(value = "Contexte général", style = "heading 2")
#'       
#'     })
#'     
#'     
#'     
#'     observeEvent(input$select_graph,{
#'       
#'       r$button_graph <- input$select_graph
#'     })
#'     
#'     observeEvent(r$button_graph,{  
#'       graph_word(gg =  r$graph_save,
#'                  doc_word = doc())
#'     })
#'     
#'     
#'     # output$test <- renderPrint({
#'     #   r$graph_save
#'     # })
#'     # 
#'     # output$test2 <- renderPlotly({
#'     #   ggplotly(r$graph_save)
#'     #   
#'     # })
#'     
#'     
#'     output$dl_graph <- downloadHandler(
#'       filename = function() {
#'         paste0("graphiques", Sys.Date(), ".docx")
#'       },
#'       content = function(file) {
#'         print(doc(), target = file)
#'       }
#'     )
#'     
#'     
#'   })
#' }
#' 
#' 
#' 
#' 
#' 
#' app_ui <- tagList(
#'   fluidPage(
#'     
#'     mod_telechargement_ui("telechargement_1")
#'     
#'   )
#'   
#'   
#' )
#' 
#' 
#' 
#' server <- function( input, output, session ) {
#'   r <- reactiveValues()
#'   
#'   mod_telechargement_server("telechargement_1")
#'   })
#'   
#' }
#' shinyApp(ui, server)
#' 
