#' zone_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_zone_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    
    use_prompt(),
    # useShinyjs(),
    
    column(
      12,
      align = "center",
      actionBttn(
        inputId = ns("goButton"),
        label = "Go!",
        style = "gradient",
        color = "primary"
      ) ,
      
      br(),
      br()),
    
    box(
      title = uiOutput(ns("titre")),
      width = 12,
      # icon = 

      #   tags$span(icon("question"))
      # %>%
      #   add_prompt(
      #     position = "right",
      #     message = "Voir onglet 'Tutoriels' - en construction",
      #     type = "info"),
        # 
        # tooltip(
        #   tags$span(icon("question")),
        #   title =  "Voir onglet 'Tutoriels' - en construction"),
      
      br(),
      
      ## Choix graph--------------------------------------------------
      fluidRow(
        column(12,
               
               div("Pour positionner les valeurs seuils, cliquez sur", 
                   icon("cogs", style = "color:grey;") )),
        
        column(6,
               br(),
               radioGroupButtons(
                 inputId = ns("choix_graph"),
                 label = "Choix du graphique",
                 choiceNames = list(paste("Répartition"),
                                    "Matrice de gain"),
                 choiceValues = list(
                   "histo",  "mat"),
                 justified = TRUE,
                 checkIcon = list(
                   yes = icon("ok",
                              lib = "glyphicon"))
               )),
        
        column(12,
               div(id = ns("g1"),
                   mod_hist_repartition_ui(ns("hist_repartition_1"))
                   ),
               div(id = ns("g2"),
                   mod_matrice_gain_ui(ns("matrice_gain_1")))
        )),
      
      sidebar = boxSidebar(
        id = ns("sidebar"),
        width = 50,
        background = "#fafafa",
        style = "color: black",
        
        
        prettyCheckbox(
          inputId = ns("coche_confort"),
          label = "Afficher la zone de confort",
          value = FALSE,
          icon = icon("check"),
          status = "success"
        ),
        
        
        
        wellPanel(id = ns("zone_conf"),
                  
                  fluidRow(
                    column(
                      width = 6,
                      
                      numericInput(ns("s_mini"),
                                   "Solde minimum",
                                   1,
                                   width ='100%' )),
                    
                    column(
                      width = 6,
                      numericInput(ns("s_att"),
                                   "Solde attendu",
                                   500,
                                   width ='100%' ))
                  )),
        
        hr(),
        
        prettyCheckbox(
          inputId = ns("coche_quart"),
          label = " Afficher les quartiles",
          value = FALSE,
          icon = icon("check"),
          status = "success"
        ),
        
        tags$button(
          id = "web_button",
          class = "btn action-button",
          tags$img(src = "www/Image7.png",
                   height = "100px"),
          onclick ="window.open('https://view.genial.ly/650b073107be920019bea9a5', '_blank')"
        ),
      )  
      
    )
 
  )
}
    
#' zone_graph Server Functions
#'
#' @noRd 
mod_zone_graph_server <- function(id,
                                  r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    observeEvent( input$goButton, {
      r$go_button <- input$goButton
    })
    
    # observe({
    #   req(r$dist_pr_graph_production)
    #   updateBoxSidebar("sidebar")
    # })  
    
    
    
    gest_text <- reactive({
      
      if(is.null(r$solde)){
        "Conséquences sur le solde choisi (marge, EBE, revenu...)" } else {
          paste("Conséquences sur le solde ",  r$solde, sep = "" )
        } 
      
    })
    
    output$titre <- renderUI( gest_text()   )  
    
    
    observe({
      r$coche_confort <- input$coche_confort
      r$coche_quart <- input$coche_quart
    })
    
    observe({
      r$s_mini   <-    input$s_mini
      r$s_att  <-    input$s_att
    })
    
    
    observe({
      toggle(id = "g1", condition = input$choix_graph == "histo")
      toggle(id = "g2", condition = input$choix_graph == "mat"  )
      })
    
    
      observeEvent(input$choix_graph,{
        r$choix_graph <- input$choix_graph
        })

    
    mod_hist_repartition_server("hist_repartition_1",
                                r = r)
    
    mod_matrice_gain_server("matrice_gain_1",
                            r = r)
    
  })
}
    
## To be copied in the UI
# mod_zone_graph_ui("zone_graph_1")
    
## To be copied in the server
# mod_zone_graph_server("zone_graph_1")
