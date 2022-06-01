#' graph_final UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly ggplotly plotlyOutput renderPlotly plot_ly add_segments add_trace layout config style
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom shinyjs toggle useShinyjs
#' @importFrom shinyWidgets actionBttn radioGroupButtons checkboxGroupButtons prettyCheckbox
#' @importFrom dplyr select mutate %>% rename case_when
#' @importFrom prompter add_prompt use_prompt
#' @importFrom ggplot2 ggplot aes geom_histogram labs geom_boxplot geom_vline theme_light coord_cartesian theme element_blank
#' 

mod_graph_final_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    use_prompt(),
    useShinyjs(),
    
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
      title = "Conséquences sur le solde choisi (marge, EBE, revenu...)",
      width = 12,
      icon = tags$span(icon("question"))
      %>%
        add_prompt(
          position = "right",
          message = "Voir onglet 'Tutoriels' - en construction",
          type = "info")
      ,
      
      textOutput(ns("texte")),

      br(),
      
      ## Choix graph--------------------------------------------------
      fluidRow(
        column(6,
               radioGroupButtons(
                 inputId = ns("choix_graph"),
                 label = "Choix du graphique",
                 choiceNames = list("Histogramme",
                                    "Boite à moustache"),
                 choiceValues = list(
                   "histo", "bam"
                 ),
                 justified = TRUE,
                 checkIcon = list(
                   yes = icon("ok",
                              lib = "glyphicon"))
               )),
        
        
        column(12,
               plotlyOutput(ns("graphique_hist")),
              plotlyOutput(ns("graphique_bam"))
      )),
      
      # Gestion de la sidebar------------------------------
      sidebar = boxSidebar(
        id = ns("sidebar"),
        width = 50,
       background = "#fafafa",
       style = "color: black",
        
        
       prettyCheckbox(
         inputId = ns("coche_confort"),
         label = "Zone de confort", 
         value = FALSE,
         icon = icon("check"),
         status = "success"
       ),
       

    wellPanel(id = ns("zone_conf"),
              
fluidRow(
              
    column(   
      width = 6,
              
   numericInput(ns("s_mini"), 
                "Seuil minimum", 
                1,
                width ='100%' )),
   
   column(   
     width = 6,
   numericInput(ns("s_att"), 
                "Résultat attendu",
                1,
                width ='100%' ))
   
   ))
      )  
    )    
    
  )
   
}

#' graph_final Server Functions
#'
#' @noRd 
mod_graph_final_server <- function(id,
                                   r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent( input$goButton, {
      r$button <- input$goButton
    })
    
    
    
    # # # Distribution du solde --------------------------------------------------    
    
    result <- reactive({
      
      req(r$dist_pr_graph_production)
      
      crossing(
        production = r$dist_pr_graph_production,
        prix = r$dist_pr_graph_prix,
        charges = r$dist_pr_graph_charges 
      )  %>%
        mutate(ca = production * prix,
               solde = ca - charges)
    })   
    
    
    descript <- reactive({
      
      result <- result()
      
      tibble(
        moy = mean(result$solde),
        mediane = median(result$solde),
        nb_val = length(result$solde),
        q1 = quantile(result$solde, 0.25),
        q3 = quantile(result$solde, 0.75),
        interv = q3 -q1
      )
      
    })
    
# Définition des graphiques -------------------------
    

    ## Histogramme de base ----------------------------
    
    graph_hist <- reactive({
      req(result())
      result <- result()
      
      graph_hist <- plot_ly() %>%
        add_trace(name = "histogramme",
                  data = result,
                  x = ~ solde,
                  type = "histogram",
                  histnorm = "probability",
                  marker = list(color = "#77b5fe"),
                  nbinsx = 40) %>%    
        config(
          modeBarButtonsToRemove = c('lasso2d', 
                                     'zoomIn2d', 
                                     'zoomOut2d',
                                     'autoScale2d') 
        ) %>% 
        layout(title = "Représentation graphique de la répartition du solde choisi<br><sup>Fréquence du solde choisi dans différents intervalles</sup>",
               xaxis = list(title = 'Solde choisi (marge, EBE, revenu, ...) (en € ou k€)'),
               yaxis = list(title = " % des données" ),
               showlegend = FALSE,
               hovermode = 'compare')
      
    })
    
    ### Ajout zone de confort------------------------------
    
    
    
    observe({
      
      req(result())
      
      graph_hist <- graph_hist()
      
      
      if(input$coche_confort){
        
        graph_hist <- graph_hist %>% 
          layout(
            shapes = list(
              list(
                type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0= -10, x1 = 50,  xref = "x",
                y0 = 0, y1 = 0.1, yref = "y")
            )
          )
      }  
      
      output$graphique_hist <- renderPlotly({
        graph_hist
      })
    })  
    
    ## Violon / bam de base -----------------------------
    
    graph_bam <- reactive({
      req(result())
      result <- result()
    
      
      
      result <- result() %>% 
        
        plot_ly(
          y = ~solde,
          type = 'violin',
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          ),
          x0 = 'Répartition du solde choisi (marge, EBE, revenu, ...)l'
        ) %>% 
        layout(
          yaxis = list(
            title = "",
            zeroline = F
          ) 
        ) %>% 
        style(hoverinfo = 'none')
      
      
      
    })
      
    
    output$graphique_bam <- renderPlotly({
      graph_bam()
    })   
      
    
    # #Définition du texte -----------
    
    output$texte <- renderText({
      
      
      req(result())
      result <- result()
      
      descript <- descript()
      
      paste(c("La moyenne de l'échantillon calculé est de"), round(descript$moy), c("euros."),
            c("La médiane coupe l'échantillon en deux parties contenant le même nombre de valeurs. Elle est de"), round(descript$mediane), c("euros."),
            c("1/4 des valeurs de l'échantillon sont inférieures à"), round(descript$q1), c("euros et 1/4 sont supérieures à"), round(descript$q3), c("euros."),
            sep = " ")
      
      
    })
    
    

    
    ### Gestion graphique ----------------------------------------------
    




observe({
  toggle(id = "zone_conf", condition = input$coche_confort)
  toggle(id = "graphique_hist", condition = input$choix_graph == "histo")
  toggle(id = "graphique_bam", condition = input$choix_graph == "bam")
})

      
    
    
  })
}

## To be copied in the UI
# mod_graph_final_ui("graph_final_ui_1")

## To be copied in the server
# mod_graph_final_server("graph_final_ui_1")