#' graph_final UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly ggplotly plotlyOutput renderPlotly plot_ly add_segments add_trace layout config style  add_annotations
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom shinyjs toggle useShinyjs
#' @importFrom shinyWidgets actionBttn radioGroupButtons checkboxGroupButtons prettyCheckbox
#' @importFrom dplyr select mutate %>% rename case_when filter
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
      title = uiOutput(ns("titre")),
      width = 12,
      icon = tags$span(icon("question"))
      %>%
        add_prompt(
          position = "right",
          message = "Voir onglet 'Tutoriels' - en construction",
          type = "info")
      ,
      


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
                500,
                width ='100%' )),
   
   textOutput(ns("texte_pourcent")) 
   
   )),

hr(),

prettyCheckbox(
  inputId = ns("coche_quart"),
  label = "Quartiles", 
  value = FALSE,
  icon = icon("check"),
  status = "success"
),

htmlOutput(ns("texte"))

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
    

  graph_titre <- reactive({
      if(is.null(r$solde) ){
        "Répartition du solde choisi" } else {
          paste("Répartition de", r$solde, sep = " " )
        } 
    }) 
    
    
    graph_axe_titre_x <- reactive({
      if(is.null(r$solde) & is.null(r$unit$solde) ){
        "Valeurs du solde choisi (marge, EBE, revenu, ...) (en € ou k€)" } else {
          paste("Valeurs de", r$solde , "en", r$unit_solde , sep = " " )
        } 
    })   
    
    
    graph_axe_titre_y <- reactive({
      if(is.null(r$solde) ){
        "Fréquence des valeurs (en %)" } else {
          paste("Fréquence des valeurs de", r$solde, "(en %)", sep = " " )
        } 
    }) 
    
    
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
        layout(title = graph_titre(),
               xaxis = list(title = graph_axe_titre_x()),
               yaxis = list(title = graph_axe_titre_y() ),
               showlegend = FALSE,
               hovermode = 'compare') %>% 
        config(editable = TRUE)
      
    })
    
    ### Ajout annotation------------------------------
    
    
    
    observe({
      
      req(result())
      
      graph_hist <- graph_hist()
      
      
      
      hist <- hist(result()$solde, plot = FALSE)
      xlim = range(hist$breaks)
      ylim  = c(0, (ceiling(max(hist$density)*100)/10)-0.03)
      
      
    ####  zone de confort ---------------------------------------  
      
      if(input$coche_confort){
        
        graph_hist <- graph_hist %>% 
          layout(
            shapes = list(
              list(
                type = "rect",
                fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                x0= xlim[1], x1 = input$s_mini,  xref = "x",
                y0 = ylim[1], y1 = ylim[2], yref = "y"),
              list(
                type = "rect",
                fillcolor = "orange", line = list(color = "orange"), opacity = 0.1,
                x0= input$s_mini, x1 = input$s_att,  xref = "x",
                y0 = ylim[1], y1 = ylim[2], yref = "y")
            )
          )
      }  
      
   #### Quartiles -----------------------------------------
      
      if(input$coche_quart){
        
        graph_hist <- graph_hist %>% 
          add_segments(x = descript()$q1, xend =  descript()$q1, 
                       y = ylim[1], yend = ylim[2], 
                       linetype = "bot") %>% 
          add_segments(x = descript()$q3, xend =  descript()$q3, 
                       y = ylim[1], yend = ylim[2], 
                       linetype = "bot") %>% 
          add_segments(x = descript()$mediane, xend =  descript()$mediane, 
                       y = ylim[1], yend = ylim[2], 
                       line = list(dash = "dash", color = "blue"))  %>% 
          add_annotations(
            x = descript()$mediane, y = ylim[2]- 0.015, xref = "x", yref = "y",
            text = "Médiane", xanchor = 'right',
            showarrow = T, arrowhead = 4, arrowsize = .5,
            ax = 20, ay = -40
          ) %>% 
          add_annotations(
            x = descript()$q1, y = ylim[2]- 0.015, xref = "x", yref = "y",
            text = "Q1", xanchor = 'right',
            showarrow = T, arrowhead = 4, arrowsize = .5,
            ax = 20, ay = -40
          )  %>% 
          add_annotations(
            x = descript()$q3, y = ylim[2]- 0.015, xref = "x", yref = "y",
            text = "Q3", xanchor = 'right',
            showarrow = T, arrowhead = 4, arrowsize = .5,
            ax = 20, ay = -40
          )
          
      }   
      
      ### Sortie Output------------------------------
      
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
    
    output$texte <- renderUI({
      
      
      req(result())
      result <- result()
      
      descript <- descript()
      
      # paste(c("La moyenne de l'échantillon calculé est de"), round(descript$moy), c("euros."),
      #       c("La médiane coupe l'échantillon en deux parties contenant le même nombre de valeurs. Elle est de"), round(descript$mediane), c("euros."),
      #       c("1/4 des valeurs de l'échantillon sont inférieures à"), round(descript$q1), c("euros et 1/4 sont supérieures à"), round(descript$q3), c("euros."),
      #       sep = " ")
      
      moy <- paste0(c("Moyenne = " ), round(descript$moy), c(" € ou K€"))
      med <- paste0(c("Médiane (coupe l'échantillon en deux parties contenant le même nombre de valeurs) = " ), round(descript$mediane), c(" € ou K€"))
      q <- paste0(c("50 % des valeurs sont comprises entre "), round(descript$q1), c(" et "), round(descript$q3), c(" euros.") )
      
      HTML(paste(moy, med, q, sep = '<br/>'))
      
    
      
      
    })
    
    
  pc_mini <- reactive({
  
    nb_result <- nrow(result())
    
    nb_mini <- result() %>% 
      filter(solde  > input$s_mini) %>% 
      nrow()
    
    pc_mini <- nb_mini*100/nb_result
    pc_mini <- round(pc_mini,digits = 0)
    
    pc_mini
 
    
  })
  
  pc_att <- reactive({
    
    nb_result <- nrow(result())
    
    nb_att <- result() %>% 
      filter(solde  > input$s_att) %>% 
      nrow()
    
    pc_att <- nb_att*100/nb_result
    pc_att <- round(pc_att,digits = 0)
    
    pc_att
    
    
  })
  
      

      
      output$texte_pourcent <- renderText({
      
    mini <- pc_mini()
    
    att <- pc_att()
    
    paste( mini, c("% des valeurs sont au-dessus du seuil minimum défini et"),
           att, c("% des valeurs sont au-dessus du seuil attendu défini"),
           sep = " ")
      
      
      }) 
      
  
    
    
    
    
    
    

    
    ### Gestion graphique ----------------------------------------------
    




observe({
  toggle(id = "zone_conf", condition = input$coche_confort)
  toggle(id = "graphique_hist", condition = input$choix_graph == "histo")
  toggle(id = "graphique_bam", condition = input$choix_graph == "bam")
})

      
  ## Gestion du titre--------------------------------------------------
      
      
      gest_text <- reactive({
        
          if(is.null(r$solde)){
            "Conséquences sur le solde choisi (marge, EBE, revenu...)" } else {
              paste("Conséquences sur ",  r$solde, sep = "" )
            } 
        
      })
      
      output$titre <- renderUI( gest_text()   )       
      
      
      
      
    
  })
}

## To be copied in the UI
# mod_graph_final_ui("graph_final_ui_1")

## To be copied in the server
# mod_graph_final_server("graph_final_ui_1")