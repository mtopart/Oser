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
               plotlyOutput(ns("graphique"))
        )
      ),
      
      # Gestion de la sidebar------------------------------
      sidebar = boxSidebar(
        id = ns("sidebar"),
        width = 50,
       #background = "#fafafa",
       #style = "color: red",
        
        
        checkboxGroupButtons(
          inputId = ns("data_graph"),
          label = "Label",
          choices = c("Zone de confort" = 1 , 
                      "Quartiles" = 2, 
                      "Courbe" = 3 ),
          status = "primary",
          justified = TRUE,
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"))
        ),
       
       
       
       prettyCheckbox(
         inputId = ns("test_b"),
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
   
   )),

verbatimTextOutput(ns("test")),
verbatimTextOutput(ns("test2"))

  
           


       

        
        # numericInput(ns("vseuil"),
        #              label = "Choix d'une valeur seuil à afficher sur le graphique (par défaut, moyenne du résultat)",
        #              value = 0)
        
        
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
    
    ## Gestion des inputs-----------------
    
    infos_graph <- reactive({
      
      req(!is.null(input$data_graph))
      
      input$data_graph  %>%
         tibble() %>%
         rename(input = 1) %>%
          mutate(type = case_when(
          input == 1 ~ "zconf",
          input == 2 ~ "quart",
          input == 3 ~ "courbe"
        ) )
      
    })
      
      
    
    output$test <- renderPrint({
      "infos_graph()"
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
        layout(title = "Représentation graphique de la répartition du solde choisi<br><sup>Fréquence du solde choisi dans différents intervalles</sup>",
               xaxis = list(title = 'Solde choisi (marge, EBE, revenu, ...) (en € ou k€)'),
               yaxis = list(title = " % des données" ),
               showlegend = FALSE,
               hovermode = 'compare')
    
    })
    
    ### Ajout zone de confort------------------------------
    
    
    
    # zone_conf <-  function(graph){
    #   
    #   graph <- graph %>%
    #     layout(
    #       shapes = list(
    #         list(
    #           type = "rect",
    #           fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
    #           x0 = 10, x2 = 20,  xref = "x",
    #           y0 = 0, y1 = 0.06, yref = "y")
    #       ))
    #   
    #   return(graph)  
    #   
    #   
    # }
    
    
   # observeEvent("zconf" %in% infos_graph(), {
   #   
   #   zone_conf(graph_hist())      
   # }, ignoreNULL = FALSE   ) 
    
    
    
    
    observeEvent(input$test_b, {
      if(input$test_b){
        
        req(result())
        
        graph <- graph_hist()
        
        graph <- graph %>% 
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
    
      output$graphique <- renderPlotly({
        graph
      })
    })
    
  #   observeEvent(input$test_b,{
  #     
  # 
  #     output$test2 <- renderPrint("coucou")}
  #     
  # , ignoreInit = TRUE)

   
  # output$test2 <- renderPrint(input$test_b)
  #  
  #   
    # observeEvent(1 %in% input$data_graph,{
    #   
    #   graph_hist <- graph_hist()
    #   
    #   graph_hist <- graph_hist %>%
    #       layout(
    #         shapes = list(
    #           list(
    #             type = "rect",
    #             fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
    #             x0 = 10, x2 = 20,  xref = "x",
    #             y0 = 0, y1 = 0.06, yref = "y")
    #         ))
    #   })

      

    
    
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
    
    
    # Sortie graphique -------------------
    
    # Définition de l'histogramme -------
    

    
    
#     output$graphique<- renderPlotly({
#       
#       if(input$choix_graph == "histo"){
#         
#         graph_hist()
#         
#         
#         
#         # if (is.null(input$data_graph)) {
#         #   result <- result 
#         #   return(result)
#         # } else if (input$data_graph == 1) {
#         # 
#         #   result <- result %>%
#         #     layout(
#         #       shapes = list(
#         #         list(
#         #           type = "rect",
#         #           fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
#         #           x0 = 10, x2 = 20,  xref = "x",
#         #           y0 = 0, y1 = 0.06, yref = "y")
#         #       ))
#         #   
#         #   return(result)
#         #     } 
#         # 
#         #   
#         # if (is.null(input$data_graph)) {
#         #   return(result)
#         # } else if (input$data_graph == 2) {
#         # 
#         # 
#         #   result <- result %>%
#         #     add_segments(name = "seuil",
#         #                  x = 20,
#         #                  xend = 20,
#         #                  y = 0,
#         #                  yend = 0.08,
#         #                  line = list(dash = "dash", color = "blue"))
#         # 
#         #   return(result)
#         # }
#         # 
# 
#         
#         
#         # Définition de la boite à moustache -----------
#         
#       } else if(input$choix_graph == "bam"){
#         
# graph_bam()
#         
# 
#         
#         
#         # result <- result %>%
#         #   select(solde) %>%
#         #   ggplot(aes(solde)) +
#         #   geom_boxplot(fill = "#77b5fe",
#         #                width = 0.8) +
#         #   coord_cartesian(ylim = c(-1,1)) +
#         #   
#         #   labs(title = "Répartition du solde choisi (marge, EBE, revenu, ...)",
#         #        x ="(en € ou k€)") +
#         #   geom_vline(aes(xintercept=input$vseuil),
#         #              color="blue", linetype="dashed", size=0.8) +
#         #   # theme_light(
#         #   #   base_size = 16
#         #   # ) +
#         #   theme(
#         #     axis.text.y = element_blank(),
#         #     axis.ticks.y = element_blank()
#         #   )
#         # 
#         # ggplotly(result)
#         
#       }
#     })
#     
#   
    
    
    ### Gestion graphique ----------------------------------------------
    

    
    
      observe({
        toggle(id = "zone_conf",
               condition = {1 %in% input$data_graph})
      })




      
    
    
  })
}

## To be copied in the UI
# mod_graph_final_ui("graph_final_ui_1")

## To be copied in the server
# mod_graph_final_server("graph_final_ui_1")