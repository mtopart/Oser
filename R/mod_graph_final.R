#' graph_final UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly ggplotly plotlyOutput renderPlotly plot_ly add_segments add_trace layout
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom shinyWidgets actionBttn radioGroupButtons
#' @importFrom dplyr select mutate %>%
#' @importFrom prompter add_prompt use_prompt
#' @importFrom ggplot2 ggplot aes geom_histogram labs geom_boxplot geom_vline theme_light coord_cartesian theme element_blank
#' 

mod_graph_final_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    use_prompt(),
    
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
      
      numericInput(ns("vseuil"),
                   label = "Choix d'une valeur seuil à afficher sur le graphique (par défaut, moyenne du résultat)",
                   value = 0),
      
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
    
    
    
    
    # #Définition du texte -----------
    
    output$texte <- renderText({
      
      
      req(result())
      result <- result()
      
      descript <- tibble(
        moy = mean(result$solde),
        mediane = median(result$solde),
        nb_val = length(result$solde),
        q1 = quantile(result$solde, 0.25),
        q3 = quantile(result$solde, 0.75),
        interv = q3 -q1
      )
      paste(c("La moyenne de l'échantillon calculé est de"), round(descript$moy), c("euros."),
            c("La médiane coupe l'échantillon en deux parties contenant le même nombre de valeurs. Elle est de"), round(descript$mediane), c("euros."),
            c("1/4 des valeurs de l'échantillon sont inférieures à"), round(descript$q1), c("euros et 1/4 sont supérieures à"), round(descript$q3), c("euros."),
            sep = " ")
      
      
    })
    
    
    # Sortie graphique -------------------
    
    # Définition de l'histogramme -------
    
    observe({
      
      moy <- round(mean(result()$solde), digits = 0 )
      
      updateNumericInput(session,
                         "vseuil",
                         value = moy)
      
    })
    
    
    output$graphique<- renderPlotly({
      
      if(input$choix_graph == "histo"){
        
        result <- result()
        
        
        
        result <- plot_ly() %>%
          add_trace(name = "histogramme",
                    data = result,
                    x = ~ solde,
                    type = "histogram",
                    histnorm = "probability",
                    marker = list(color = "#77b5fe"),
                    nbinsx = 50) %>%
          add_segments(name = "seuil",
                       x = input$vseuil,
                       xend = input$vseuil,
                       y = 0,
                       yend = 0.08,
                       line = list(dash = "dash", color = "blue"))  %>%
          layout(title = "Représentation graphique de la répartition du solde choisi<br><sup>Fréquence du solde choisi dans différents intervalles</sup>",
                 xaxis = list(title = 'Solde choisi (marge, EBE, revenu, ...) (en € ou k€)'),
                 yaxis = list(title = " % des données" ),
                 showlegend = FALSE)
        
        # Définition de la boite à moustache -----------
        
      } else if(input$choix_graph == "bam"){
        
        result <- result()
        
        result <- result %>%
          select(solde) %>%
          ggplot(aes(solde)) +
          geom_boxplot(fill = "#77b5fe",
                       width = 0.8) +
          coord_cartesian(ylim = c(-1,1)) +
          
          labs(title = "Répartition du solde choisi (marge, EBE, revenu, ...)",
               x ="(en € ou k€)") +
          geom_vline(aes(xintercept=input$vseuil),
                     color="blue", linetype="dashed", size=0.8) +
          # theme_light(
          #   base_size = 16
          # ) +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
        
        ggplotly(result)
        
      }
    })
    
    
    
    
  })
}

## To be copied in the UI
# mod_graph_final_ui("graph_final_ui_1")

## To be copied in the server
# mod_graph_final_server("graph_final_ui_1")