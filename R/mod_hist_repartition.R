#' hist_repartition UI Function
#'
#' @description Module qui gere l'histogramme 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly add_text add_lines add_histogram ggplotly renderPlotly layout config plotlyOutput
#' @importFrom flextable autofit htmltools_value
#' @importFrom stats median quantile
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @importFrom ggplot2 aes after_stat annotate geom_vline scale_fill_manual scale_y_continuous labs theme_bw theme element_text


mod_hist_repartition_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(".textcaption{float:right;}")),
    
    plotlyOutput(ns("graphique_hist")),
    
 
    fluidRow(column(
      12,
      id = ns("caption_zc"),

       div(
           htmlOutput(ns("caption")) ,
              style="text-align:right; color:grey; font-size:13px;",

         class = 'textcaption')
      ),
      
      column(
        12,
        tags$button(
          id = "web_button",
          class = "btn action-button",
          tags$img(src = "www/Image7.png",
                   height = "100px"),
          onclick ="window.open('https://view.genial.ly/650b073107be920019bea9a5', '_blank')"
        ),
      hr(),
      p(strong("Quelques chiffres")),
      htmlOutput(
        ns("texte")),
        
      br(),
      hr(
        id = ns("brr")
      ),
        materialSwitch(
          inputId = ns("detail"),
          label = strong("Pour plus de d\u00e9tails sur la zone de confort"),
          value = FALSE,
          status = "primary"
        ),
      htmlOutput(ns("text_var")),
      br(),
      plotOutput(ns("graphique_variable"),
                   width = "60%",
                 height = "600px")
      
      )
    )
  )
}
    
#' hist_repartition Server Functions
#'
#' @noRd 
mod_hist_repartition_server <- function(id,
                                        r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    # # # Distribution du solde --------------------------------------------------    
    
    result <- reactive({
      
      req(r$dist_pr_graph_production)
      
      crossing(
        production = r$dist_pr_graph_production,
        prix = r$dist_pr_graph_prix,
        charges = r$dist_pr_graph_charges
      )  %>%
        mutate(solde = production * prix - charges)
    })
    
    
    descript <- reactive({
      
      result <- result()
      
      tibble(
        moy = mean(result$solde),
        mediane = median(result$solde),
        nb_val = length(result$solde),
        q1 = quantile(result$solde, 0.25),
        q3 = quantile(result$solde, 0.75),
        interv = q3 -q1 ,
        qA = quantile(result$solde, 0.20),
        qB = quantile(result$solde, 0.80)
      )
      
    })    
    
    
    # Définition des graphiques -------------------------
    
    
    graph_titre <- reactive({
      if(is.null(r$solde) ){
        "R\u00e9partition du solde choisi" } else {
          paste("R\u00e9partition du solde", r$solde, sep = " " )
        } 
    }) 
    
    graph_axe_titre_x <- reactive({
      if(is.null(r$solde) & is.null(r$unit$solde) ){
        "Solde (en \u20ac ou k\u20ac)" } else {
          paste( r$solde,  sep = " " )
        } 
    })   
    
    
    
    
    ## Histogramme de base ----------------------------
    
    
    # graph_hist <- reactive({
    #   req(result())
    #   result <- result()
    #   
    #   
    #   graph_hist_content <- result %>%
    #     ggplot(aes(solde)) +
    #     geom_histogram(aes(y = after_stat(count / sum(count))),
    #                    binwidth = 200,
    #                    show.legend = FALSE,
    #                    alpha = 0.7 ) +
    #     scale_y_continuous(labels = scales::percent) +
    #     labs(
    #       title = graph_titre(),
    #       #subtitle = "R\u00e9partition de la marge ",
    #       x = graph_axe_titre_x(),
    #       y = "Fr\u00e9quence",
    #       fill = ""
    #     ) +
    #     theme_bw() +
    #     theme(
    #       plot.title = element_text(size = 13L,
    #                                 face = "bold"),
    #       plot.subtitle = element_text(size = 12L,
    #                                    face = "italic")
    #     )
    #   
    #   
    #   ### Ajout annotation------------------------------
    #   
    #   hist <- hist(result()$solde, plot = FALSE)
    #   xlim <- range(hist$breaks)
    #   ylim  <- c(0, (ceiling(max(hist$density)*100)/10)-0.03)    
    #   
    #   ### Zone de confort------------------------------
    #   if(r$coche_confort){
    #     
    #     graph_hist_content <- graph_hist_content +
    #       aes(fill = case_when(solde <  r$s_mini  ~ "A",
    #                            solde >  r$s_att ~ "B",
    #                            TRUE ~ "C")) +
    #       scale_fill_manual(
    #         values = c("A" = "red",
    #                    "C" = "orange",
    #                    "B" = "green3")
    #       )+
    #       theme(
    #         legend.position = "none")+
    #       labs(
    #         caption = "Vert = zone de confort\nOrange = zone de vigilance\nRouge = zone critique"
    #       )
    #     
    #     
    #   } else {
    #     graph_hist_content <- graph_hist_content +
    #       aes(fill = "#XXXXX") +
    #       scale_fill_manual(values = c("#XXXXX" = "#77b5fe"))  +
    #       theme(
    #         legend.position = "none")
    #   }
    #   
    #   #### Quartiles -----------------------------------------
    #   
    #   if(r$coche_quart){
    #     
    #     graph_hist_content <- graph_hist_content +
    #       geom_vline(xintercept = descript()$q1,
    #                  color = "orange") +
    #       geom_vline(xintercept = descript()$q3,
    #                  color = "green") +
    #       geom_vline(xintercept = descript()$mediane,
    #                  color = "blue",
    #                  linetype =  "dashed")   +
    #       annotate("text",
    #                x = descript()$mediane,
    #                y = ylim[2]- 0.015,
    #                label = "M\u00e9diane") +
    #       annotate("text",
    #                x = descript()$q1,
    #                y =ylim[2]- 0.015,
    #                label = "Q1") +
    #       annotate("text",
    #                x = descript()$q3,
    #                y =ylim[2]- 0.015,
    #                label = "Q3")
    #     
    #   }
    #   
    #   return(graph_hist_content)
    #   
    # })
    
## Essai graphique -------------------------------------------------------
    
    
    graph_hist_1 <- reactive({
      req(result())
      result <- result()
      
      
      graph_hist_content <- result %>%
        ggplot(aes(solde)) +
        geom_histogram(aes(y = after_stat(count / sum(count))),
                       binwidth = 200,
                       show.legend = FALSE,
                       alpha = 0.7 ) +
        scale_y_continuous(labels = scales::percent) +
        labs(
          title = graph_titre(),
          #subtitle = "R\u00e9partition de la marge ",
          x = graph_axe_titre_x(),
          y = "Fr\u00e9quence",
          fill = ""
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 13L,
                                    face = "bold"),
          plot.subtitle = element_text(size = 12L,
                                       face = "italic")
        )
      
      
   
      
      ### Zone de confort------------------------------
      if(r$coche_confort){
        
        graph_hist_content <- graph_hist_content +
          aes(fill = case_when(solde <  r$s_mini  ~ "A",
                               solde >  r$s_att ~ "B",
                               TRUE ~ "C")) +
          scale_fill_manual(
            values = c("A" = "red",
                       "C" = "orange",
                       "B" = "green3")
          )+
          theme(
            legend.position = "none")+
          labs(
            caption = "Vert = zone de confort\nOrange = zone de vigilance\nRouge = zone critique"
          )
        
        
      } else {
        graph_hist_content <- graph_hist_content +
          aes(fill = "#XXXXX") +
          scale_fill_manual(values = c("#XXXXX" = "#77b5fe"))  +
          theme(
            legend.position = "none")
      }
      
      return(graph_hist_content)  # graph_hist_1
      
    })   
      
      
      #### Quartiles -----------------------------------------
    graph_hist_2_plotly <- reactive({
      req(graph_hist_1())
      graph_hist_content <- graph_hist_1()
      
      
      ### Ajout annotation------------------------------
      
      hist <- hist(result()$solde, plot = FALSE)
      xlim <- range(hist$breaks)
      ylim  <- c(0, (ceiling(max(hist$density)*100)/10)-0.03)  
      
      graph_hist_content <- ggplotly(graph_hist_content)
      
      if(r$coche_quart){
        
        graph_hist_content <- graph_hist_content  %>% 
          add_lines(x =  descript()$q1, 
                    y = ylim, inherit = FALSE,
                    line = list(color = "orange",
                                dash = 'dot')
                    ) %>% 
          add_lines(x =  descript()$mediane, 
                    line = list(color = "blue",
                                dash = 'dash'),
                    y = ylim, inherit = FALSE)%>% 
          add_lines(x =  descript()$q3, 
                    line = list(color = "green",
                                dash = 'dot'),
                    #color =I("green"),
                    y = ylim, inherit = FALSE) %>% 
          add_text( x = descript()$mediane + 100,
                    y = 0,
                    text = "M\u00e9diane", inherit = FALSE)%>% 
          add_text( x = descript()$q1,
                    y =  0,
                    text = "Q1", inherit = FALSE) %>% 
          add_text( x = descript()$q3,
                    y =  0,
                    text = "Q3", inherit = FALSE)
        
      }
      
      return(graph_hist_content) # Graphique pour le plotly
      
    })   
    
    
    graph_hist_2_imp <- reactive({
      req(graph_hist_1())
      graph_hist_content <- graph_hist_1()
      
      
      ### Ajout annotation------------------------------
      
      hist <- hist(result()$solde, plot = FALSE)
      xlim <- range(hist$breaks)
      ylim  <- c(0, (ceiling(max(hist$density)*100)/10)-0.03)  
      
      
      
      
      if(r$coche_quart){
        
        graph_hist_content <- graph_hist_content        +
          geom_vline(xintercept = descript()$q1,
                     color = "orange") +
          geom_vline(xintercept = descript()$q3,
                     color = "green") +
          geom_vline(xintercept = descript()$mediane,
                     color = "blue",
                     linetype =  "dashed") +
          annotate("text",
                   x = descript()$mediane,
                   y = -0.005,
                   label = "M\u00e9diane")  +
          annotate("text",
                   x = descript()$q1,
                   y = -0.005,
                   label = "Q1") +
          annotate("text",
                   x = descript()$q3,
                   y = -0.005,
                   label = "Q3")
        
      }
      
      return(graph_hist_content)  # Sortie pour l'impression
      
    }) 
    
    
    
    
    output$graphique_hist <- renderPlotly({
      w <- graph_hist_2_plotly() %>%    
        layout(dragmode = "select" )%>%  
        config(
          modeBarButtonsToRemove = c('lasso2d',
                                     'zoomIn2d',
                                     'zoomOut2d',
                                     'autoScale2d')
        )
      
      
        for(longueurdata in seq(1, length(w$x$data), 1)){
          w$x$data[[longueurdata]]$text <- paste(r$solde2, "=", w$x$data[[longueurdata]]$x, r$unit_e,
                                                 "<br>",  round(w$x$data[[longueurdata]]$y * 100, digits = 2 ) , "% des valeurs" )

        }
    
      w    
      
    })             
    
    
    
    # #Définition du texte -----------
    
    output$texte <- renderUI({
      
      req(result())
      result <- result()
      
      descript <- descript()
      
      
      moy <- paste0(c("Moyenne du solde (") ,r$select_solde, c(") = "), round(descript$moy)," ", r$unit_e)
      med <- paste0(c("M\u00e9diane (coupe l'\u00e9chantillon en deux parties contenant le m\u00eame nombre de valeurs) = " ), round(descript$mediane), " ", r$unit_e)
      q <- paste0(c("50 % des valeurs sont comprises entre "), round(descript$q1), c(" et "), round(descript$q3)," ", r$unit_e)
      q2 <- paste0(c("80 % des valeurs sont comprises entre "), round(descript$qA), c(" et "), round(descript$qB)," ", r$unit_e)
      
      HTML(paste(moy, med, q, q2, sep = '<br/>'))
      
    })
    
    output$caption <- renderUI({
      HTML("Vert = zone de confort<br/>Orange = zone de vigilance<br/>Rouge = zone critique")
    })
    
    
    
    # #Définition du graphique sur les variables -----------
    

    
    graph_var <- reactive({
      
      req(result())
      
      result <- result()
      
      gener_graph(
        titre_prod = r$titre_production,
        titre_prix = r$titre_prix ,
        titre_charges = r$titre_charges,
      nom_solde = r$select_solde ,
      result = result,
      seuil_mini = r$s_mini,
      seuil_att = r$s_att,
      unite_euros = r$unit_e,
      unite_prod = r$unit_prod,
      unite_prix = r$unit_prix
    )
      })
    
    output$graphique_variable <- renderPlot({
      graph_var() 
    })
    
    text_v <- reactive({"Le graphique suivant permet d'observer comment les variables (production, prix et charges) se comportent au sein de chaque zone."})
    
    
    output$text_var <- renderText({
      text_v()
    }) # Text graph variable-----------
    
    
    observe({
      toggle(id = "detail", condition = r$coche_confort)
      toggle(id = "caption_zc", condition = r$coche_confort)
      toggle(id = "brr", condition = r$coche_confort)
      })
    
    
    observe({ 
      toggle(id = "graphique_variable", condition = input$detail &  r$coche_confort)
      toggle(id = "text_var", condition = input$detail &  r$coche_confort)
    })
 
    # Liens avec les modules --------------------------------
    
    observeEvent( r$button_graph , {
      
      if(r$choix_graph == "histo"){
      
      r$graph_save <- graph_hist_2_imp()
      
      r$graph_var_save <-  graph_var()
      
      r$text_var <- text_v()
      
      }
      
    })    
    
  })
}
    
## To be copied in the UI
# mod_hist_repartition_ui("hist_repartition_1")
    
## To be copied in the server
# mod_hist_repartition_server("hist_repartition_1")
