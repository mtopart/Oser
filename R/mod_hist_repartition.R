#' hist_repartition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly add_text add_lines add_histogram
#' @importFrom flextable autofit htmltools_value
#' @importFrom stats median quantile


mod_hist_repartition_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("graphique_hist")),
    
 
    fluidRow(column(
      12,
      # p("Blblabl", style = "color:red ;font-size: 20px;
      #                            font-style: italic"
      # ),
      # br(),
      p(strong("Quelques chiffres")),
      htmlOutput(
        ns("texte")),
        
      br(),
      
        materialSwitch(
          inputId = ns("detail"),
          label = strong("Pour plus de détails sur la zone de confort"),
          value = FALSE,
          status = "primary"
        ),
    
      
      plotlyOutput(ns("graphique_variable"),
                   width = "50%")
      
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
        interv = q3 -q1
      )
      
    })    
    
    
    # Définition des graphiques -------------------------
    
    
    graph_titre <- reactive({
      if(is.null(r$solde) ){
        "Répartition du solde choisi" } else {
          paste("Répartition du solde", r$solde, sep = " " )
        } 
    }) 
    
    graph_axe_titre_x <- reactive({
      if(is.null(r$solde) & is.null(r$unit$solde) ){
        "Solde (en € ou k€)" } else {
          paste( r$solde,  sep = " " )
        } 
    })   
    
    
    
    
    ## Histogramme de base ----------------------------
    
    
    graph_hist <- reactive({
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
          #subtitle = "Répartition de la marge ",
          x = graph_axe_titre_x(),
          y = "Fréquence",
          fill = ""
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 15L,
                                    face = "bold"),
          plot.subtitle = element_text(size = 13L,
                                       face = "italic")
        )
      
      
      ### Ajout annotation------------------------------
      
      hist <- hist(result()$solde, plot = FALSE)
      xlim <- range(hist$breaks)
      ylim  <- c(0, (ceiling(max(hist$density)*100)/10)-0.03)    
      
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
            legend.position = "none")
        
        
      } else {
        graph_hist_content <- graph_hist_content +
          aes(fill = "#XXXXX") +
          scale_fill_manual(values = c("#XXXXX" = "#77b5fe"))  +
          theme(
            legend.position = "none")
      }
      
      #### Quartiles -----------------------------------------
      
      if(r$coche_quart){
        
        graph_hist_content <- graph_hist_content +
          geom_vline(xintercept = descript()$q1,
                     color = "orange") +
          geom_vline(xintercept = descript()$q3,
                     color = "green") +
          geom_vline(xintercept = descript()$mediane,
                     color = "blue",
                     linetype =  "dashed")   +
          annotate("text",
                   x = descript()$mediane,
                   y = ylim[2]- 0.015,
                   label = "Médiane") +
          annotate("text",
                   x = descript()$q1,
                   y =ylim[2]- 0.015,
                   label = "Q1") +
          annotate("text",
                   x = descript()$q3,
                   y =ylim[2]- 0.015,
                   label = "Q3")
        
      }
      
      return(graph_hist_content)
      
    })
    
    
    
    output$graphique_hist <- renderPlotly({
      w <- ggplotly(graph_hist()) %>%    
        layout(dragmode = "select") %>%   
        config(
          modeBarButtonsToRemove = c('lasso2d',
                                     'zoomIn2d',
                                     'zoomOut2d',
                                     'autoScale2d')
        )
      
      if(!r$coche_quart){
        for(longueurdata in seq(1, length(w$x$data), 1)){
          w$x$data[[longueurdata]]$text <- paste(r$solde2, "=", w$x$data[[longueurdata]]$x, r$unit_e,
                                                 "<br>",  round(w$x$data[[longueurdata]]$y * 100, digits = 2 ) , "% des valeurs" )
          
        }}
      
      
      w    
      
    })             
    
    
    
    # #Définition du texte -----------
    
    output$texte <- renderUI({
      
      req(result())
      result <- result()
      
      descript <- descript()
      
      
      moy <- paste0(c("Moyenne du solde (") ,r$select_solde, c(") ="), round(descript$moy)," ", r$unit_e)
      med <- paste0(c("Médiane (coupe l'échantillon en deux parties contenant le même nombre de valeurs) = " ), round(descript$mediane), " ", r$unit_e)
      q <- paste0(c("50 % des valeurs sont comprises entre "), round(descript$q1), c(" et "), round(descript$q3)," ", r$unit_e)
      
      HTML(paste(moy, med, q, sep = '<br/>'))
      
    })
    
    
    
    # #Définition du graphique -----------
    
    # tabl_descript <- reactive({
    #   req(r$coche_confort)
    #   test_tabl(nom_solde = r$select_solde ,
    #             result = result(),
    #             seuil_mini = r$s_mini,
    #             seuil_att = r$s_att,
    #             unite_euros = r$unit_e,
    #             unite_prod = r$unit_prod,
    #             unite_prix = r$unit_prix) %>% 
    #     as_flextable()
    # })
    
    
    # output$sortie_tabl <- renderTable(
    #   rownames = TRUE, {
    #     tabl_descript()
    #   })    
    
    
    graph_var <- reactive({
      
      req(result())
      
      gener_graph(
        # production,
        # prix, 
        # charges,
      nom_solde = r$select_solde ,
      result = result(),
      seuil_mini = r$s_mini,
      seuil_att = r$s_att,
      unite_euros = r$unit_e,
      unite_prod = r$unit_prod,
      unite_prix = r$unit_prix
    )
      })
    
    output$graphique_variable <- renderPlotly({
      graph_var() 
    })
    
    
    
    # output$sortie_tabl <- renderUI({
    #     tabl_descript() %>% 
    #     autofit() %>% 
    #     htmltools_value()
    #   }) 
    
    
    
    observe({
      toggle(id = "detail", condition = r$coche_confort) })
    
    
    observe({
      toggle(id = "graphique_variable", condition = input$detail &  r$coche_confort)
    })
 
    # Liens avec les modules --------------------------------
    
    observeEvent( r$button_graph , {
      
      if(r$choix_graph == "histo"){
      
      r$graph_save <- graph_hist()
      
      # r$tabl_save <-  tabl_descript() 
      
      r$graph_var_save <-  graph_var()
      }
      
    })    
    
  })
}
    
## To be copied in the UI
# mod_hist_repartition_ui("hist_repartition_1")
    
## To be copied in the server
# mod_hist_repartition_server("hist_repartition_1")
