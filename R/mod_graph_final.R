#' graph_final UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList downloadHandler downloadButton
#' @importFrom plotly ggplotly plotlyOutput renderPlotly event_data plot_ly add_segments add_trace layout config style  add_annotations
#' @importFrom tidyr crossing
#' @importFrom tibble tibble rownames_to_column
#' @importFrom shinyjs toggle useShinyjs
#' @importFrom shinyWidgets actionBttn radioGroupButtons checkboxGroupButtons prettyCheckbox prettyToggle
#' @importFrom dplyr select mutate %>% rename case_when filter group_by ungroup
#' @importFrom prompter add_prompt use_prompt
#' @import ggplot2 
#' @importFrom officer read_docx body_add_gg body_add_par
#' @importFrom scales percent
#' @importFrom rlang :=
#' @importFrom ranger ranger
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
      icon = 
        # tooltip( tags$span(icon("question")),
        #               title = "Voir onglet 'Tutoriels' - en construction"),
        tags$span(icon("question"))
      %>%
        add_prompt(
          position = "right",
          message = "Voir onglet 'Tutoriels' - en construction",
          type = "info")
      ,
      
      
      
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
                 choiceNames = list("Histogramme",
                                    "Boite à moustache",
                                    "Matrice de gain"),
                 choiceValues = list(
                   "histo", "bam", "mat"),
                 justified = TRUE,
                 checkIcon = list(
                   yes = icon("ok",
                              lib = "glyphicon"))
               )),
        
        
        column(12,
               plotlyOutput(ns("graphique_hist")),
               plotlyOutput(ns("graphique_bam"))
        )),
      
      fluidRow( id = ns("graphique_mat"),
                
                ### Matrice---------------
                column(8,
                       plotlyOutput(ns("graph_mat"))
                ),
                column(4,
                       style = "background: #f2f2f2;",
                       selectInput(inputId = ns("idSelect_mat"), label = "Selectionnez la variable fixe ", selected = 3,
                                   choices = c("Production" = 1, "Prix" = 2, "Charges" = 3)),
                       
                       numericInput(inputId = ns("charges_mat"),
                                    label = paste0("Précisez le niveau de charges souhaité"),
                                    value = uiOutput(ns("charges_moy_UI"))),
                       
                       numericInput(inputId = ns("prod_mat"),
                                    label = paste0("Précisez le niveau de production souhaité"),
                                    value = uiOutput(ns("prod_moy_UI"))),
                       
                       numericInput(inputId = ns("prix_mat"),
                                    label = paste0("Précisez le niveau de prix souhaité"),
                                    value = uiOutput(ns("prix_moy_UI"))),
                       p(style = "font-style: italic","(Par défaut, moyenne)")
                       
                )),
      
      fluidRow(
        column(5,
               br(),
               br(),
               htmlOutput(ns("texte") 
               )),
        column(7,
               br(),
               tableOutput(ns("sortie_tabl"))
        )
      ),
      
      # Gestion de la sidebar------------------------------
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
      r$go_button <- input$goButton
    })
    
    
    
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
          subtitle = "Répartition de la marge ",
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
      if(input$coche_confort){
        
          graph_hist_content <- graph_hist_content +
            aes(fill = case_when(solde < input$s_mini  ~ "A",
                                 solde > input$s_att ~ "B",
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
      
      if(input$coche_quart){
        
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
      
      for(longueurdata in seq(1, length(w$x$data), 1)){
        w$x$data[[longueurdata]]$text <- paste(r$solde2, "=", w$x$data[[longueurdata]]$x, r$unit_e,
                                    "<br>",  round(w$x$data[[longueurdata]]$y * 100, digits = 2 ) , "% des valeurs" )
        
      }
      
      
      w    
      
    })           
    
    
    
    ## Violon / bam de base -----------------------------
    
    graph_bam <- reactive({
      req(result())
      
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
          x0 = 'Solde',
          hoverinfo = 'y'
        ) %>% 
        layout(
          yaxis = list(
            title = "",
            zeroline = F,
            hoverformat = ".2f"
          ) 
        ) 
      # %>% 
      #   style(hoverinfo = 'none')
      
      
      
    })
    
    
    output$graphique_bam <- renderPlotly({
      graph_bam()
    })   
    
    
    ## Matrice--------------------------------------------------------
    
    
    col1_n <- reactive({
      
      req(result())
      nom <- case_when(
        input$idSelect_mat == 3 ~ "Production",
        input$idSelect_mat == 2 ~ "Production",
        input$idSelect_mat == 1 ~ "Prix"
      )
      
      unit <- case_when(
        input$idSelect_mat == 3 ~ r$unit_prod,
        input$idSelect_mat == 2 ~ r$unit_prod,
        input$idSelect_mat == 1 ~ r$unit_prix
      )
      
      paste0(nom, " en (", unit,  ") ")
    })
    
    col2_n <- reactive({
      req(result())
      
      nom <- case_when(
        input$idSelect_mat == 3 ~ "Prix",
        input$idSelect_mat == 2 ~ "Charges",
        input$idSelect_mat == 1 ~ "Charges"
      )
      
      unit <- case_when(
        input$idSelect_mat == 3 ~ r$unit_prix,
        input$idSelect_mat == 2 ~ r$unit_e,
        input$idSelect_mat == 1 ~ r$unit_e
      )
      
      paste0(nom, " en (", unit,  ") ")
    })
    
    
    marge_n <- reactive({
      req(result())
      
      r$solde
    })
    
    
    
    
    
    tbl_matrice <- reactive({
      
      req(result())
      
      
      col1_n <- col1_n()
      col2_n <- col2_n()
      marge_n <- marge_n()
      
      
      
      if(input$idSelect_mat == 3){   # ici charges sont fixes et on fait varier prod et prix
        req(input$charges_mat)
        
        tbl_m <-  expand.grid(col1 = r$dist_pr_graph_production, col2 = r$dist_pr_graph_prix) %>%
          mutate(
            marge = col1 * col2 - input$charges_mat
          ) %>%
          unique()   
        
      } else if(input$idSelect_mat == 2){ # ici prix fixe et on fait varier prod et charges
        req(input$prix_mat)
        
        tbl_m <- expand.grid(col1 = r$dist_pr_graph_production, col2 = r$dist_pr_graph_charges) %>%
          mutate(
            marge = col1 * input$prix_mat - col2
          ) %>%
          unique()  
        
      } else if(input$idSelect_mat == 1){ # ici prod fixe et on fait varier prix et charges
        req(input$prod_mat)
        
        tbl_m <- expand.grid(col1 = r$dist_pr_graph_prix, col2 = r$dist_pr_graph_charges) %>%
          mutate(
            marge = input$prod_mat * col1 - col2
          ) %>%
          unique()  
      }
      
      
      tbl_m %>% 
        rename(
          !!col1_n := col1,
          !!col2_n := col2,
          !!marge_n := marge
        )
      
    })
    
    
    
    graph_mat <- reactive({
      
      col1_n <- col1_n()
      col2_n <- col2_n()
      marge_n <- marge_n()
      
      g <- ggplot(tbl_matrice())  +
        geom_tile(aes(x = .data[[col1_n]], y = .data[[col2_n]], fill = .data[[marge_n]]))
      
      
      g + 
        labs(
          title = case_when(
            input$idSelect_mat == 3 ~ titre_mat_3(),
            input$idSelect_mat == 2 ~ titre_mat_2(),
            input$idSelect_mat == 1 ~ titre_mat_1()
          ),
          x = case_when(
            input$idSelect_mat == 3 ~ "Production",
            input$idSelect_mat == 2 ~ "Production",
            input$idSelect_mat == 1 ~ "Prix"
          ),
          y = case_when(
            input$idSelect_mat == 3 ~ "Prix",
            input$idSelect_mat == 2 ~ "Charges",
            input$idSelect_mat == 1 ~ "Charges"
          ) ) 
      
    })
    
    ####  Moyennes definies --------------------------------------- 
    
    
    charges_moy_UI <- renderUI( round(mean(r$dist_pr_graph_charges))  ) 
    
    prod_moy_UI <- renderUI( round(mean(r$dist_pr_graph_production))  ) 
    
    prix_moy_UI <- renderUI( round(mean(r$dist_pr_graph_prix))  ) 
    
    observe({
      updateNumericInput(session, "charges_mat", value = round(mean(r$dist_pr_graph_charges)))
      updateNumericInput(session, "prod_mat", value = round(mean(r$dist_pr_graph_production)))
      updateNumericInput(session, "prix_mat", value = round(mean(r$dist_pr_graph_prix)))
    })
    
    
    ####  zone de confort ---------------------------------------  
    
    observe({

      req(tbl_matrice())

      graph_mat <- graph_mat()

      if(input$coche_confort){

        req(input$s_mini)
        req(input$s_att)
        
        marge_n <- marge_n()

        seuil_mini <- scales::rescale(input$s_mini, c(0,1), from = range(tbl_matrice()[marge_n]))
        seuil_att <- scales::rescale(input$s_att, c(0,1), from = range(tbl_matrice()[marge_n]))

        if( seuil_att >= 1) {

          if (seuil_mini >= 1){

            graph_mat <- graph_mat  +
              scale_fill_gradientn(colours = c("#CD0000", "red"), values = c(0, 1) )

          } else if (0 < seuil_mini & seuil_mini < 1) {

            graph_mat <- graph_mat +
              scale_fill_gradientn(colours = c("red", "orange"), values = c(0, seuil_mini, 1) )
          } else {
            graph_mat <- graph_mat +
              scale_fill_gradientn(colours = c("#FF7F00", "orange"), values = c(0,  1) )

          }

        } else if(seuil_mini < 0 ) {

          if(seuil_att > 0) {
            graph_mat <-  graph_mat  +
              scale_fill_gradientn(colours = c( "orange", "green"), values = c(0,seuil_att,  1) )
          } else {

            graph_mat <-  graph_mat  +
              scale_fill_gradientn(colours = c("chartreuse", "green"), values = c(0,  1) )
          }

        } else {

          graph_mat <- graph_mat +
            scale_fill_gradientn(colours = c("red", "orange", "green"), values = c(0, seuil_mini, seuil_att, 1) )
        }


      } else {

        graph_mat <- graph_mat  +
          scale_fill_distiller(palette = "RdYlGn",direction = 1)

      }
      
      
      output$graph_mat <- renderPlotly({  
        ggplotly(graph_mat)
      })
      
    })
    
    
    ##Ranger ------------------------------------------
    # graph_rg <- reactive({
    #   
    #   req(calc_rg())
    # 
    #   model <- calc_rg()
    #   
    #   as.data.frame(model$variable.importance) %>% 
    #     rownames_to_column("nom") %>% 
    #     rename("value" = `model$variable.importance`)%>% 
    #     mutate(
    #       resultat = value / sum(value) # pourcentage
    #     ) %>% 
    #     ggplot() +
    #     geom_col( aes(y = "", x = resultat, fill = nom),
    #               position = "fill",
    #               width = 0.2)+
    #     scale_fill_manual(
    #       values = c(charges = "#D2691E",
    #                  prix = "#00868B",
    #                  production = "#00688B")
    #     ) +
    #     theme_void()  
    #   
    #   
    # })
    
    
    # output$graphique_rg <- renderPlot({
    #   
    #   graph_rg()
    # })
    
    
    
    # #Définition du texte -----------
    
    output$texte <- renderUI({
      
      req(result())
      result <- result()
      
      descript <- descript()
      
      
      moy <- paste0(c("Moyenne du solde (") ,r$select_solde, c(") ="), round(descript$moy), r$unit_e)
      med <- paste0(c("Médiane (coupe l'échantillon en deux parties contenant le même nombre de valeurs) = " ), round(descript$mediane), r$unit_e)
      q <- paste0(c("50 % des valeurs sont comprises entre "), round(descript$q1), c(" et "), round(descript$q3), r$unit_e)
      
      HTML(paste(moy, med, q, sep = '<br/>'))

    })
    
    

    
    
    output$sortie_tabl <- renderTable (
      rownames = TRUE,{
        req(input$coche_confort)
        test_tabl(nom_solde = r$select_solde ,
                  result = result(),
                  seuil_mini = input$s_mini,
                  seuil_att = input$s_att,
                  unite_euros = r$unit_e,
                  unite_prod = r$unit_prod,
                  unite_prix = r$unit_prix)
    
      })
    
    
    
    
    
    
    
    ### Toggle Gestion graphique ----------------------------------------------
    
    
    
    
    
    observe({
      toggle(id = "texte_pourcent", condition = input$coche_confort)
      toggle(id = "graphique_hist", condition = input$choix_graph == "histo")
      toggle(id = "graphique_bam", condition = input$choix_graph == "bam")
      toggle(id= "graphique_rg", condition = input$choix_graph == "rg")
      toggle(id= "graphique_mat", condition = input$choix_graph == "mat")
      toggle(id= "charges_mat", condition = input$idSelect_mat == 3 )
      toggle(id = "prix_mat", condition = input$idSelect_mat == 2 )
      toggle(id = "prod_mat", condition = input$idSelect_mat == 1 )
      toggle(id = "texte", condition = input$choix_graph %in% c("histo", "bam"))
      
    })
    
    observe({
      req(result())
      updateBoxSidebar("sidebar")
    })
    
    ## Gestion du titre--------------------------------------------------
    
    
    
    gest_text <- reactive({
      
      if(is.null(r$solde)){
        "Conséquences sur le solde choisi (marge, EBE, revenu...)" } else {
          paste("Conséquences sur le solde ",  r$solde, sep = "" )
        } 
      
    })
    
    output$titre <- renderUI( gest_text()   )  
    
    
    
    ### Matrice
    
    titre_mat_3 <- reactive({
      if(is.null(r$solde)){
        "Solde en fonction du prix et de la production" } else {
          paste0(r$solde, " en fonction du prix et de la production") } 
    })
    
    titre_mat_2 <- reactive({
      if(is.null(r$solde)){
        "Solde en fonction des charges et de la production" } else {
          paste0(r$solde, " en fonction des charges et de la production") } 
      
    })
    
    titre_mat_1 <- reactive({
      if(is.null(r$solde)){
        "Solde en fonction du prix et des charges" } else {
          paste0(r$solde, " en fonction du prix et des charges") } 
      
    })
    
  #  # Liens avec les modules --------------------------------
  #   
  #   observeEvent( r$button_graph , {
  #     
  # r$graph_save <- graph_hist()
  #     
  #     })
    
  })
}

## To be copied in the UI
# mod_graph_final_ui("graph_final_ui_1")

## To be copied in the server
# mod_graph_final_server("graph_final_ui_1")