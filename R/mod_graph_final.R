#' graph_final UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList downloadHandler downloadButton
#' @importFrom plotly ggplotly plotlyOutput renderPlotly plot_ly add_segments add_trace layout config style  add_annotations
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom shinyjs toggle useShinyjs
#' @importFrom shinyWidgets actionBttn radioGroupButtons checkboxGroupButtons prettyCheckbox prettyToggle
#' @importFrom dplyr select mutate %>% rename case_when filter group_by ungroup
#' @importFrom prompter add_prompt use_prompt
#' @importFrom ggplot2 ggplot scale_fill_manual annotate aes after_stat element_text scale_y_continuous geom_histogram labs geom_boxplot scale_fill_distiller geom_vline theme_light theme_bw coord_cartesian theme element_blank geom_tile scale_fill_gradientn
#' @importFrom officer read_docx body_add_gg body_add_par
#' @importFrom scales percent
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
                                    "Boite à moustache",
                                    "Matrice"),
                 choiceValues = list(
                   "histo", "bam", "mat"
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
      
      fluidRow( id = ns("graphique_mat"),
      
                ### Matrice---------------
      column(8,
             plotlyOutput(ns("graph_mat"))
             ),
      column(4,
             style = "background: #f2f2f2;",
       selectInput(inputId = ns("idSelect_mat"), label = "Selectionner la variable fixe ", selected = 3,
                         choices = c("Production" = 1, "Prix" = 2, "Charges" = 3)),
       
       numericInput(inputId = ns("charges_mat"),
                    label = paste0("Niveau de charges"),
                    value = uiOutput(ns("charges_moy_UI"))),
       
       numericInput(inputId = ns("prod_mat"),
                    label = paste0("Niveau de production"),
                    value = uiOutput(ns("prod_moy_UI"))),
       
       numericInput(inputId = ns("prix_mat"),
                    label = paste0("Niveau de prix"),
                    value = uiOutput(ns("prix_moy_UI")))
      
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

# prettyCheckbox(
#   inputId = ns("coche_quart"),
#   label = "Quartiles",
#   value = FALSE,
#   icon = icon("check"),
#   status = "success"
# ),

prettyToggle(
  inputId = ns("coche_quart"),
  label_on = "Quartiles affichés",
  status_on = "default",
  icon_on = icon("ok-circle", lib = "glyphicon"),
  label_off = "Sans quartiles",
  status_off = "default",
  icon_off = icon("remove-circle", lib = "glyphicon"),
  plain = TRUE,
  inline = FALSE),

htmlOutput(ns("texte")),

br(),
br(),
hr(),



actionBttn(
  inputId = ns("select_graph"),
  label = "Sélectionner le graphique", 
  style = "bordered",
  color = "success"
),
br(),
br(),
downloadButton(ns("dl_graph"), "Télécharger le récapitulatif")




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
        "Fréquence des valeurs" } else {
          paste("Fréquence des valeurs de", r$solde,  sep = " " )
        } 
    }) 
    
    
    ## Histogramme de base ----------------------------
    
    
    graph_hist <- reactive({
      req(result())
      result <- result()
      
      graph_hist <- result %>%
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
          y = graph_axe_titre_y(),
          fill = ""
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 15L,
                                    face = "bold"),
          plot.subtitle = element_text(size = 13L,
                                       face = "italic")
        )
      
        
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

        graph_hist <- graph_hist +
          aes(fill = case_when(solde < input$s_mini ~ "A", 
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
        graph_hist <- graph_hist +
          aes(fill = "#XXXXX") +
          scale_fill_manual(values = c("#XXXXX" = "#77b5fe"))  + 
          theme(
            legend.position = "none")
      }

   #### Quartiles -----------------------------------------

      if(input$coche_quart){

        graph_hist <- graph_hist +
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
      
      ### Sortie Output------------------------------
      
      output$graphique_hist <- renderPlotly({
        ggplotly(graph_hist) %>%    
              config(
                modeBarButtonsToRemove = c('lasso2d',
                                           'zoomIn2d',
                                           'zoomOut2d',
                                           'autoScale2d')
              )
      })
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
          hoverinfo = "y"
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
    

    tbl_matrice <- reactive({
      
      req(result())
      
      if(input$idSelect_mat == 3){   # ici charges sont fixes et on fait varier prod et prix
        
        expand.grid(col1 = r$dist_pr_graph_production, col2 = r$dist_pr_graph_prix) %>%
          mutate(
            marge = col1 * col2 - input$charges_mat
            # ,
            # col1 = as.factor(col1),
            # col2 = as.factor(col2)
          ) %>%
          group_by(col1, col2) %>%
          unique()  %>%
          ungroup()  
        
      } else if(input$idSelect_mat == 2){ # ici prix fixe et on fait varier prod et charges
        
        
        expand.grid(col1 = r$dist_pr_graph_production, col2 = r$dist_pr_graph_charges) %>%
          mutate(
            marge = col1 * input$prix_mat - col2
            # ,
            # col1 = as.factor(col1),
            # col2 = as.factor(col2)
          ) %>%
          group_by(col1, col2) %>%
          unique()  %>%
          ungroup() 
        
      } else if(input$idSelect_mat == 1){ # ici prod fixe et on fait varier prix et charges
        
        
        expand.grid(col1 = r$dist_pr_graph_prix, col2 = r$dist_pr_graph_charges) %>%
          mutate(
            marge = input$prod_mat * col1 - col2
            # ,
            # col1 = as.factor(col1),
            # col2 = as.factor(col2)
          ) %>%
          group_by(col1, col2) %>%
          unique()  %>%
          ungroup()     
        
      }
      
    })
    
 
    
    graph_mat <- reactive({
      
    g <- ggplot(tbl_matrice())  +
        geom_tile(aes(x = col1, y = col2, fill = marge))
      
      
       g + 
        labs(
          title = case_when(
            input$idSelect_mat == 3 ~ "Marge en fonction du prix et de la production",
            input$idSelect_mat == 2 ~ "Marge en fonction des charges et de la production",
            input$idSelect_mat == 1 ~ "Marge en fonction du prix et des charges"
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
         
         seuil_mini <- scales::rescale(input$s_mini, c(0,1), from = range(tbl_matrice()$marge))
         seuil_att <- scales::rescale(input$s_att, c(0,1), from = range(tbl_matrice()$marge))
         
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
        ggplotly(graph_mat,
                 tooltip = "marge")
      })
      
    })
    
    
    # output$graph_mat <- renderPlotly({
    #   
    #   seuil <- scales::rescale(1000, c(0,1), from = range(tbl_matrice()$marge))
    #   
    #   if( seuil >= 1) {
    #     
    #     g <-ggplot(tbl_matrice())  +
    #       geom_tile(aes(x = col1, y = col2, fill = marge)) +
    #       scale_fill_gradientn(colours = c("red", "orange"), values = c(0, 1) )  
    #     
    #   } else if(seuil < 0 ) {
    #     
    #     g <-ggplot(tbl_matrice())  +
    #       geom_tile(aes(x = col1, y = col2, fill = marge)) +
    #       scale_fill_gradientn(colours = c("red", "orange", "green"), values = c(0,0, 1) ) 
    #     
    #   } else { 
    #     
    #     g <- ggplot(tbl_matrice())  +
    #       geom_tile(aes(x = col1, y = col2, fill = marge)) +
    #       scale_fill_gradientn(colours = c("red", "orange", "green"), values = c(0, seuil, 1) )
    #   } 
    #   
    #   
    #   
    # g <-  g + 
    #     labs(
    #       title = case_when(
    #         input$idSelect_mat == 3 ~ "Marge en fonction du prix et de la production",
    #         input$idSelect_mat == 2 ~ "Marge en fonction des charges et de la production",
    #         input$idSelect_mat == 1 ~ "Marge en fonction du prix et des charges"
    #       ),
    #       x = case_when(
    #         input$idSelect_mat == 3 ~ "Production",
    #         input$idSelect_mat == 2 ~ "Production",
    #         input$idSelect_mat == 1 ~ "Prix"
    #       ),
    #       y = case_when(
    #         input$idSelect_mat == 3 ~ "Prix",
    #         input$idSelect_mat == 2 ~ "Charges",
    #         input$idSelect_mat == 1 ~ "Charges"
    #       )
    #   )
    # 
    #   
    #   ggplotly(g)
    #   
    # })
    
    
    
    
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
      
  
    
    
    
    
    
    

    
    ### Toggle Gestion graphique ----------------------------------------------
    




observe({
  toggle(id = "zone_conf", condition = input$coche_confort)
  toggle(id = "graphique_hist", condition = input$choix_graph == "histo")
  toggle(id = "graphique_bam", condition = input$choix_graph == "bam")
  toggle(id= "graphique_mat", condition = input$choix_graph == "mat")
  toggle(id= "charges_mat", condition = input$idSelect_mat == 3 )
  toggle(id = "prix_mat", condition = input$idSelect_mat == 2 )
  toggle(id = "prod_mat", condition = input$idSelect_mat == 1 )
  
})

      
  ## Gestion du titre--------------------------------------------------
      
      
      gest_text <- reactive({
        
          if(is.null(r$solde)){
            "Conséquences sur le solde choisi (marge, EBE, revenu...)" } else {
              paste("Conséquences sur ",  r$solde, sep = "" )
            } 
        
      })
      
      output$titre <- renderUI( gest_text()   )  
      
      
      
## Téléchargement----------------------------
      
      
  graph_word <- function(gg ){

    if(exists("doc")){
    } else {
      doc <- read_docx()
    }

  doc <- doc %>%
    body_add_par(value = "Table des graphiques", style = "heading 1") %>%
    body_add_gg(value = gg,
                style = "Normal")

  }


  observeEvent(input$select_graph,{
   doc <- reactive({
     graph_word(graph_hist())
   })

  })
      
  
  
  # doc <- reactive({
  #  read_docx() %>%
  #     body_add_par(value = "Retranscription des commentaires", style = "heading 1") %>%
  #     
  #     body_add_par(value = "Contexte général", style = "heading 2")
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
  #     
  # doc() %>%
  #      print(target = tempfile(fileext = ".docx")) %>%
  #   browseURL()
      
    
  })
}

## To be copied in the UI
# mod_graph_final_ui("graph_final_ui_1")

## To be copied in the server
# mod_graph_final_server("graph_final_ui_1")