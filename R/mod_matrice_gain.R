#' matrice_gain UI Function
#'
#' @description Gère tout ce qui est en lien avec la matrice de gain
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr case_when rename
#' @importFrom rlang :=
#' @importFrom ggplot2 geom_tile aes theme_minimal scale_fill_gradientn scale_fill_distiller
#' @importFrom shinyWidgets prettyCheckbox
#' 

mod_matrice_gain_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow( id = ns("graphique_mat"),
              
              ### Matrice---------------
              column(8,
                     plotlyOutput(ns("graph_mat")),
                     tags$button(
                       id = "web_button",
                       class = "btn action-button",
                       tags$img(src = "www/Image7.png",
                                height = "100px"),
                       onclick ="window.open('https://view.genial.ly/650b073107be920019bea9a5', '_blank')"
                     )
                     # ,
                     # verbatimTextOutput(ns("test"))
              ),
              column(4,
                     style = "background: #f2f2f2;",
                     selectInput(inputId = ns("idSelect_mat"), label = "Selectionnez la variable fixe ", selected = 3,
                                 choices = c("Production" = 1, "Prix" = 2, "Charges" = 3)),
                     
                     numericInput(inputId = ns("charges_mat"),
                                  label = paste0("Pr\u00e9cisez le niveau de charges souhait\u00e9"),
                                  value = uiOutput(ns("charges_moy_UI"))),
                     
                     numericInput(inputId = ns("prod_mat"),
                                  label = paste0("Pr\u00e9cisez le niveau de production souhait\u00e9"),
                                  value = uiOutput(ns("prod_moy_UI"))),
                     
                     numericInput(inputId = ns("prix_mat"),
                                  label = paste0("Pr\u00e9cisez le niveau de prix souhait\u00e9"),
                                  value = uiOutput(ns("prix_moy_UI"))),
                     div(style = "font-style: italic","(Par d\u00e9faut, moyenne)")
                     
              ))
  )
}
    
#' matrice_gain Server Functions
#'
#' @noRd 
mod_matrice_gain_server <- function(id, 
                                    r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
 
    col1_n <- reactive({

      req(r$dist_pr_graph_production)
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
      req(r$dist_pr_graph_production)

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
      req(r$dist_pr_graph_production)

      r$solde
    })



######## Pour l'appli en ligne----------------------------------------

    tbl_matrice <- reactive({

      req(r$dist_pr_graph_production)


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


## Def de la matrice ggplot-------------
    graph_mat <- reactive({

      col1_n <- col1_n()
      col2_n <- col2_n()
      marge_n <- marge_n()

      graph_mat_content  <- ggplot(tbl_matrice())  +
        geom_tile(aes(x = .data[[col1_n]], y = .data[[col2_n]], fill = .data[[marge_n]])) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 15L,
                                    face = "bold",
                                    hjust = 0.5)
        )


      graph_mat_content <-   graph_mat_content  +
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

 
      
      req(tbl_matrice())
      
      
      if(r$coche_confort){
        
        req(r$s_mini)
        req(r$s_att)
        
        marge_n <- marge_n()
        
        seuil_mini <- scales::rescale(r$s_mini, c(0,1), from = range(tbl_matrice()[marge_n]))
        seuil_att <- scales::rescale(r$s_att, c(0,1), from = range(tbl_matrice()[marge_n]))
        
        if( seuil_att >= 1) {
          
          if (seuil_mini >= 1){
            
            graph_mat_content  <- graph_mat_content   +
              scale_fill_gradientn(colours = c("#CD0000", "red"), values = c(0, 1) ) 
            
          } else if (0 < seuil_mini & seuil_mini < 1) {
            
            graph_mat_content  <- graph_mat_content +
              scale_fill_gradientn(colours = c("red", "orange"), values = c(0, seuil_mini, 1) ) 
            
          } else {
            graph_mat_content  <- graph_mat_content  +
              scale_fill_gradientn(colours = c("#FF7F00", "orange"), values = c(0,  1) ) 
            
          }
          
        } else if(seuil_mini < 0 ) {
          
          if(seuil_att > 0) {
            graph_mat_content  <-  graph_mat_content   +
              scale_fill_gradientn(colours = c( "orange", "green"), values = c(0,seuil_att,  1) ) 
          } else {
            
            graph_mat_content  <-  graph_mat_content   +
              scale_fill_gradientn(colours = c("chartreuse", "green"), values = c(0,  1) ) 
          }
          
        } else {
          
          graph_mat_content  <- graph_mat_content  +
            scale_fill_gradientn(colours = c("red", "orange", "green"), values = c(0, seuil_mini, seuil_att, 1) ) 
        }
        
        
      } else {
        
        graph_mat_content  <- graph_mat_content   +
          scale_fill_distiller(palette = "RdYlGn",direction = 1) 
        
      }

      
    })
      
      
      
     
    output$graph_mat <- renderPlotly({
      ggplotly(graph_mat() )
    })
    
    # Pour le word----------------------------------------------------
    # Ici pour le fonctionnement besoin de mettre une colonne en facteur, par rapport a l'appli en ligne
    
    
    
    
    tbl_matrice_word <- reactive({
      
      req(r$dist_pr_graph_production)
      
      
      col1_n <- col1_n()
      col2_n <- col2_n()
      marge_n <- marge_n()
      
      
      
      if(input$idSelect_mat == 3){   # ici charges sont fixes et on fait varier prod et prix
        req(input$charges_mat)
        
        tbl_m <-  expand.grid(col1 = round(seq( r$saisie_mini_production, r$saisie_maxi_production, length.out = 15)), # Production
                              col2 = round(seq( r$saisie_mini_prix, r$saisie_maxi_prix, length.out = 15))    ) %>% #Prix
          mutate(
            marge = col1 * col2 - input$charges_mat
          ) %>%
          unique()
        
      } else if(input$idSelect_mat == 2){ # ici prix fixe et on fait varier prod et charges
        req(input$prix_mat)
        
        tbl_m <- expand.grid(col1 = round(seq( r$saisie_mini_production, r$saisie_maxi_production, length.out = 15)),     #production
                             col2 = round(seq( r$saisie_mini_charges, r$saisie_maxi_charges, length.out = 15))) %>%    #charges
          mutate(
            marge = col1 * input$prix_mat - col2
          ) %>%
          unique()
        
      } else if(input$idSelect_mat == 1){ # ici prod fixe et on fait varier prix et charges
        req(input$prod_mat)
        
        tbl_m <- expand.grid(col1 = round(seq( r$saisie_mini_prix, r$saisie_maxi_prix, length.out = 15)),      #Prix
                             col2 = round(seq( r$saisie_mini_charges, r$saisie_maxi_charges, length.out = 15))) %>%  #Charges
          mutate(
            marge = input$prod_mat * col1 - col2
          ) %>%
          unique()
      }  
      
      tbl_m %>% 
        mutate(
          col1 = as.factor(col1),
          col2 = as.factor(col2)) %>% 
        rename(
          !!col1_n := col1,
          !!col2_n := col2,
          !!marge_n := marge
        )
      
    })
    
    
    #### Définition de la matrice du rapport---------------------------------------------
    graph_mat_word <- reactive({
      
      col1_n <- col1_n()
      col2_n <- col2_n()
      marge_n <- marge_n()
      
      graph_mat_content_word  <- ggplot(tbl_matrice_word())  +
        geom_tile(aes(x = .data[[col1_n]], y = .data[[col2_n]], fill = .data[[marge_n]])) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 13L,
                                    face = "bold",
                                    hjust = 0.5)
        )
      
      
      graph_mat_content_word <-   graph_mat_content_word  +
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
      
      
      
      req(tbl_matrice_word())
      
      
      if(r$coche_confort){
        
        req(r$s_mini)
        req(r$s_att)
        
        marge_n <- marge_n()
        
        seuil_mini <- scales::rescale(r$s_mini, c(0,1), from = range(tbl_matrice()[marge_n]))
        seuil_att <- scales::rescale(r$s_att, c(0,1), from = range(tbl_matrice()[marge_n]))
        
        if( seuil_att >= 1) {
          
          if (seuil_mini >= 1){
            
            graph_mat_content_word  <- graph_mat_content_word   +
              scale_fill_gradientn(colours = c("#CD0000", "red"), values = c(0, 1) )
            
          } else if (0 < seuil_mini & seuil_mini < 1) {
            
            graph_mat_content_word  <- graph_mat_content_word +
              scale_fill_gradientn(colours = c("red", "orange"), values = c(0, seuil_mini, 1) )
          } else {
            graph_mat_content_word  <- graph_mat_content_word  +
              scale_fill_gradientn(colours = c("#FF7F00", "orange"), values = c(0,  1) )
            
          }
          
        } else if(seuil_mini < 0 ) {
          
          if(seuil_att > 0) {
            graph_mat_content_word  <-  graph_mat_content_word   +
              scale_fill_gradientn(colours = c( "orange", "green"), values = c(0,seuil_att,  1) )
          } else {
            
            graph_mat_content_word  <-  graph_mat_content_word   +
              scale_fill_gradientn(colours = c("chartreuse", "green"), values = c(0,  1) )
          }
          
        } else {
          
          graph_mat_content_word  <- graph_mat_content_word  +
            scale_fill_gradientn(colours = c("red", "orange", "green"), values = c(0, seuil_mini, seuil_att, 1) )
        }
        
        
      } else {
        
        graph_mat_content_word  <- graph_mat_content_word   +
          scale_fill_distiller(palette = "RdYlGn",direction = 1)
        
      }
      
      
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



    
    ### Titre des matrices----------------------------
    
    titre_mat_3 <- reactive({
      if(is.null(r$solde)){
        "Solde en fonction du prix et de la production" } else {
          paste0(r$solde, " en fonction \n du prix et de la production") } 
    })
    
    titre_mat_2 <- reactive({
      if(is.null(r$solde)){
        "Solde en fonction des charges et de la production" } else {
          paste0(r$solde, " en fonction \n des charges et de la production") } 
      
    })
    
    titre_mat_1 <- reactive({
      if(is.null(r$solde)){
        "Solde en fonction du prix et des charges" } else {
          paste0(r$solde, " en fonction \n du prix et des charges") } 
      
    })  
    
    
    
    observe({
      toggle(id= "charges_mat", condition = input$idSelect_mat == 3 )
      toggle(id = "prix_mat", condition = input$idSelect_mat == 2 )
      toggle(id = "prod_mat", condition = input$idSelect_mat == 1 )

    })

    
    observeEvent( r$button_graph , {
      
      if(r$choix_graph == "mat"){
      
      r$graph_save <- graph_mat_word() }
      
    }) 
    
    
    
    # output$test <- renderPrint({
    #   tbl_matrice()
    #   
    #  
    #   })
    # 
    # 
      
    
  })
}
    
## To be copied in the UI
# mod_matrice_gain_ui("matrice_gain_1")
    
## To be copied in the server
# mod_matrice_gain_server("matrice_gain_1")
