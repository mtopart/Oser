#' telechargement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom officer read_docx body_add_gg body_add_par body_add_break plot_instr body_add_plot fpar body_add_fpar fp_text external_img ftext
#' @importFrom flextable body_add_flextable 
#' @importFrom shinyalert shinyalert 
#' 
mod_telechargement_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
   # useShinyalert(),
    
    box(
      title = "Enregistrement des donn\u00e9es",
      width = 12,
      
      
      
    # strong(style = "color:red ;font-size: 20px;
    #                              font-style: italic","En construction"),
    fluidRow(
      
      
      column(
      4,
    textInput(ns("nom_prenom"), 
              "Nom - Pr\u00e9nom / D\u00e9signation")),
    
    column(
      4,
    textInput(ns("titre_analyse"), 
              "Contexte d'analyse")),
    
    column(
      4,
      textInput(ns("titre_scenar"), 
                "Titre du sc\u00e9nario")),
    
    textAreaInput(
      inputId = ns("com_hist"),
      label = NULL,
      width = '100%',
      height = '150px',
      placeholder = "Commentaires (histogramme)"
    ),
    
    textAreaInput(
      inputId = ns("com_mat"),
      label = NULL,
      width = '100%',
      height = '150px',
      placeholder = "Commentaires (matrice de gain)"
    ),
    
   column(
     12,

    actionButton(ns("select_graph"), 
                 "Enregistrer le graphique et les commentaires",
                 icon("save")
                 ),
    
    
    br(),
    br(),
    downloadButton(ns("dl_graph"), span("T\u00e9l\u00e9charger le compte-rendu")),
    br(),
    tags$button(
      id = "web_button",
      class = "btn action-button",
      tags$img(src = "www/Image7.png",
               height = "100px"),
      onclick ="window.open('https://view.genial.ly/650c8cc6504ecb00114f906a', '_blank')"
    )
     # ,
     # verbatimTextOutput(ns("test"))
    # ,
    # plotOutput(ns("test2"))
  ))
    )
  )
}
    
#' telechargement Server Functions
#'
#' @noRd 
mod_telechargement_server <- function(id, 
                                      r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    
    
    # titre_sce <- reactive(
    #   if
    # )

graph_word <- function(gg,
                       gg_tabl,
                       doc_word){
  
  
  #Definition des paragraphes------------
  par_unit <- fpar(
    ftext("Echelle d'analyse = ", fp_text(bold = TRUE)),  
    r$echelle)
  
  ## Paragraphe production
  
  par_prod1 <- fpar(
    ftext("Mini = ", prop = fp_text(bold = TRUE)),
    paste0(r$saisie_mini_production, " ", r$unit_prod))
  
  par_prod2 <- fpar(
    ftext("Maxi = ", prop = fp_text(bold = TRUE)),  
    paste0( r$saisie_maxi_production, " ", r$unit_prod))
  
  par_prod3 <- fpar(
    ftext("Choix de distribution = ", fp_text(bold = TRUE)),  
    r$saisie_distrib_production)
  
  
  ## Paragraphe prix
  
  par_prix1 <- fpar(
    ftext("Mini = ", prop = fp_text(bold = TRUE)),
    paste0(r$saisie_mini_prix, " ", r$unit_prix))
  
  par_prix2 <- fpar(
    ftext("Maxi = ", prop = fp_text(bold = TRUE)),  
    paste0( r$saisie_maxi_prix, " ", r$unit_prix))
  
  par_prix3 <- fpar(
    ftext("Choix de distribution = ", fp_text(bold = TRUE)),  
    r$saisie_distrib_prix)
  
  
  
  ## Paragraphe charges
  
  par_charges1 <- fpar(
    ftext("Mini = ", prop = fp_text(bold = TRUE)),
    paste0(r$saisie_mini_charges, " ", r$unit_e ))
  
  par_charges2 <- fpar(
    ftext("Maxi = ", prop = fp_text(bold = TRUE)),  
    paste0( r$saisie_maxi_charges, " ", r$unit_e ))
  
  par_charges3 <- fpar(
    ftext("Choix de distribution = ", fp_text(bold = TRUE)),  
    r$saisie_distrib_charges)
  
  

  
  # Titre  -------------------------------
  doc_word <- doc_word %>%
    body_add_par(value = input$titre_scenar, style = "heading 1") 
  
  
  #  Recapitulatif de saisie-------------------------
  
  doc_word <- doc_word %>%
    body_add_par(value = "R\u00e9capitulatif des \u00e9l\u00e9ments de saisie", style = "heading 2") %>% 
    body_add_fpar(value = par_unit, style = "Normal") %>% 
    body_add_par(value = "Production", style = "heading 3") %>% 
    body_add_fpar(par_prod1) %>% 
    body_add_fpar(par_prod2) %>% 
    body_add_fpar(par_prod3) 
  
  if( r$saisie_distrib_production == "Distribution manuelle"){
    doc_word <- doc_word %>%
      body_add_gg( r$graph_distrib_production,
                   width = 4,
                   height = 3)
  }

    doc_word <- doc_word %>%
    body_add_par(value = "Prix", style = "heading 3") %>% 
    body_add_fpar(par_prix1) %>% 
    body_add_fpar(par_prix2) %>% 
    body_add_fpar(par_prix3) 
    
    
    if( r$saisie_distrib_prix == "Distribution manuelle"){
      doc_word <- doc_word %>%
        body_add_gg( r$graph_distrib_prix,
                     width = 4,
                     height = 3)
    }
    
    doc_word <- doc_word %>%
    body_add_par(value = "Charges", style = "heading 3")   %>% 
    body_add_fpar(par_charges1) %>% 
    body_add_fpar(par_charges2) %>% 
    body_add_fpar(par_charges3) 
    
    
    if( r$saisie_distrib_charges == "Distribution manuelle"){
      doc_word <- doc_word %>%
        body_add_gg( r$graph_distrib_charges,
                     width = 4,
                     height = 3)
    }
  
  
  # Ree9sultats -------------------------------
  
  doc_word <- doc_word  %>%
    body_add_par(value = "R\u00e9sultats", style = "heading 2") 
  
  if(r$choix_graph == "histo"){ # Ajout tableau et commentaires pour histogramme
    
  doc_word <- doc_word %>%
    body_add_gg(value = gg,
                style = "Normal") %>%
      body_add_par(value = input$com_hist, style = "Normal") 
  
  if(r$coche_confort){   # Ajout du graphique qui concerne les variables de la zone de confort
    doc_word <- doc_word %>%
      body_add_break() %>% 
      body_add_par(value = r$text_var, style = "Normal") %>% 
      body_add_gg(gg_tabl)
  }
  }
  
  if(r$choix_graph == "mat"){    # Ajout tableau et commentaires pour matrice
    
    doc_word <- doc_word %>%
      body_add_gg(value = gg,
                  style = "Normal") %>%
      body_add_par(value = input$com_mat, style = "Normal")  
  }
    

  
}

    doc <- reactive({
     read_docx(path = "inst/app/templates/template.docx") %>%
        body_add_fpar(
          fpar(
          ftext(text = "Sorties ",  
                prop = fp_text(bold = TRUE, color = "#6E97C9", font.size = 12)),
          external_img(src = "inst/app/www/oser.jpg"),
          ftext(text = " - Compte-rendu",  
                prop = fp_text(bold = TRUE, color = "#6E97C9", font.size = 12))),   
                      style = "centered"
          ) %>% 
        body_add_par(value = input$titre_analyse, style = "centered") %>%
        body_add_par(value = input$nom_prenom, style = "centered")
    })



    observeEvent(input$select_graph,{
      req(r$dist_pr_graph_production)
      r$button_graph <- input$select_graph
    })

      observeEvent(r$button_graph,{
      graph_word(gg =  r$graph_save,
                 gg_tabl = r$graph_var_save,
                 doc_word = doc())
    })

    
    # output$test <- renderPrint({
    #   r$graph_save
    # })
    # 

      
      observeEvent(input$select_graph, {
        # Show a simple modal
        #shinyalert(title = "You did it!", type = "success")
        req(r$dist_pr_graph_production)
        
        shinyalert(
          title = "Enregistrement effectu\u00e9",
          text = "Graphique(s) et donn\u00e9es m\u00e9moris\u00e9s",
          size = "xs", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = FALSE
        )
        
        
      })
      
    
    output$dl_graph <- downloadHandler(
      filename = function() {
        paste0("Sorties Oser ",input$nom_prenom ,"_ ", Sys.Date(), ".docx")
      },
      content = function(file) {
        print(doc(), target = file)
      }
    )
    
    
    observe({
      toggle("com_hist", condition = r$choix_graph == "histo")
      toggle("com_mat", condition = r$choix_graph == "mat") })
    
    # output$test <- renderPrint({
    #   #r$saisie_dist_graph_production 
    #   r$saisie_distrib_charges
    #   })
    # 
    # output$test2 <- renderPlot({
    #   #r$graph_save
    #   r$saisie_dist_graph_production
    # 
    # })
    
    

    
  })
}
 
## To be copied in the UI
# mod_telechargement_ui("telechargement_1")
    
## To be copied in the server
# mod_telechargement_server("telechargement_1")
