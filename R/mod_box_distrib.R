#' box_distrib UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom  bs4Dash box boxSidebar 
#' @importFrom dplyr %>% 
#' @importFrom shinyWidgets materialSwitch actionBttn radioGroupButtons
#' @importFrom prompter add_prompt use_prompt
#' @importFrom shinyalert  shinyalert
#' @importFrom shinyjs toggle  useShinyjs 
#' @importFrom rhandsontable rHandsontableOutput  rhandsontable renderRHandsontable hot_to_r hot_cols




mod_box_distrib_ui <- function(id,
                               type,
                               input_mini,
                               input_maxi){
  ns <- NS(id)
  tagList(
    
    useShinyjs(),
    use_prompt(),
    
    
    tags$head(
      tags$style(
        HTML("[class*=hint--][aria-label]:after {
   white-space: pre;
}")
      )
    ),
    

    box(
      title = uiOutput(ns("titre")),
      # icon = tags$span(icon("question")) %>% 
      #   add_prompt(
      #     position = "right",
      #     message = def_help_text(type),  # Fonction pour modifier le message d'aide en fonction du type de variable
      #     type = "info"
      #   ),
      width = 4,
      
      
      wellPanel(
        
        
        if(type %in% "charges")  {
          h5("")},
        
        numericInput(
          inputId = ns("v_mini"),
          label = "minimale",
          value = input_mini
        ),
        
        numericInput(
          inputId = ns("v_maxi"),
          label = "maximale",
          value = input_maxi
        )
      ),
      
      
      materialSwitch(
        inputId = ns("loi"),
        label = "Je souhaite tracer une distribution (par défaut, la distribution sera uniforme)",
        value = FALSE,
        status = "primary"
      ),
      
      plotOutput(ns("roulette"),
                 click = ns("location"))  %>% 
        add_prompt(
          message = "Pour vous aider : un jeton peut représenter une unité de temps. \n\r (année, semaine, mois..). Amusez-vous et testez !!
          \n Plus il y a de jetons sur une valeur, plus celle-ci sera fréquente.
          \n Vous n'êtes pas à un jeton près !", 
          type = "info",
          position = "top"
        ),
      p(id = ns("t_distrib"),
      htmlOutput(ns("mean_distrib"))),
      
      #plotOutput(ns("histogram")),  
      
      
      br(),
      actionButton(ns("button_distrib"), icon("eye")),

      if(type %in% "charges")  {
       h5("")},
      
     # verbatimTextOutput(ns("test")),


      # Sidebar de la box

      sidebar = if(type %in% "charges")  { boxSidebar(
        id = ns("mycardsidebar"),
        width = 80,
        background = "#fafafa",

        rHandsontableOutput(outputId = ns("tabelle")) # Tableau qui permet de calculer les sommes
      )   } else {
       NULL
      }
    )
    
  )
}

#' box_distrib Server Functions
#'
#' @noRd 
mod_box_distrib_server <- function(id,
                                   graph_title,
                                   type,
                                   r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

 
    # Sortie des fonctions de distribution    ----------------------------------
    fs <- 12
    nBins <- 10
    gridHeight <- 10
    
    
    ### Unif-------------------------------------------        
    
    distrib_unif <- reactive ({
      
      v_mini <- input$v_mini
      v_maxi <- input$v_maxi
      
      if (v_mini > v_maxi ){
      distribution <- 0
      
      distribution

      } else {
        # bs4Dash::closeAlert(id = "myalert")
        
        distribution <- repet_unif(mini = v_mini,
                                   maxi = v_maxi)
        distribution
        
      
      }
    })
    
    
    
    
    # ### Distrib--------------------------------------------------------
    
    v_mini <- reactive({ input$v_mini })
    
    v_maxi <- reactive({ input$v_maxi })
    
    
    v_bin.width <- reactive({ bin.width(v_mini(), 
                                        v_maxi()) }) 
    
    
    v_bin.left <- reactive({ bin.left(mini = v_mini(),
                                      maxi = v_maxi(),
                                      width = v_bin.width()) })
    
    
    v_bin.right <- reactive({ bin.right(mini = v_mini(),
                                        maxi = v_maxi(),
                                        width = v_bin.width()) })
    
    rl <- reactiveValues(x=-1, y=-1,
                         chips = rep(0, 10),
                         allBinsPr = NULL,
                         nonempty = NULL )
    
    graph_distrib <-  reactive({
      plot_elicit(name =  graph_title,
                  chips = rl$chips,
                  mini = v_mini(),
                  maxi = v_maxi(),
                  left = v_bin.left(),
                  right = v_bin.right())
    })
    
    
    output$roulette <- renderPlot({
      graph_distrib() 
    })
    
    observeEvent(input$loi, {toggle("roulette")})
    observeEvent(input$loi, {toggle(id = "t_distrib")})



    
    observeEvent(input$location, {
      rl$x <-input$location$x
      rl$y <-input$location$y
      plotHeight <- max(gridHeight, max(rl$chips) + 1)
      
      if(rl$x > v_mini() & rl$x < v_maxi() & rl$y < plotHeight){
        index <- which(rl$x >= v_bin.left() & rl$x < v_bin.right())
        rl$chips[index]<-ceiling(max(rl$y, 0))
        rl$allBinsPr <- cumsum(rl$chips)/sum(rl$chips)
        rl$nonEmpty <- rl$allBinsPr > 0 & rl$allBinsPr < 1
      }
    })
    
    p <- reactive({
      rp <- rl$allBinsPr[rl$nonEmpty]
      myp <- rp
      myp
    })
    
    v <- reactive({
      rv <- v_bin.right()[rl$nonEmpty]
      myv <- rv
      myv
    })
    
    v_myfit <- reactive({
      
      myfit <- tryCatch({
        this_fit <- fitdisti(mini = v_mini(),
                             maxi = v_maxi(),
                             v = v(),
                             p = p())
        if(is.null(this_fit)){
          this_fit <- "Manque de points"
        }
        return(this_fit)
      },
      error = function(e) { return("Manque de points")}
      )
      
      myfit
    })
    
    
    distrib_man <-  reactive({
 
      req(v_myfit())
      
      myfit <- v_myfit()
      if (is.list(myfit)){
        calc_distrib(myfit = myfit,
                     mini = v_mini(),
                     maxi = v_maxi()) 
      } else {
        NULL
      }
        

    
    })
    
    
    
    ## Choix de la distribution finale en fonction du choix de l'utilisateur---------------
    
    distrib_finale <- reactive({
      if (input$loi == TRUE && !is.null(distrib_man())) {
        distrib <- distrib_man() %>%
          round(., digits = 1) %>%
          sort()
      } else   if (input$loi == TRUE && is.null(distrib_man())) {
        distrib <- "Oser a besoin de jetons supplémentaires pour déterminer une distribution"
      } else{
        distrib <- distrib_unif()%>%
          round(., digits = 1) %>%
          sort()
      }
      distrib 
    }
    )
   




    output$mean_distrib <- renderUI({
      if (input$loi){
        myfit <- v_myfit()
        text <- paste0(em("Oups ! J'ai encore besoin de jetons !"))
        if (is.list(myfit)){
          my_mean <- mean(distrib_finale())  %>%
            round(., digits = 1)
          text <- paste0(em("Moyenne de la distribution :"), br(), my_mean)
        }
        HTML(text)
      }
     
      })
       



    
    # Aperçu de la distribution Shinyalert quand clique sur le bouton--------------
    
    observeEvent(input$button_distrib, {
      #   # Show a modal when the button is pressed
      
     req(distrib_finale())
      
       distrib <- distrib_finale()
      
      output$distrib<- renderPrint(distrib)
      
      shinyalert(
        html = TRUE,
        title = "Distribution :",
        text = tagList(
          verbatimTextOutput(ns("distrib"))
        ),
        type = "",
        closeOnClickOutside = TRUE)
      
    })
    
    
    
    
    
    #   Gestion du rhandsontable pour intégrer plus de praticité pour l'utilisateur  ---------------------
    
    ## Définition du tableau
    Nom <- c("", "","","","", "", "", "", "", "")
    Mini <- c("", "","","","", "", "", "", "", "") %>%
      as.numeric()
    Maxi <- c("", "","","","", "", "", "", "", "") %>%
      as.numeric()
    
    rvd <- reactiveValues()
    rvd$sum_mini <- 0
    rvd$sum_maxi <- 0
    
    df <- data.frame(Nom,
                     Mini,
                     Maxi)
    
    names(df) <- c(paste0("Types de ", type), "Mini", "Maxi")
    
    df <- df %>%
      rhandsontable() %>%
      hot_cols(renderer =  "function(instance, td) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            td.style.color = 'black';
          }")
    
    
    output$tabelle <- renderRHandsontable({
      df
    })
    
    
    observe({
      df$values <- hot_to_r(input$tabelle)
      
      rvd$sum_mini <- df$values[[2]] %>% 
        sum(., na.rm = TRUE)
      
      rvd$sum_maxi <- df$values[[3]] %>% 
        sum(., na.rm = TRUE)
    })
    
    
    
    # Mise à jour des inputs avec les données du tableau (uniquement si rempli)  
    observe({
      req(rvd$sum_mini!=0)
      
      updateNumericInput(session,
                         "v_mini",
                         value = rvd$sum_mini)
    }) 
    
    
    observe({
      req(rvd$sum_maxi!=0)
      
      updateNumericInput(session,
                         "v_maxi",
                         value = rvd$sum_maxi)
    })     
    
    ##  Envoi du graph pour sortie word -----------
    
    # observeEvent(r$go_button, {
    #   essai <- graph_distrib()
    #   graph_data$plot_prod <- essai
    # })
    # 
    # 
    # # Affichez le graphique
    # output$plot_output <- renderPlot({
    #   # Affichez le graphique stocké dans l'objet reactiveValues
    #   print(graph_data$plot_prod)
    # })
    
    
    # output$test <- renderPrint({  input$location })
    # 
    # 
    # saveRoulettePlot <- function() {
    #   png_file <- tempfile(fileext = ".png")  # Crée un fichier PNG temporaire
    #   png(png_file, width = 800, height = 600, units = "px", res = 100)
    #   plotRoulette()
    #   dev.off()
    #   graph_png_reactive(png_file)  # Stocke le nom du fichier PNG temporaire dans la variable réactive
    # }
    # 
    # 
    # r[[paste("graph_png", type, sep = "_")]]
    # 
    
    ## Liens avec les modules--------------------- 
    
    observeEvent( r$go_button , {
      
      if(input$v_mini > input$v_maxi ){
        shinyalert(
          title = "Mini > Maxi !",
          text = "Attention, erreur de saisie ",
          size = "xs", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = TRUE,
          type = "error",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else {   
      
      r[[paste("dist_pr_graph", type, sep = "_")]] <- distrib_finale() 
      
      r[[paste("saisie_mini", type, sep = "_")]]  <- input$v_mini 
      r[[paste("saisie_maxi", type, sep = "_")]]  <- input$v_maxi
      

      if(input$loi){
        r[[paste("saisie_distrib", type, sep = "_")]] <- "Distribution manuelle"
        
      } else {
        r[[paste("saisie_distrib", type, sep = "_")]] <- "Distribution uniforme"
      }
      
      }
    })
    

    
    # observeEvent(r$button_unit,{
    #   texte <- r$unit_prix
    #   
    # })
    
    # test_text <- reactive({
    #   if(is.null(r$unit_prix)){
    #     box_title } else {
    #       paste(title,r$unit_prix, sep = ,)
    #     }
    # 
    # })
    
    
 ## Gestion des titres -------------------------------------------------------------------------   
    gest_text <- reactive({
      
      if(type == "prix"){
        if(is.null(r$unit_prix)){
          "Prix" } else {
            HTML(paste("Prix (",  em(r$unit_prix), ")", sep ="" ))
          } 
        
      }     else if(type == "charges"){
        if(is.null(r$unit_e)){
          "Charges" } else {
            HTML(paste("Charges (",  
                       em(r$unit_e), 
                       em(" à l'échelle "),  
                       em(r$echelle),
                       em(")"), sep ="" ))
          } 
      }      else if(type == "production"){
        if(is.null(r$unit_prod)){
          "Production et quantité" } else {
            HTML(paste("Production (",  em(r$unit_prod), ")", sep ="" ))
          } 
      }
      
    })
    
    output$titre <- renderUI( gest_text()   )  
    
    
    # output$test <- renderPrint(
    #   v_myfit()
    #   
    # )

  })
}

## To be copied in the UI
# mod_box_distrib_ui("box_distrib_ui_1")

## To be copied in the server
# mod_box_distrib_server("box_distrib_ui_1")
