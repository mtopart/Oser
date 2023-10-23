#' box_distrib UI Function
#'
#' @description Module selection et definition de la distribution
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom  bs4Dash box boxSidebar 
#' @importFrom dplyr %>% as_tibble 
#' @importFrom ggplot2 ggplot geom_histogram aes labs theme_light theme element_line element_text coord_cartesian scale_x_continuous
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
      
      div("Par d\\u00e9faut, l'\\u00e9chantillon choisi suit une distribution uniforme."),
      materialSwitch(
        inputId = ns("loi"),
        label = "Tracer une distribution",
        value = FALSE,
        status = "primary"
      ),
      
      plotOutput(ns("roulette"),
                 click = ns("location"))  %>% 
        add_prompt(
          message = "Pour vous aider : un jeton peut repr\\u00e9senter une unit\\u00e9 de temps. \n\r (ann\\u00e9e, semaine, mois..). Amusez-vous et testez !!
          \n Plus il y a de jetons sur une valeur, plus celle-ci sera fr\\u00e9quente.
          \n Vous n'\\u00eates pas \\u00e0 un jeton pr\\u00e8s !", 
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
        distrib <- "Oser a besoin de jetons suppl\\u00e9mentaires pour d\\u00e9terminer une distribution"
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
       
   output$text <- renderText( "Les valeurs :")

    # output$distrib<- renderPrint(matrix(unlist(distrib_finale()),
    #                                            ncol = 5) %>% 
    #                                as_tibble(.name_repair = "minimal"))
    
   ## Toutput tableau distrib-------------------
   
   tabl_distrib_af <- reactive({
     req(is.numeric(distrib_finale()[1]))
     
     matrix(unlist(distrib_finale()),
            ncol = 5) %>% 
     #  as_tibble(.name_repair = "minimal")
     as.data.frame()
     
   })
   
   
    output$distrib<- renderTable(
      tabl_distrib_af(),
      
                                 colnames = FALSE,
                                 spacing = "s")
                                        
    
    # Aperçu de la distribution Shinyalert quand clique sur le bouton--------------
    
    observeEvent(input$button_distrib, {
      #   # Show a modal when the button is pressed
      
      req(distrib_finale())
      
     #  
     #   distrib <- distrib_finale()
     #  
     #  output$distrib<- renderPrint(distrib)
      ##shinyalert-------------------------------
      shinyalert(
        html = TRUE,
        title = "Plus de d\\u00e9tails sur l'\\u00e9chantillon :",
        size = "m",
        text = tagList(
          # if(input$loi){
          # plotOutput(ns("graph_dist")
          #             ,
          #            width = "350px",
          #            height = "350px"
          #            )},
          # if(input$loi){
          #   br()},
          # textOutput(ns("text")),
          # verbatimTextOutput(ns("distrib"))
          
          
          fluidRow( 
            column(6,
                   textOutput(ns("text")),
                   #verbatimTextOutput(ns("distrib")),
                   br(),
                   tableOutput(ns("distrib"))
                   
                   # materialSwitch(
                   #   inputId = ns("modif_choix"),
                   #   label = "Modifier les valeurs",
                   #   value = FALSE,
                   #   status = "primary"
                   # )

                   ),
            column(6,
                   if(input$loi){
                     plotOutput(ns("graph_dist")) } ))

          
        ),
        type = "",
        closeOnClickOutside = TRUE)
      
    })
    
    
    
    output$donnees_modif <- renderPrint({ input$modif_choix })
     
    
    
    #   Gestion du rhandsontable pour int\\u00e9grer plus de praticit\\u00e9 pour l'utilisateur  ---------------------
    
    ## D\\u00e9finition du tableau
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
    
    
    
    # Mise \\u00e0 jour des inputs avec les donn\\u00e9es du tableau (uniquement si rempli)  
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
    #   # Affichez le graphique stock\\u00e9 dans l'objet reactiveValues
    #   print(graph_data$plot_prod)
    # })
    
    
    # output$test <- renderPrint({  input$location })
    # 
    # 
    # saveRoulettePlot <- function() {
    #   png_file <- tempfile(fileext = ".png")  # Cr\\u00e9e un fichier PNG temporaire
    #   png(png_file, width = 800, height = 600, units = "px", res = 100)
    #   plotRoulette()
    #   dev.off()
    #   graph_png_reactive(png_file)  # Stocke le nom du fichier PNG temporaire dans la variable r\\u00e9active
    # }
    # 
    # 
    # r[[paste("graph_png", type, sep = "_")]]
    
    
    
    ## Graphique distrib -----------------------
    
    
    graph_distibution <- reactive({
      req(is.numeric(distrib_finale()[1]))
      req(input$loi == TRUE)
      
      dist <- distrib_finale() %>% 
        as_tibble()
      
      ggplot(dist) +
        aes(x = value) +
        geom_histogram(bins = 10,  color = "#C2D3E1",fill = "#C2D3E1") +
        labs(
          x = paste0("Valeurs de ", type ),
          y = "Nombre de valeurs",
          title = "Repr\\u00e9sentation de l'\\u00e9chantillon",
          subtitle = "Echantillon d\\u00e9fini par Oser \\u00e0 partir de la trace manuelle"
        ) +
        theme_light()  +
        theme(
          panel.grid = element_line(linetype = 2, color = "grey70"),
          plot.title = element_text(size = 11,hjust = 0.5),
          plot.subtitle = element_text(size = 10,hjust = 0.5)
        ) +
        coord_cartesian(xlim =c(v_mini(), v_maxi())) +
        scale_x_continuous(breaks = seq(v_mini(),v_maxi(),
                                        length.out = 11/2))
      
      
    })
    
    
    output$graph_dist <- renderPlot( graph_distibution() )
    
    
    
    # output$graph_dist2 <- renderPlot( 
    #   distrib_finale() %>% 
    #     as_tibble() %>% 
    #     ggplot() +
    #                                     aes(x = value) +
    #                                     geom_density(adjust = 1L, fill = "#C2D3E1") +
    #                                     theme_minimal()
    # )
    # 
    # output$test_distrib<- renderPrint(v_myfit())
    
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
      
      r[[paste("titre", type, sep = "_")]]  <- gest_text_2()

      
      if(input$loi){
        r[[paste("saisie_distrib", type, sep = "_")]] <- "Distribution manuelle"
        r[[paste("graph_distrib", type, sep = "_")]] <- graph_distibution()
        
      } else {
        r[[paste("saisie_distrib", type, sep = "_")]] <- "Distribution uniforme"
      }
      
      }
    })
    

  
    
    
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
                       em(" \\u00e0 l'\\u00e9chelle "),  
                       em(r$echelle),
                       em(")"), sep ="" ))
          } 
      }      else if(type == "production"){
        if(is.null(r$unit_prod)){
          "Production et quantit\\u00e9" } else {
            HTML(paste("Production (",  em(r$unit_prod), ")", sep ="" ))
          } 
      }
      
    })
    
    
    gest_text_2 <- reactive({  # Pour le tableau des variables en sortie
      
      if(type == "prix"){
        if(is.null(r$unit_prix)){
          "Prix" } else {
            paste("Prix (",  r$unit_prix, ")", sep ="" )
          } 
        
      }     else if(type == "charges"){
        if(is.null(r$unit_e)){
          "Charges" } else {
            paste("Charges (",  
                       r$unit_e, 
                       " \\u00e0 l'\\u00e9chelle ",  
                       r$echelle,
                       ")", sep ="" )
          } 
      }      else if(type == "production"){
        if(is.null(r$unit_prod)){
          "Production et quantit\\u00e9" } else {
            paste("Production (",  r$unit_prod, ")", sep ="" )
          } 
      }
      
    })
    
    
    output$titre <- renderUI( gest_text()   )  
    
    
 

  })
}

## To be copied in the UI
# mod_box_distrib_ui("box_distrib_ui_1")

## To be copied in the server
# mod_box_distrib_server("box_distrib_ui_1")
