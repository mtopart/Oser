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
      icon = tags$span(icon("question")) %>% 
        add_prompt(
          position = "right",
          message = def_help_text(type),  # Fonction pour modifier le message d'aide en fonction du type de variable
          type = "info"
        ),
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
                 click = ns("location")),
      p(em(
        id = ns("t_distrib"), "Moyenne de la distribution :"
      )),
      textOutput(ns("mean_distrib")),
      
      
      br(),
      actionButton(ns("button_distrib"), icon("eye")),

      if(type %in% "charges")  {
       h5("")},


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
      
      distribution <- repet_unif(mini = v_mini,
                                 maxi = v_maxi)
      distribution
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
    
    
    output$roulette <- renderPlot({
      plot_elicit(name =  graph_title,
                  chips = rl$chips,
                  mini = v_mini(),
                  maxi = v_maxi(),
                  left = v_bin.left(),
                  right = v_bin.right())
    })
    
    observeEvent(input$loi, {toggle("roulette")})
    observeEvent(input$loi, {toggle(id = "t_distrib")})
    observeEvent(input$loi, {toggle("mean_distrib")})
    
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
      
      fitdisti(mini = v_mini(),
               maxi = v_maxi(),
               v = v(),
               p = p())
    })
    
    
    distrib_man <-  reactive({
      calc_distrib(myfit = v_myfit(),
                   mini = v_mini(),
                   maxi = v_maxi()) 
    })
    
    
    
    ## Choix de la distribution finale en fonction du choix de l'utilisateur---------------
    
    distrib_finale <- reactive({
      if (input$loi == TRUE) {
        distrib <- distrib_man()
      } else {
        distrib <- distrib_unif()
      }
      distrib %>%
        round(., digits = 1) %>%
        sort()
    })
    
    
    output$mean_distrib <- renderText(mean(distrib_finale())  %>%
                                        round(., digits = 1))
    
    
    
    
    
    
    # Aperçu de la distribution Shinyalert quand clique sur le bouton--------------
    
    observeEvent(input$button_distrib, {
      #   # Show a modal when the button is pressed
      
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
    
    
    
    ## Liens avec les modules--------------------- 
    
    observeEvent( r$button , {
      r[[paste("dist_pr_graph", type, sep = "_")]] <- distrib_finale()
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
        if(is.null(r$unit_ch)){
          "Charges" } else {
            HTML(paste("Charges (",  
                       em(r$unit_ch), 
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
    
    
  })
}

## To be copied in the UI
# mod_box_distrib_ui("box_distrib_ui_1")

## To be copied in the server
# mod_box_distrib_server("box_distrib_ui_1")
