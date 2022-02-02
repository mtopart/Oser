#' Oser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom  SHELF fitdist
#' @importFrom dplyr %>% mutate case_when select
#' @importFrom shinyjs useShinyjs toggle
#' @importFrom bs4Dash box
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_histogram labs geom_boxplot geom_vline theme_light coord_cartesian theme element_blank
#' @importFrom plotly ggplotly plotlyOutput renderPlotly plot_ly add_segments add_trace layout
#' @importFrom stats quantile runif median 
#' @importFrom prompter add_prompt use_prompt
#' @importFrom shinyalert  shinyalert
#' @importFrom shinyWidgets materialSwitch actionBttn radioGroupButtons
#' 
mod_Oser_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      useShinyjs(),
      use_prompt(),

      
      tags$head(
        tags$style(
          HTML("[class*=hint--][aria-label]:after {
   white-space: pre;
}")
        )
      ),
        
      textOutput(ns("ma_sortie_mini")),
      textOutput(ns("ma_sortie_maxi")),
      
      ## Production------------------------------------
      
      box(
        title = "Production et quantité",
        icon = tags$span(icon("question")) %>% 
          add_prompt(
            position = "right",
            message = "Par unité de mesure sur une échelle choisie \n\nPar exemple : en tonnes pour 150 ha ou 1000 L pour l'atelier vaches laitières.",
            type = "info"
          )
        ,
        width = 4,
      
        wellPanel(
          numericInput(
            inputId = ns("prod_min"),
            label = "minimale",
            value = 20
          ),
          
          numericInput(
            inputId = ns("prod_max"),
            label = "maximale",
            value = 50
          )
        ),
        
        materialSwitch(
          inputId = ns("loi_prod"),
          label = "Je souhaite tracer une distribution (par défaut, la distribution sera uniforme)",
          value = FALSE,
          status = "primary"
        ),
        
        plotOutput(ns("roulette_p"),
                   click = ns("location_p")),
        p(em(
          id = ns("t_prod"), "Moyenne de la distribution :"
        )),
        textOutput(ns("mean_prod")),
        
        br(),
        actionButton(ns("button_prod"), icon("eye"))
      ),
      
      # Prix -----------------------------------
      
      box(
        title = "Prix",
        width = 4,
        icon = tags$span(icon("question")) %>% 
          add_prompt(
            position = "right",
            message = "Par unité de mesure sur une échelle choisie \n\nPar exemple : en €/t ou €/1000 L.",
            type = "info"),
        wellPanel(
          numericInput(
            inputId = ns("prix_min"),
            label = "minimal",
            value = 50
          ),
          
          numericInput(
            inputId = ns("prix_max"),
            label = "maximal",
            value = 150
          ),
          
          materialSwitch(
            inputId = ns("loi_prix"),
            label = "Je souhaite tracer une distribution (par défaut, la distribution sera uniforme)",
            value = FALSE,
            status = "primary"
          ),
          
          plotOutput(ns("roulette_px"),
                     click = ns("location_px")),
          p(em(
            id = ns("t_prix"), "Moyenne de la distribution :"
          )),
          textOutput(ns("mean_prix")),
          
          br(),
          actionButton(ns("button_prix"), icon("eye"))
        )
      ),
      
      # Charges -----------------------------------
      
      box(
        title = "Charges",
        width = 4,
        icon = tags$span(icon("question")) %>% 
          add_prompt(
            position = "left",
            message = "Par unité de mesure (€ ou k€) sur une échelle choisie \n\nPar exemple : charges sur 150 ha ou sur l'atelier vaches laitières.",
            type = "info")
        ,

        wellPanel(
          numericInput(
            inputId = ns("charges_min"),
            label = "minimales",
            value = 1000
          ),
          
          numericInput(
            inputId = ns("charges_max"),
            label = "maximales",
            value = 2000
          ) ,
          
          materialSwitch(
            inputId = ns("loi_charges"),
            label = "Je souhaite tracer une distribution, (par défaut, la distribution sera uniforme)",
            value = FALSE,
            status = "primary"
          ),
          
          
          plotOutput(ns("roulette_c"),
                     click = ns("location_c")),
          p(em(
            id = ns("t_charges"), "Moyenne de la distribution :"
          )),
          textOutput(ns("mean_charges")),
          
          br(),
          actionButton(ns("button_ch"), icon("eye")),
          tags$style(type='text/css', "#button_ch { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
          #,
          # textOutput(ns("ma_sortie_mini")),
          # textOutput(ns("ma_sortie_maxi"))
        )
      ),
      
      # Aide ---------------------------------------------------------
      
      # actionButton("button_help",  icon("wrench")),

      column( 
        12,
      materialSwitch(
        inputId = ns("aide"),
        label = strong("J'ai besoin d'aide pour comprendre comment tracer ma distribution"),
        value = FALSE,
        status = "primary",
        inline = TRUE
      ),
      
      p(id = ns("h1"), strong("Utilisation :")," Cliquez directement dans le graphique pour allouer un nombre de jetons à chaque intervalle. 
                           Cliquez juste en-dessous de la ligne du 0 de l'axe des y pour effacer une case."),
      p(id= ns("h2"), "Si un message d'erreur ", strong("rouge")," apparait, le nombre de jetons est insuffisant pour faire le calcul."), 
      br(id = ns("h3")),
      p(id = ns("h4"), strong("Pour mieux comprendre :"), "Plus la barre est haute, plus la probabilité que la valeur de l'intervalle soit atteinte dans la distribution est élevée.
                           Inversement, s'il n'y a pas de jeton dans une barre verticale, la probabilité qu'il y ait une valeur dans l'intervalle est", strong(" faible, mais possible.") 
        ),
      
      br()),

      
      # Go---------------------------------------------------------------------
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
      
      # Sorties--------------------------------------------------------------------- 
      
      box(
        title = "Conséquences sur le solde choisi (marge, EBE, revenu...)",
        width = 12,
        icon = tags$span(icon("question")) %>% 
          add_prompt(
            position = "right",
            message = "Voir onglet 'Tutoriels' - en construction",
            type = "info"),

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
          )))
        
    ))

}
    
#' Oser Server Functions
#'
#' @noRd 
mod_Oser_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    
    # # * Modification du slider pour la moyenne  ------------------------------------------------------------ 
    observe({
      #Production
      prod_min <- input$prod_min
      prod_max <- input$prod_max
      prod_moy <- (prod_min + prod_max) /2
        
      
      updateSliderInput(session, "mean_prod", 
                        min = prod_min, max = prod_max, value = prod_moy)
      
      
      #Prix
      prix_min <- input$prix_min
      prix_max <- input$prix_max
      prix_moy <- (prix_min + prix_max) / 2
      
      updateSliderInput(session, "mean_prix", 
                        min = prix_min, max = prix_max, value = prix_moy)
      
      #Charges
      charges_min <- input$charges_min
      charges_max <- input$charges_max
      charges_moy <- (charges_min + charges_max) /2
      
      updateSliderInput(session, "mean_charges", 
                        min = charges_min, max = charges_max, value = charges_moy)  
      
      
    })
   
    # Sortie des fonctions de distribution   ----------------------------------   
    fs <- 12
    nBins <- 10
    gridHeight <- 10
    
    # # * Production ------------------------------------------------------------  
    ### Unif-------------------------------------------
    
    production_unif <- reactive ({
      
      prod_min <- input$prod_min
      prod_max <- input$prod_max

      distribution <-runif(40, prod_min, prod_max)
      
      distribution 
    })  
    
    ### Distrib--------------------------------------------------------
    prod_min <- reactive({
      input$prod_min
    }) 
    
    prod_max <- reactive({
      input$prod_max
    }) 
    
    bin.width_p <- reactive({ bin.width(prod_min(), 
                                        prod_max()) })
    
    bin.left_p <- reactive({ bin.left(mini = prod_min(),
                                      maxi = prod_max(),
                                      width = bin.width_p()) })
    
    bin.right_p <- reactive({ bin.right(mini = prod_min(),
                                      maxi = prod_max(),
                                      width = bin.width_p()) })
    
    rl_p <- reactiveValues(x=-1, y=-1,
                           chips = rep(0, 10),
                           allBinsPr = NULL,
                           nonempty = NULL
    )
    
    
    output$roulette_p <- renderPlot({
      plot_elicit(name =  "de la production",
                  chips = rl_p$chips,
                  mini = prod_min(),
                  maxi = prod_max(),
                  left = bin.left_p(),
                  right = bin.right_p())
    })
    
    observeEvent(input$loi_prod, {toggle("roulette_p")})
    observeEvent(input$loi_prod, {toggle(id = "t_prod")})
    observeEvent(input$loi_prod, {toggle("mean_prod")})

    observeEvent(input$location_p, {
      rl_p$x <-input$location_p$x
      rl_p$y <-input$location_p$y
      plotHeight_p <- max(gridHeight, max(rl_p$chips) + 1)

      if(rl_p$x > prod_min() & rl_p$x < prod_max() & rl_p$y < plotHeight_p){
        index <- which(rl_p$x >= bin.left_p() & rl_p$x < bin.right_p())
        rl_p$chips[index]<-ceiling(max(rl_p$y, 0))
        rl_p$allBinsPr <- cumsum(rl_p$chips)/sum(rl_p$chips)
        rl_p$nonEmpty <- rl_p$allBinsPr > 0 & rl_p$allBinsPr < 1
      }
      })
    
    p_p <- reactive({
      rp <- rl_p$allBinsPr[rl_p$nonEmpty]
      myp <- rp
      myp
    })
    
    v_p <- reactive({
      rv <- bin.right_p()[rl_p$nonEmpty]
      myv <- rv
      myv
    })
    
    myfit_p <- reactive({
     fitdisti(mini = prod_min(),
              maxi = prod_max(),
              v = v_p(),
              p = p_p())
    })  
    
    
    production_distrib <-  reactive({
      calc_distrib(myfit = myfit_p(),
                   mini = prod_min(),
                   maxi = prod_max())    })
    

    loi_prod <- reactive({
      input$loi_prod
    }) 


    production <- reactive({
      if (input$loi_prod == TRUE) {
        production <- production_distrib()
      } else {
        production <- production_unif()
      }
      production %>%
        round(., digits = 1) %>%
        sort()
    })
    
    output$mean_prod <- renderText(mean(production())  %>%
                                     round(., digits = 1))
    
   
    # Aperçu de la distribution Shinyalert quand clique sur le bouton--------------
    
    observeEvent(input$button_prod, {
      # Show a modal when the button is pressed
      
      prod <- production()
      
    prod <- matrix(prod, 
                   ncol = 5)
      
    shinyalert(
      html = TRUE,
      title = "Distribution :", 
      text = tagList(
        tableOutput(ns("prod"))
      ), 
      type = "")
      
      output$prod <- renderTable(prod)
    })
    
    

    # #  * Prix ---------------------------------------------------------------
    
    ### Unif-------------------------------------------  
    
    prix_unif <- reactive ({
      
      prix_min <- input$prix_min
      prix_max <- input$prix_max
      mean_prix <- input$mean_prix
      sd_prix <- input$sd_prix
      
      
      distribution <-runif(40, prix_min, prix_max)
      
      distribution 
    })  
    
    ### Distrib--------------------------------------------------------
    
    
    prix_min <- reactive({
      input$prix_min
    })

    prix_max <- reactive({
      input$prix_max
    })

    bin.width_px <- reactive({ bin.width(prix_min(),
                                         prix_max()) })

    bin.left_px <- reactive({ bin.left(mini = prix_min(),
                                      maxi = prix_max(),
                                      width = bin.width_px()) })

    bin.right_px <- reactive({ bin.right(mini = prix_min(),
                                        maxi = prix_max(),
                                        width = bin.width_px()) })

    rl_px <- reactiveValues(x=-1, y=-1,
                           chips = rep(0, 10),
                           allBinsPr = NULL,
                           nonempty = NULL
    )


    output$roulette_px <- renderPlot({
      plot_elicit(name =  "du prix",
                  chips = rl_px$chips,
                  mini = prix_min(),
                  maxi = prix_max(),
                  left = bin.left_px(),
                  right = bin.right_px())
    })

    observeEvent(input$loi_prix, {toggle("roulette_px")})
    observeEvent(input$loi_prix, {toggle(id = "t_prix")})
    observeEvent(input$loi_prix, {toggle("mean_prix")})

    observeEvent(input$location_px, {
      rl_px$x <-input$location_px$x
      rl_px$y <-input$location_px$y
      plotHeight_px <- max(gridHeight, max(rl_px$chips) + 1)

      if(rl_px$x > prix_min() & rl_px$x < prix_max() & rl_px$y < plotHeight_px){
        index <- which(rl_px$x >= bin.left_px() & rl_px$x < bin.right_px())
        rl_px$chips[index]<-ceiling(max(rl_px$y, 0))
        rl_px$allBinsPr <- cumsum(rl_px$chips)/sum(rl_px$chips)
        rl_px$nonEmpty <- rl_px$allBinsPr > 0 & rl_px$allBinsPr < 1
      }
    })

    p_px <- reactive({
      rp <- rl_px$allBinsPr[rl_px$nonEmpty]
      myp <- rp
      myp
    })

    v_px <- reactive({
      rv <- bin.right_px()[rl_px$nonEmpty]
      myv <- rv
      myv
    })

    myfit_px <- reactive({
      fitdisti(mini = prix_min(),
               maxi = prix_max(),
               v = v_px(),
               p = p_px())
    })


    prix_distrib <-  reactive({
      calc_distrib(myfit = myfit_px(),
                   mini = prix_min(),
                   maxi = prix_max())    })



    prix <- reactive({
      if (input$loi_prix == TRUE) {
        prix <- prix_distrib()
      } else {
        prix <- prix_unif()
      }
      prix %>%
        round(., digits = 1) %>%
        sort()
    })
    

    
    output$mean_prix <- renderText(mean(prix())  %>%
                                     round(., digits = 1))
    
    # Aperçu de la distribution Shinyalert quand clique sur le bouton--------------
    
    observeEvent(input$button_prix, {
      # Show a modal when the button is pressed
      
      px <- prix()
      
      shinyalert(
        html = TRUE,
        title = "Distribution :", 
        text = tagList(
          verbatimTextOutput(ns("px"))
        ),
       type = "")
      
      output$px <- renderPrint(px)
        
    })
    

    # # * Charges -------------------------------------------------------------
    
    ### Unif-------------------------------------------
    
    charges_unif <- reactive ({
      
      charges_min <- input$charges_min
      charges_max <- input$charges_max
      
      distribution <-runif(40, charges_min, charges_max)
      
      distribution 
    })   
    
    ### Distrib--------------------------------------------------------
    
    charges_min <- reactive({
      input$charges_min
    })

    charges_max <- reactive({
      input$charges_max
    })


    bin.width_c <- reactive({ bin.width(charges_min(),
                                        charges_max()) })

    bin.left_c <- reactive({ bin.left(mini = charges_min(),
                                       maxi = charges_max(),
                                       width = bin.width_c()) })

    bin.right_c <- reactive({ bin.right(mini = charges_min(),
                                         maxi = charges_max(),
                                         width = bin.width_c()) })


    rl_c <- reactiveValues(x=-1, y=-1,
                           chips = rep(0, 10),
                           allBinsPr = NULL,
                           nonempty = NULL
    )

    output$roulette_c <- renderPlot({
      plot_elicit(name =  "des charges",
                  chips = rl_c$chips,
                  mini = charges_min(),
                  maxi = charges_max(),
                  left = bin.left_c(),
                  right = bin.right_c())
    })

    observeEvent(input$loi_charges, {toggle("roulette_c")})
    observeEvent(input$loi_charges, {toggle(id = "t_charges")})
    observeEvent(input$loi_charges, {toggle("mean_charges")})

    observeEvent(input$location_c, {
      rl_c$x <-input$location_c$x
      rl_c$y <-input$location_c$y
      plotHeight_c <- max(gridHeight, max(rl_c$chips) + 1) 

    if(rl_c$x > charges_min() & rl_c$x < charges_max() & rl_c$y < plotHeight_c){
      index <- which(rl_c$x >= bin.left_c() & rl_c$x < bin.right_c())
      rl_c$chips[index]<-ceiling(max(rl_c$y, 0))
      rl_c$allBinsPr <- cumsum(rl_c$chips)/sum(rl_c$chips)
      rl_c$nonEmpty <- rl_c$allBinsPr > 0 & rl_c$allBinsPr < 1
    }
    })


  p_c <- reactive({
    rp <- rl_c$allBinsPr[rl_c$nonEmpty]
    myp <- rp
    myp
  })

  v_c <- reactive({
    rv <- bin.right_c()[rl_c$nonEmpty]
    myv <- rv
    myv
  })


  myfit_c <- reactive({
    fitdisti(mini = charges_min(),
             maxi = charges_max(),
             v = v_c(),
             p = p_c())
  })


  charges_distrib <-  reactive({
    calc_distrib(myfit = myfit_c(),
                 mini = charges_min(),
                 maxi = charges_max())    })


charges <- reactive({
  if (input$loi_charges == TRUE) {
    charges <- charges_distrib()
  } else {
    charges <- charges_unif()
  }
  charges %>%
    round(., digits = 1) %>%
    sort()
})

    

output$mean_charges <- renderText(mean(charges())  %>%
                                 round(., digits = 1))

# Aperçu de la distribution Shinyalert quand clique sur le bouton--------------

observeEvent(input$button_ch, {
  # Show a modal when the button is pressed
  
  ch <- charges()
  
  shinyalert("Distribution :", 
             ch, type = "")
})

# Aide pour distribution-------------------

observe({
  toggle(id = "h1", condition = input$aide)
  toggle(id = "h2", condition = input$aide)
  toggle(id = "h3", condition = input$aide)
  toggle(id = "h4", condition = input$aide)
})

# Distribution du solde --------------------------------------------------



result <- eventReactive(input$goButton,{
  
  production <- production()
  prix <- prix()
  charges <- charges()
  
  crossing(
    production,
    prix,
    charges
  ) %>% 
    mutate(ca = production * prix,
           solde = ca - charges,
           risque = case_when(
             solde <= 0 ~ "solde inferieur a 0 €",
             solde > 0 & solde <= 2000 ~ "solde compris entre 0 et 2000 €",
             solde > 2000 ~ "solde superieur a 2 000 €"
           ))
})  


#Définition du texte -----------
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






# ## Liens avec les modules---------------------------------------


# observeEvent(r$data_val, {
#   ma_sortie <- r$data_val
#   
#   output$ma_sortie <- renderText(ma_sortie)
# })
# 
# 

# Mise à jour des charges


observeEvent(r$ch, {
  ma_sortie_mini <- r$ch
  
  output$ma_sortie_mini <- renderText(ma_sortie_mini)
})

observeEvent(r$ch2, {
  ma_sortie_maxi <- r$ch2
  
  output$ma_sortie_maxi <- renderText(ma_sortie_maxi)
})

# observeEvent(r$ch_maxi, {
#   ma_sortie_maxi <- r$ch_maxi
#   
#   output$ma_sortie_maxi <- renderText(ma_sortie_maxi)
# })




  }
)}
    
## To be copied in the UI
# mod_Oser_ui("Oser_ui_1")
    
## To be copied in the server
# mod_Oser_server("Oser_ui_1")
