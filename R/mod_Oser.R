#' Oser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import  SHELF
#' @importFrom dplyr %>%
#'  
mod_Oser_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Production et quantité",
        width = 4,
        "(par unité de mesure sur une échelle choisie)",
        p(em("Par exemple : en tonnes pour 150 ha ou 1000 L pour l'atelier vaches laitières")),
        br(),
        wellPanel(numericInput(
          inputId = ns("prod_min"),
          label = "minimale",
          value = 20),
          
          numericInput(
            inputId = ns("prod_max"),
            label = "maximale",
            value = 50)
      ),
      
      materialSwitch(
        inputId = ns("loi_prod"),
        label = "Je souhaite tracer une distribution (par défaut, la distribution sera uniforme)", 
        value = FALSE,
        status = "primary"
      ),
      
      conditionalPanel(
        condition = "input.loi_prod == 1",
        plotOutput(ns("roulette_p"),
                   click = "location_p"),
        p(em("Moyenne de la distribution :")),
        textOutput(ns("mean_prod")),
        hr()
      ),
      p(em("Distribution :")),
      verbatimTextOutput(ns("distrib"))
      ),
      
      
      box(
        title = "Prix",
        width = 4
      ),
      
      box(
        title = "Charges",
        width = 4
        
      )
    
 
  ))
}
    
#' Oser Server Functions
#'
#' @noRd 
mod_Oser_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # # * Production ------------------------------------------------------------ 
    observe({
      #Production
      prod_min <- input$prod_min
      prod_max <- input$prod_max
      prod_moy <-  (prod_min + prod_max) /2
      
      updateSliderInput(session, "mean_prod", 
                        min = prod_min, max = prod_max, value = prod_moy)
    })
    
    ### Unif-------------------------------------------
    
    production_unif <- reactive ({
      
      prod_min <- input$prod_min
      prod_max <- input$prod_max
      mean_prod <- input$mean_prod
      sd_prod <- input$sd_prod
      
      
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
    
    
    bin.width_p <- reactive({
      req(prod_min(), prod_max())
      (prod_max() - prod_min()) / nBins
    })
    
    bin.left_p <- reactive({
      req(prod_min(), prod_max(), bin.width_p())
      seq(from = prod_min(),
          to = prod_max() - bin.width_p(),
          length=nBins)
    })
    
    bin.right_p <- reactive({
      req(prod_min(), prod_max(), bin.width_p()) 
      seq(from = prod_min()[1] + bin.width_p(),
          to = prod_max(),
          length = nBins)
    })
    
    
    rl_p <- reactiveValues(x=-1, y=-1,
                           chips = rep(0, 10),
                           allBinsPr = NULL,
                           nonempty = NULL
    )
    
    
    output$roulette_p <- renderPlot({
      
      plotHeight_p <-  max(gridHeight, 
                           max(rl_p$chips) + 1)
      
      par(ps = fs)
      plot(c(prod_min(), prod_max()), c(0, 0),
           xlim=c(prod_min(), prod_max()),
           ylim=c(-1, plotHeight_p),
           type="l",
           ylab="Fréquence",
           xaxp=c(prod_min()[1], prod_max(), nBins), 
           main = paste("Total des jetons  :", sum(rl_p$chips)),
           xlab = "Répartition de la production")
      for(i in 1:nBins){
        lines(c(bin.left_p()[i],bin.left_p()[i]),
              c(0, plotHeight_p),lty=3,col=8)
      }
      lines(c(bin.right_p()[nBins],bin.right_p()[nBins]),
            c(0, plotHeight_p),lty=3,col=8)
      
      for(i in 1:plotHeight_p){
        lines(c(prod_min(), prod_max()),
              c(i,i), lty=3,col=8)
      }
      
      for(i in 1:nBins){
        if(rl_p$chips[i]>0){
          rect(rep(bin.left_p()[i],rl_p$chips[i]),c(0:(rl_p$chips[i]-1)),
               rep(bin.right_p()[i],rl_p$chips[i]),c(1:rl_p$chips[i]),col=2)
        }
      }
      
    })
    
    
    
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
      req(prod_max(), prod_min(), v_p(), p_p())
      myfit_p<- fitdist(vals = v_p(), probs = p_p(), lower = prod_min(),
                        upper = prod_max() )
      myfit_p
    })  
    
    
    production_distrib <-  reactive({
      
      
      if (myfit_p()$best.fitting[1] == "normal") {
        
        distribution <- EnvStats::rnormTrunc(40, min = prod_min(), max = prod_max(),
                                             mean = myfit_p()$Normal$mean,
                                             sd = myfit_p()$Normal$sd)
        
      } else if (myfit_p()$best.fitting[1] == "t" | myfit_p()$best.fitting[1] == "logt") {
        
        
        distribution <- crch::rct(40, location = myfit_p()$Student.t$location,
                                  scale = myfit_p()$Student.t$scale,
                                  df = myfit_p()$Student.t$df,
                                  left = prod_min(),
                                  right = prod_max()
        )
        
      } else if (myfit_p()$best.fitting[1] == "gamma") {
        
        distribution <- RGeode::rgammatr(40, A = myfit_p()$Gamma$shape,
                                         B = myfit_p()$Gamma$rate,
                                         range = c(prod_min(),prod_max())
        )
        
      } else if (myfit_p()$best.fitting[1] == "lognormal") {
        
        distribution <- EnvStats::rlnormTrunc(40, min = prod_min(), max = prod_max(),
                                              meanlog = myfit_p()$Log.normal$mean.log.X,
                                              sdlog = myfit_p()$Log.normal$sd.log.X)
        
      } else if (myfit_p()$best.fitting[1] == "beta") {
        
        
        
        distribution <-  prod_min() + (prod_max() - prod_min())  *
          qbeta((1:40/41), myfit_p()$Beta$shape1, myfit_p()$Beta$shape2)
      }
      
      distribution
      
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
    
    output$mean_prod <- renderText(mean(production()))
    
    output$distrib <- renderPrint(production())
    
 
  })
}
    
## To be copied in the UI
# mod_Oser_ui("Oser_ui_1")
    
## To be copied in the server
# mod_Oser_server("Oser_ui_1")
