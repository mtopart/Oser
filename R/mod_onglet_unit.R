#' onglet_unit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash tooltip actionButton
#' @importFrom shinyjs show
mod_onglet_unit_ui <- function(id) {
  ns <- NS(id)
  tagList(tags$head(
    tags$style(
      type = "text/css",
      "#inline label{ display: table-cell;
                  font-family:  Cabin Sketch; vertical-align: middle;}
                 #inline .form-group { display: table-row;}"
    ) ),
  
  fluidPage(fluidRow(
    # A propos
    column(
      4,
      wellPanel(
        style = "
                border-left: 6px solid #2196F3!important;
                background: white;",

        p("  A propos à rédiger"),
        

      )
    ),

    column(
      8,
      style = "background: white;",
      tags$div(
        id = "inline",

        tooltip( ##val_echelle-----------------
          textInput(ns("val_echelle"), "Je veux visualiser mes résultats à l'échelle"),
          title = "D'un atelier, d'un groupe de culture, d'une culture, d'un hectare... "
        ),

        div(
          id = ns("p2"),
          tooltip(
          textInput( ##val_unit_prod-------------
            ns("val_unit_prod"), "Sur cette échelle de travail, ma production s'exprime en "),
          title = "1 000 L, tonnes, nombre de paniers" )),

        div(
          id = ns("p3"),
          
          selectInput(
          ns("val_unit_e"), ## val_unit_e----------
          label = "Et son prix en ",
          choices = c("€",
                      "k€")
        ),
        tooltip(
          textInput(ns("val_unit_p"), "par"), ## val_unit_p----------
          title = "1 000 L, tonne, panier")), 
        
        div(
          id = ns("p4"),
          br(),
        textOutput(ns("text1")) %>% 
          tagAppendAttributes(style ="font-family:  Cabin Sketch;"),
        p(style ="font-family:  Cabin Sketch;",
          "Le solde obtenu sera différent en fonction du type de charges rentrées dans le calculateur."),
        
        selectInput(
          ns("val_unit_solde"),  ## val_unit_solde----------
          label = "Mon solde final sera un/une ",
          choices = c(
            "Marge brute",
            "EBE",
            "Valeur ajoutée",
            "Autre" = "autre"
          )
        ),
        textInput(ns("val_unit_sautre"), "Solde choisi"), ## val_unit_sautre----------
        textOutput(ns("text2")) %>% 
          tagAppendAttributes(style ="font-family:  Cabin Sketch;")
      ),
      
      # column(
      #   12,
      div(
        id = ns("butt_oser"), ## button----------
        align = "center",
        hr(),
        actionButton(ns("button_v_oser"), 
                     icon("play"),
                     status = "primary",
                     label = "C'est parti !")
      # infoBox(
      #   title = "C'est parti !",
      #   color = "info",
      #   icon = icon("play"),
      #   tabName = "oser"
      # )%>% 
      #   tagAppendAttributes(style ="font-family:  Cabin Sketch;")
      )

    )
   ))
  ))
  
  
}






#' onglet_unit Server Functions
#'
#' @noRd
mod_onglet_unit_server <- function(id, r, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
  output$text1 <- renderText({
    req(input$val_echelle)
    req(input$val_unit_e)
    
    paste0("Les charges seront à renseigner à l'échelle ", input$val_echelle, 
           " en ", input$val_unit_e,"." )
    
  })  
    
  output$text2 <- renderText({
  
    paste0("en  ",  input$val_unit_e,"." )
    
  })  
    observe({
      toggle("p2", condition = input$val_echelle)
      toggle("p3", condition = input$val_unit_prod)
      toggle("p4", condition = input$val_unit_p)
      toggle("val_unit_sautre", condition = input$val_unit_solde == "autre")
      toggle("butt_oser", condition = input$val_unit_p)
    })
    
  
    unit_solde <- reactive({
      
    d <-  if(input$val_unit_solde == "autre"){ val_unit_sautre
    } else { input$val_unit_solde }
    
    paste0(d, " en ", input$val_unit_e )
      
    })  
    
    
    unit_prix <- reactive({
      paste0(input$val_unit_e, " / ", input$val_unit_p)
    })
    
    
    observeEvent( input$button_v_oser, {
      
      r$check_saisie <- "oui"
      
       r$button_unit <- input$button_v_oser

       r$echelle <- input$val_echelle
       r$unit_prix <- unit_prix()
       r$unit_prod <- input$val_unit_prod
       r$unit_ch <-   input$val_unit_e
       r$solde <- unit_solde()

       updateTabItems(session = parent_session,
                      "tabs",
                      selected = "oser")
    })  
    
    
    
  })
}

## To be copied in the UI
# mod_onglet_unit_ui("onglet_unit_1")

## To be copied in the server
# mod_onglet_unit_server("onglet_unit_1")




# Aide CSS
# library(shiny)
#
# ui <- fluidPage(
#
#   fluidRow(
#
#     tags$head(
#       tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
#     ),
#
#     textInput(inputId = "txtInp", label = "Label:"),
#     numericInput(inputId = "numInp", label = "Label:", value = 0)
#   )
# )
#
# server <- function(input, output){}
#
#
# shinyApp(ui, server)
#
#
#
#
# library(shiny)
#
# ui <- fluidPage(
#
#   fluidRow(
#
#     tags$head(
#       tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
#                #inline .form-group { display: table-row;}")
#     ),
#
#     tags$div(id = "inline", textInput(inputId = "txtInp", label = "Label:")),
#     numericInput(inputId = "numInp", label = "Label:", value = 0)
#   )
# )
#
# server <- function(input, output){}
#
#
# shinyApp(ui, server)
#
# Pour avoir du scrollable
# style = "overflow-y:scroll; max-height: 600px",
