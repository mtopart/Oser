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
#' @importFrom shinyWidgets pickerInput
mod_onglet_unit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
    tags$style(
      type = "text/css",
      ".inline label{ display: table-cell; padding-right: 1em; vertical-align: middle; } 
                 .inline .form-group{display: table-row;}
      "
    ) ),
    
    
  
  fluidPage(fluidRow(
    # A propos
    column(
      6,
      wellPanel(
        style = "
                border-left: 6px solid #2196F3!important;
                background: white;",

        htmlOutput(ns("video"))
        

      )
      #,
      #htmlOutput(ns("presentation"))
    ),

    column(
    6,
      style = "background: white;",
    
      h4(class = "titre", "Avant de me lancer..."),
    
  
    
    tags$button(
      id = "web_button",
      class = "btn action-button",
      tags$img(src = "www/Image7.png",
               height = "100px"),
      onclick ="window.open('https://view.genial.ly/650af2df6c8849001028e14f', '_blank')"
    ),
    
    
      tags$div(
        class = "inline",
        
      h6("Choix des unit\u00e9s et des param\u00e8tres :",
         style ="text-decoration: underline;" ),
      
        tooltip( ##val_echelle-----------------
          textInput(ns("val_echelle"), "Je veux visualiser mes r\u00e9sultats \u00e0 l'\u00e9chelle"),
          title = "D'un atelier, d'un groupe de culture, d'une culture, d'un hectare... "
        ),

        div(
          id = ns("p2"),
          tooltip(
          textInput( ##val_unit_prod-------------
            ns("val_unit_prod"), "La production s'exprime en "),
          title = "1 000 L, tonnes, nombre de paniers" ))
      #,
      
      ), # div inline

        div(
          id = ns("p3"),
   
          fluidRow(   
            br(),
    column(6,
          #  pickerInput(
          # ns("val_unit_e"), ## val_unit_e----------
          # label = "Et son prix en ",
          # choices = c("\u20ac",
          #             "k\u20ac"),
          # selected = 1,
          # width = "fit",
          # inline = TRUE,
          # options = list(
          #   class="selectize-control shiny-input-select shinyjs-resettable shiny-bound-input single plugin-selectize-plugin-a11y",
          #                style = "background-color: white;"
          #   ),
          # choicesOpt = list(
          #     style = rep_len("font-size: 150%; line-height: 1.6;", 2)
          #   )
          # )
          
          
          selectInput(
            ns("val_unit_e"), ## val_unit_e----------
            label = "Et son prix en ",
            choices = c("\u20ac",
                        "k\u20ac"),
            selected = 1,
            
          )
          
        ),
    
    
    column(6,
         #  class = "inline",
        tooltip(
          textInput(ns("val_unit_p"), "par"), ## val_unit_p----------
          title = "1 000 L, tonne, panier")
    ))
        ), 
        
        div(
          id = ns("p4"),
          br(),

          htmlOutput(ns("text1")) %>% 
         # tagAppendAttributes(style ="color:#FF0000;"),
           tagAppendAttributes(style ="color:blue;"),
        br(),
        
        selectInput(
          ns("val_unit_solde"),  ## val_unit_solde----------
          label = "Mon solde final sera un/une ",
          choices = c(
            "Marge brute",
            "EBE",
            "Valeur ajout\u00e9e",
            "Autre" = "autre"
          ),
          selected = 1
        ),
        textInput(ns("val_unit_sautre"), "Solde choisi") ## val_unit_sautre----------
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
      )

    )
   ))
  )
  
  
}






#' onglet_unit Server Functions
#'
#' @noRd
mod_onglet_unit_server <- function(id, r, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
        observe({
      updateTextInput(session, "val_echelle", value = r$echelle) })

    observe({
      updateTextInput(session, "val_unit_prod", value = r$unit_prod) })

    observe({
      updateTextInput(session, "val_unit_p", value = r$unit_prix_p) })

    observe({
      updateSelectInput(
        session,
        "val_unit_solde",
        choices = list("Marge brute",
                       "Marge nette",
                       "EBE",
                       "Autre" = "autre"),
        selected = r$select_solde
      )})

    # observe({
    #   updateSelectInput(
    #     session,
    #     "val_unit_e",
    #     choices = c("\u20ac",
    #                 "k\u20ac"),
    #     selected = r$unit_e
    #   ) 
    #   })

    observe({
      updateTextInput(session, "val_unit_sautre", value = r$solde2)

    })
     
    
  output$text1 <- renderUI({
    req(input$val_echelle)
    req(input$val_unit_e)
    
    paste( "<span style='text-decoration: underline;'>Attention !</span>",
           "Lors de la saisie des charges :",
           "- garder une coh\u00e9rence entre le type de charges saisies et le solde choisi",
    paste0("- les renseigner \u00e0 l'\u00e9chelle ", input$val_echelle, 
           " et en ", input$val_unit_e,"." ) 
    , sep ="<br/>" ) %>% 
      HTML()
    
  })  
    
  # output$text2 <- renderText({
  # 
  #   paste0("en  ",  input$val_unit_e,"." )
  #   
  # })  
    observe({
      toggle("p2", condition = input$val_echelle)
      toggle("p3", condition = input$val_unit_prod)
      toggle("p4", condition = input$val_unit_p)
      toggle("val_unit_sautre", condition = input$val_unit_solde == "autre")
      toggle("butt_oser", condition = input$val_unit_p)
    })
    
  
    unit_solde <- reactive({
      
    d <-  if(input$val_unit_solde == "autre"){ input$val_unit_sautre
    } else { input$val_unit_solde }
    
    paste0(d, " (en ", input$val_unit_e,")" )
      
    })  
    
    unit_solde2 <- reactive({
      
      d <-  if(input$val_unit_solde == "autre"){ input$val_unit_sautre
      } else { input$val_unit_solde }
      
     d
      
    })  
    
    unit_prix <- reactive({
      paste0(input$val_unit_e, " / ", input$val_unit_p)
    })
    
    # Video--------------------------
    output$video <- renderUI({
      HTML(
        '<div style="width: 100%;"><div style="position: relative;
        padding-bottom: 100%; padding-top: 0; height: 0;"><iframe 
        title="video_oser_sson" frameborder="0" width="800" height="800" 
        style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
        src="https://view.genial.ly/6553cab9eb1630001165ae6c" type="text/html"
        allowscriptaccess="always" allowfullscreen="true" scrolling="yes"
        allownetworking="all"></iframe> </div> </div>')
    })
    
    # Sortie des modules----------------------------------
    observeEvent( input$button_v_oser, {
      
       r$button_unit <- input$button_v_oser

       r$echelle <- input$val_echelle
       r$unit_prix <- unit_prix()     # Ex : €/t
       r$unit_prix_p <- input$val_unit_p  # Ex : t
       r$unit_prod <- input$val_unit_prod # Ex : tonnes
       r$unit_e <-   input$val_unit_e # Ex : €
       r$solde <- unit_solde()  # Unit\u00e9 finale ( en €)
       r$solde2 <- unit_solde2()
       r$select_solde <- input$val_unit_solde  # unit\u00e9 rentr\u00e9e

       session$sendCustomMessage("toggle-tab-item", "oser")
       
       updateTabItems(session = parent_session,
                      "tabs",
                      selected = "oser")
    })  
    

    output$presentation <- renderUI({
      HTML(
        '<div style="width: 80%;"><div style="position: relative; padding-bottom: 56.25%; padding-top: 0; height: 0;"><iframe title="Tutoriel Enregistrement"
        frameborder="0" width="1200" height="675" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;" 
        src="https://view.genial.ly/650c8cc6504ecb00114f906a" type="text/html" allowscriptaccess="always" allowfullscreen="true" 
        scrolling="yes" allownetworking="all"></iframe> </div> </div>')
    }) 
    
    
  })
}

## To be copied in the UI
# mod_onglet_unit_ui("onglet_unit_1")

## To be copied in the server
# mod_onglet_unit_server("onglet_unit_1")




# Aide CSS------------------------------------
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

