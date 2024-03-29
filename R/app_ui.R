#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(insertTab, actionButton, tabsetPanel, column))
#' @import bs4Dash 
#' @import waiter
#' @importFrom gfonts use_font
#' @noRd
#' 
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    use_font(
      id = "cabin-sketch",
        css_path = "inst/app/www/css/cabin-sketch.css",
      selector = c("h1", "h3", "h4", "h5", "h6", "span", "p", "button", "h2"
                   )
    ), 
    
#     
# #browser pour developpement
# 
#     actionButton("browser", "browser"),
#     tags$script("$('#browser');"),

    # Define this page as a dashboard page to signal we're using the     dashboard page format
    dashboardPage(
      
      preloader = list(html = tagList(spin_1(), "Chargement en cours ..."), color = "#3c8dbc"),
      
      
      title = "Oser",
      freshTheme = TRUE,
      #help = TRUE,
      #fullscreen = TRUE,
      header = dashboardHeader(
        title = bs4DashBrand("Oser",
                             color = NULL , href = NULL, image = NULL, opacity = 0.8),      
        actionButton(inputId = "controlbarToggle", label = "Gestion des unit\u00e9s", class = "mx-2")
      ),
      
      # Create our navigation menu that links to each of the tabs we defined
      sidebar = dashboardSidebar(
        skin = "light",
        status = "primary",
        collapsed = FALSE,
        minified = TRUE,
        elevation = 3,
        sidebarUserPanel(
          image = "https://cdn-icons-png.flaticon.com/512/1055/1055683.png",
          name = span("Welcome Onboard!")
        ),
        
        # Définition des menus de la barre de gauche      
        sidebarMenu(
          id = "tabs",
          
          # menuItem("L'outil", icon = icon("dashboard"), startExpanded = TRUE,
          #          menuSubItem(
          #            "Saisie", icon = icon("arrow-right"), tabName = "saisie_unit"),
          #          menuSubItem(
          #            "Oser", 
          #            icon = icon("arrow-right"), 
          #            tabName = "oser")),
          
          menuItem("Pour commencer", 
                   icon = icon("arrow-right"), 
                   tabName = "saisie_unit"),
          
          menu_to_hide, # to hide
          
          # menuItem("L'outil", 
          #          icon = icon("dashboard"), 
          #          tabName = "oser"
          #          ),
          
          menuItem("Tutoriels", icon = icon("tools"), tabName = "tuto"),
          menuItem("A propos", icon = icon("th"), tabName = "apropos")
          
        )),
      
      # D\u00e9finition des menus de la barre de gauche       
      footer = dashboardFooter(
        left = "Oser - Outil de Simulation du risque Economique et des Revenus - V1.0",
        right = "2023 - Projet Strat&co"
      ),
      
      
      body = dashboardBody(
        tabItems(
          tabItem("saisie_unit", 
                    mod_onglet_unit_ui("onglet_unit_1"),
                  tags$head(
                    tags$script(
                      "$(function(){
                Shiny.addCustomMessageHandler('toggle-tab-item', function(message) {
                  console.log(message);
                  $('a[data-value=\"'+ message + '\"]')
                    .parent('li')
                    .show();
                });
              });"
                    )
                  )),
          
          tabItem("oser", 
                  
                  # Mode simple Oser
                  
                  # conditionalPanel(
                  #   'input.select == 1',
                  
                  fluidRow(
                    mod_box_distrib_ui("box_distrib_ui_1", 
                                       type = "production",
                                       input_mini = 20,
                                       input_maxi = 50),
                    mod_box_distrib_ui("box_distrib_ui_2", 
                                       type = "prix",
                                       input_mini = 50,
                                       input_maxi = 150),
                    mod_box_distrib_ui("box_distrib_ui_3", 
                                       type = "charges",
                                       input_mini = 1000,
                                       input_maxi = 2000),
                    mod_aide_distrib_ui("aide_distrib_ui_1"),
                    mod_zone_graph_ui("zone_graph_1")
                    ,
                    mod_telechargement_ui("telechargement_1")
                    
                    
                  )                      
          ),
          
          # # Mode avancé
          # conditionalPanel(
          #   'input.select == 2',
          #   box(
          #     p("En construction", style = "color:red") )
          # ,
          # uiOutput("tab"))),
          
          # Reste des items
          
          tabItem("tuto", mod_tutoriel_ui("tutoriel_ui_1")),
          tabItem("apropos", mod_apropos_ui("apropos_ui_1"))
          )
        ),
      #,
      
      # # Création de la controlbar -----------------------------
      
      # controlbar = dashboardControlbar(
      #   skin = "light",
      #   # pinned = FALSE,
      #   # collapsed = FALSE,
      #   # overlay = FALSE,
      #   controlbarMenu(
      #     id = "controlbarmenu",
      #     controlbarItem(
      #       title = "Item 1",
      #       selectInput("select", 
      #                   label = "Select box", 
      #                   choices = list("Simple" = 1, 
      #                                  "Avanc\u00e9" = 2), 
      #                   selected = 1),
      #       conditionalPanel(
      #         'input.select == 2',
      #         numericInput(
      #           inputId = "obs",
      #           label = "Number of observations:",
      #           min = 1,
      #           max = 10,
      #           value = 1
      #         ))
      #     ),
      #     controlbarItem(
      #       "Item 2",
      #       textOutput("text_test")
      #     )
      #   )
      # )
      
      controlbar = dashboardControlbar(
        id = "controlbar",
        skin = "light",
         #pinned = FALSE,
         #collapsed = FALSE,
        overlay = FALSE,
        width = 500,
        mod_unites_secondaire_ui("gestion_unites_1")
          )
          )
      )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @rawNamespace import(shiny, except = c(insertTab, actionButton, tabsetPanel, column))
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Oser'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

