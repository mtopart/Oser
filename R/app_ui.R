#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(insertTab, actionButton, tabsetPanel, column))
#' @import bs4Dash 
#' @import waiter
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    
    # #browser pour développement

    actionButton("browser", "browser"),
    tags$script("$('#browser');"),

    # Define this page as a dashboard page to signal we're using the     dashboard page format
    dashboardPage(
      
      preloader = list(html = tagList(spin_1(), "Chargement en cours ..."), color = "#3c8dbc"),
      
      title = "Oser",
      freshTheme = TRUE,
      fullscreen = TRUE,
      header = dashboardHeader(
        title = bs4DashBrand("Oser",
                             color = NULL , href = NULL, image = NULL, opacity = 0.8),
        actionButton(inputId = "controlbarToggle", label = "Gestion des unités", class = "mx-2")
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
          name = "Welcome Onboard!"
        ),
        
        # Définition des menus de la barre de gauche      
        sidebarMenu(
          id = "tabs",
          
          menuItem("L'outil", icon = icon("dashboard"), startExpanded = TRUE,
                   menuSubItem(
                     "Oser", icon = icon("arrow-right"), tabName = "oser")),
          
          menuItem("Tutoriels", icon = icon("tools"), tabName = "tuto"),
          menuItem("A propos", icon = icon("th"), tabName = "apropos")
          
        )),
      
      # Définition des menus de la barre de gauche       
      footer = dashboardFooter(
        left = "Prototype - application en développement",
        right = "2022 - Strat&co"
      ),
      
      
      body = dashboardBody(
        tabItems(
          
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
                    mod_graph_final_ui("graph_final_ui_1")
                    
                    
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
      #                                  "Avancé" = 2), 
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
         mod_gestion_unites_ui("gestion_unites_1")
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

