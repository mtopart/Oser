def_help_text <- function(type) {
  
  if(type == "production"){
    help_text <- "Par unit\\u00e9s de mesure sur une \\u00e9chelle choisie \n\nPar exemple : en tonnes pour 150 ha ou 1000 L pour l'atelier vaches laiti\\u00e8res."
    
  } else if (type == "prix") {
    help_text <- "Par unit\\u00e9s de mesure sur une \\u00e9chelle choisie \n\nPar exemple : en \\u20ac/t ou \\u20ac/1000 L."
    
  } else {
    help_text <- "Par unit\\u00e9s de mesure (\\u20ac ou k\\u20ac) sur une \\u00e9chelle choisie \n\nPar exemple : charges sur 150 ha ou sur l'atelier vaches laiti\\u00e8res."
    
  }
  
  return(help_text) }



menu_tab <- function(v_obs) {
  
 lapply(1 : v_obs, function(i) {
  tabPanel(
    sprintf("Menu %s", i),
    sprintf("Hello tab %s", i)
  )
  })
  
}


menu_to_hide <- bs4Dash::menuItem("L'outil", icon = icon("dashboard"), tabName = "oser")
menu_to_hide$attribs$style <- "display: none ;"
