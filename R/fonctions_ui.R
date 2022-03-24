def_help_text <- function(type) {
  
  if(type == "production"){
    help_text <- "Par unité de mesure sur une échelle choisie \n\nPar exemple : en tonnes pour 150 ha ou 1000 L pour l'atelier vaches laitières."
    
  } else if (type == "prix") {
    help_text <- "Par unité de mesure sur une échelle choisie \n\nPar exemple : en €/t ou €/1000 L."
    
  } else {
    help_text <- "Par unité de mesure (€ ou k€) sur une échelle choisie \n\nPar exemple : charges sur 150 ha ou sur l'atelier vaches laitières."
    
  }
  
  return(help_text) }
