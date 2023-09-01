# Fonctions nécessaires pour faire tourner Oser

# Création du graphique d'entrée (élicitation)

#' @importFrom dplyr pull summarise group_by  mutate
#' @importFrom purrr map_dfc
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with



bin.width <- function(mini,
                      maxi,
                      nBins = 10) {
  req(mini, maxi)

      (maxi - mini) / nBins

}




bin.left <- function(mini, 
                     maxi,
                     width,
                     nBins = 10) {
  req(mini, maxi, width)
  
  seq(from = mini,
      to = maxi - width,
      length = nBins)
  
}


# p <- bin.width(min = 10, max = 50)
# bin.left(10, 50, 4, 10)
#bin.right(10, 50, 4, 10)

bin.right <- function(mini, 
                      maxi,
                      width,
                      nBins = 10){
  req(mini, maxi, width)
  
  seq(from = mini[1] + width,
      to = maxi,
      length = nBins)
}




#' @importFrom graphics par lines rect

plot_elicit <- function(name,
                        chips,
                        mini,
                        maxi,
                        left,
                        right,
                        gridHeight = 10,
                        fs = 12,
                        nBins = 10){
  
  
  plotHeight <-  max(gridHeight, 
                       max(chips) + 1)
  
  par(ps = fs)
  plot(c(mini, maxi), c(0, 0),
       xlim=c(mini, maxi),
       ylim=c(-1, plotHeight),
       type="l",
       ylab="",
       xaxp=c(mini[1], maxi, nBins), 
       main = paste0("Répartissez vos jetons (", sum(chips), ")"),
       xlab =  name)
  for(i in 1:nBins){
    lines(c(left[i],left[i]),
          c(0, plotHeight),lty=3,col=8)
  }
  lines(c(right[nBins],right[nBins]),
        c(0, plotHeight),lty=3,col=8)
  
  for(i in 1:plotHeight){
    lines(c(mini, maxi),
          c(i,i), lty=3,col=8)
  }
  
  for(i in 1:nBins){
    if(chips[i]>0){
      rect(rep(left[i],chips[i]),c(0:(chips[i]-1)),
           rep(right[i],chips[i]),c(1:chips[i]),col=2)
    }
  }  
  }


# plot_elicit(name = "prod",
#             chips = rep(0, 10),
#             left = 4,
#             right =  40,
#             mini = 10,
#             maxi = 50)




tryCatch_mod <- function(vals, probs, lower = -Inf,
                          upper = Inf, weights = 1, tdf = 3,
                          expertnames = NULL){
  tryCatch(fitdist_mod(vals, probs, lower, 
                       upper, weights = 1, tdf = 3,
                       expertnames = NULL),
           error = function(e) {
             if (conditionMessage(e) %in% c("smallest elicited probability must be less than 0.4",
                                            "probabilities must be between 0 and 1",
                                            "largest elicited probability must be greater than 0.6",
                                            "elicited parameter values cannot be smaller than lower parameter limit",
                                            "elicited parameter values cannot be greater than upper parameter limit",
                                            "Student-t degrees of freedom must be greater than 0",
                                            "probabilities must be specified in ascending order",
                                            "parameter values must be specified in ascending order")) 
               return(NULL)})
  
}


fitdisti <- function(mini,
                     maxi,
                     v,
                     p){
  
  # req(maxi, mini, v, p)
  # myfit <- fitdist_mod(vals = v, probs = p, lower = mini,
  #                   upper = maxi )
  
  myfit <- tryCatch_mod(vals = v, probs = p, lower = mini,
                       upper = maxi )
  myfit
}


# myfit <- fitdisti(mini = c(10,0),
#                   maxi = c(100, Inf),
#                   v = matrix(c(20,30,50,55,60,70),3,2),
#                   p = c(0.25,0.5,0.75))



calc_distrib <- function (myfit,
                          mini,
                          maxi) {
  
  
  
  if (myfit$best.fitting[1] == "normal") {
    
    
    
    

    
    distribution <- map_dfc(
      1:50, ~{ EnvStats::rnormTrunc(50, min = mini, max = maxi,
                                    mean = myfit$Normal$mean,
                                    sd = myfit$Normal$sd) %>% 
          sort()}) %>% 
      mean_distrib()

    
    
  } else if (myfit$best.fitting[1] == "t") {
    
    
    distribution <- map_dfc(
      1:50, ~{ crch::rtt(50, location = myfit$Student.t$location,
                         scale = myfit$Student.t$scale,
                         df = myfit$Student.t$df,
                         left = mini,
                         right = maxi) %>% 
          sort()}) %>% 
      mean_distrib()
    
  } else if ( myfit$best.fitting[1] == "logt") {
    
    
    distribution <- map_dfc(
      1:50, ~{ crch::rtt(50, location = myfit$Student.t$location,
                         scale = myfit$Student.t$scale,
                         df = myfit$Student.t$df,
                         left = mini,
                         right = maxi) %>% 
          sort()}) %>% 
      mean_distrib()
    
  }  else if (myfit$best.fitting[1] == "gamma") {
    

    distribution <- map_dfc(
      1:50, ~{ RGeode::rgammatr(50, A = myfit$Gamma$shape,
                                          B = myfit$Gamma$rate,
                                          range = c(mini, maxi)) %>% 
          sort()}) %>% 
      mean_distrib()
    
  } else if (myfit$best.fitting[1] == "lognormal") {
    
    distribution <- map_dfc(
      1:50, ~{ EnvStats::rlnormTrunc(50, min = mini, max = maxi,
                                     meanlog = myfit$Log.normal$mean.log.X,
                                     sdlog = myfit$Log.normal$sd.log.X) %>% 
          sort()}) %>% 
      mean_distrib()
    
  } else if (myfit$best.fitting[1] == "beta") {
    
    
    distribution <- map_dfc(
      1:50, ~{ (mini + (maxi - mini)  *
          stats::qbeta((1:50/51), myfit$Beta$shape1, myfit$Beta$shape2)) %>% 
          sort()}) %>% 
      mean_distrib()
  } else if (myfit$best.fitting[1] == "mirrorgamma") {
    
    
    distribution <- map_dfc(
      1:50, ~{ 100 - RGeode::rgammatr(50, A = myfit$mirrorgamma$shape,
                                B = myfit$mirrorgamma$rate,
                                range = c(mini, maxi)) %>% 
          sort()}) %>% 
      mean_distrib()
    
  } else if (myfit$best.fitting[1] == "mirrorlognormal") {
    
    distribution <- map_dfc(
      1:50, ~{ 100 - EnvStats::rlnormTrunc(50, min = mini, max = maxi,
                                     meanlog = myfit$mirrorlognormal$mean.log.X,
                                     sdlog = myfit$mirrorlognormal$sd.log.X) %>% 
          sort()}) %>% 
      mean_distrib()
    
  }

  
  distribution
  
  
}


repet_unif <- function( mini,
                        maxi) {
  
  map_dfc(
    1:50, ~{ runif(50, mini, maxi) %>% 
        sort()}) %>% 
    mean_distrib()
}






mean_distrib <- function(tableau) {
  
 distribution <- tableau %>% 
    mutate(
      valeur = 1 :50
    ) %>% 
    pivot_longer(
      cols = starts_with("..."),
      names_to = "dist",
      values_to = "val" 
    ) %>% 
    group_by( valeur) %>% 
    summarise(
      distribution = mean(val)
    ) %>% 
    pull(distribution) 
  
  return(distribution)
  
}




#' @importFrom dplyr arrange
#' @importFrom flextable as_flextable


test_tabl <- function(nom_solde,
                      result,
                      seuil_mini,
                      seuil_att,
                      unite_euros,
                      unite_prix,
                      unite_prod
                      ){
  

  nb_result <- nrow(result)
  
  pc_mini <- ((result %>% 
                 filter(solde  < seuil_mini ) %>% 
                 nrow())*100/nb_result) %>% 
    round(.,digits = 1)
  
  
  pc_att <- ((result %>%
                filter(solde  > seuil_att) %>%
                nrow())*100/nb_result)%>% 
    round(.,digits = 1)
  
  
  tabl <- round(result, digits = 0) %>% 
    mutate(
      group_solde = case_when(
        solde < seuil_mini ~ paste(nom_solde, "< à", seuil_mini, unite_euros, sep = " " ),
        solde > seuil_att ~  paste(nom_solde, "> à", seuil_att, unite_euros, sep = " "),
        TRUE  ~ paste(nom_solde, "> à", seuil_mini, "et < à", seuil_att, unite_euros, sep = " ")
      ),
      pourcent = case_when(
        solde < seuil_mini ~ pc_mini,
        solde > seuil_att ~ pc_att,  
        TRUE ~  (100 - pc_mini - pc_att)
      )) %>% 
    group_by(group_solde)  %>% 
    mutate(
      mini_prod = min(production),
      maxi_prod = max(production),
      mini_prix = min(prix),
      maxi_prix = max(prix),
      mini_charges = min(charges),
      maxi_charges = max(charges)
    )  %>% 
    select(-c(1:4)) %>% 
    ungroup()  %>% 
    unique() %>% 
    arrange(group_solde) %>% 
    mutate(
      Production = paste("De", mini_prod, "à", maxi_prod, unite_prod, sep = " "),
      Prix = paste("De", mini_prix, "à", maxi_prix, unite_prix, sep = " "),
      Charges = paste("De", mini_charges, "à", maxi_charges, unite_euros, sep = " ")
    ) %>% 
    select(- starts_with("m")) %>% 
    t()  
  
  tabl <- data.frame(tabl)
  
  names(tabl) <- tabl[1,]
  
  tabl <- tabl[-c(1),]
  
  row.names(tabl) <- c("% des valeurs", "Production", "Prix", "Charges")  
  
  #essai flextable pour enregistrement
  
  as_flextable(tabl)
  
  return(tabl)
  
}

#' @importFrom dplyr n
#' @importFrom forcats as_factor

gener_graph <- function(nom_solde,
                        result,
                        seuil_mini,
                        seuil_att,
                        unite_euros,
                        unite_prix,
                        unite_prod
                        # ,
                        # production,
                        # prix, 
                        # charges
                        ) {
  
  result_mod <- result %>% 
    mutate(
      group_solde = case_when(
        solde < seuil_mini ~ "group1",
        solde < seuil_att & solde  > seuil_mini ~  "group2",
        TRUE  ~ "group3"
      ),
      group_solde = as_factor(group_solde)) 
  
  nb_result <- nrow(result) 
  
  result2 <- result_mod %>% 
    select(-solde  ) %>% 
    pivot_longer(
      1:3,
      names_to = "indicateur",
      values_to = "data"
    ) 
  
  
  result3 <- result_mod %>% 
    group_by(group_solde) %>% 
    summarise(
      pourcent =  round(n()*100/ nb_result))
  
  
  result2b <- result2 %>% 
    mutate(
      group_solde = case_when(
        group_solde %in% "group1" ~ paste0(nom_solde, " < ", seuil_mini," ", unite_euros, "\n (", result3[1,2], " % des valeurs)", sep = "" ),
        group_solde %in% "group2" ~ paste0(nom_solde, " > ", seuil_mini, " et < ", seuil_att," ", unite_euros, "\n (", result3[2,2], " % des valeurs)"),
        TRUE  ~ paste0( nom_solde, " > ", seuil_att," ", unite_euros,"\n (", result3[3,2], " % des valeurs)")
      )
    )
  
  
  
  result2b %>%
    ggplot( aes(x = group_solde, y=data, fill=group_solde)) +
    #geom_violin() +
    geom_boxplot(alpha=0.4) +
    scale_fill_manual(values=c("red", "orange", "green3")) +
    facet_wrap(facets = "indicateur", ncol =1, scales = "free_y") +
    labs(
      x = "Groupes selon les seuils choisis",
      y = "Répartition des données",
      subtitle = "Pour mieux comprendre les résultats",
      title = "Répartition des différentes variables par groupe",
      caption = "Vert = zone de confort \n Orange = zone de vigilance \n Rouge = zone critique"
    ) + 
    theme_light() +
    # Customizations
    theme(
      legend.position = "none", 
      strip.background = element_rect(fill = "#2a475e"),
      
      # This is the new default font in the plot
      plot.title = element_text(
        size = 14,
        face = "bold",
        color = "#2a475e"
      ),# Statistical annotations below the main title
      plot.subtitle = element_text(
        size = 13, 
        face = "bold",
        color="#1b2838"
      )
    ) 
  
  
  
}


