# Fonctions nécessaires pour faire tourner Oser

# Création du graphique d'entrée (élicitation)


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
       ylab="Fréquence",
       xaxp=c(mini[1], maxi, nBins), 
       main = paste("Total des jetons  :", sum(chips)),
       xlab = paste("Répartition ", name))
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



fitdisti <- function(mini,
                     maxi,
                     v,
                     p){
  
  req(maxi, mini, v, p)
  myfit <- fitdist(vals = v, probs = p, lower = mini,
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
    
    distribution <- EnvStats::rnormTrunc(40, min = mini, max = maxi,
                                         mean = myfit$Normal$mean,
                                         sd = myfit$Normal$sd)
    
  } else if (myfit$best.fitting[1] == "t" | myfit$best.fitting[1] == "logt") {
    
    
    distribution <- crch::rct(40, location = myfit$Student.t$location,
                              scale = myfit$Student.t$scale,
                              df = myfit$Student.t$df,
                              left = mini,
                              right = maxi
    )
    
  } else if (myfit$best.fitting[1] == "gamma") {
    
    distribution <- RGeode::rgammatr(40, A = myfit$Gamma$shape,
                                     B = myfit$Gamma$rate,
                                     range = c(mini, maxi)
    )
    
  } else if (myfit$best.fitting[1] == "lognormal") {
    
    distribution <- EnvStats::rlnormTrunc(40, min = mini, max = maxi,
                                          meanlog = myfit$Log.normal$mean.log.X,
                                          sdlog = myfit$Log.normal$sd.log.X)
    
  } else if (myfit$best.fitting[1] == "beta") {
    
    
    
    distribution <-  mini + (maxi - mini)  *
      stats::qbeta((1:40/41), myfit$Beta$shape1, myfit$Beta$shape2)
  }
  
  distribution
  
  
}

# select_distrib <- function(choixloi,
#                            unif,
#                            stat
#                            ) {
#   
#   
#   if (choixloi == TRUE) {
#     
#     distrib <- unif 
#   } else {
#     
#     distrib <- stat
#   }
#   
#   distrib %>% 
#     round(., digits = 1) %>% 
#     sort()
#   
#   
# }


