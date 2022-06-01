# production <- runif(50, 80, 100)
# prix <- runif(50, 160, 210)
# charges <- runif(50, 8000, 10000)
# 
# result <-crossing(
#   production,
#   prix,
#   charges
# )  %>%
#   # mutate(
#   #   production = round(production, digits = 0),
#   #   prix = round(prix, digits = 0),
#   #   charges = round(charges, digits = 0)
#   # ) %>%
#   # group_by(production, prix, charges) %>%
#   # unique() %>%
#   mutate(ca = production * prix,
#          solde = ca - charges)
# #%>%
#   # filter(solde < 500) %>%
#   # filter(solde > 300)
# 
#  result %>%
#   summary()
# 
#  #
#  # library(openxlsx
#  #         )
#  # addWorksheet(wb, "Summary")
#  # writeData(
#  #   wb,
#  #   2,
#  #   result %>%
#  #     summary())
#  #
#  # wb %>%
#  #   openXL()
#  #
# 
# 
# 
#  library(ggiraph)
#  library(ggplot2)
# 
# 
# test <- result %>%
#    ggplot(aes(solde )) +
#    geom_histogram_interactive( aes(tooltip = solde))
# 
# #+
#    # labs(title = "Représentation graphique de la répartition du solde choisi (fréquence du solde choisi dans différents intervalles)",
#    #      x = "Solde choisi (marge, EBE, revenu, ...) (en € ou k€)",
#    #      y ="Nombre des soldes calculés \n (marge, EBE, revenu, ..) \n par intervalle ",
#    #      fill = ""
#    # )+
#    # theme(legend.position="bottom")
#    #
# 
#  girafe(code = print(test))
