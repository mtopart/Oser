# # ##### Debug
# library(tidyverse)
# library(plotly)
# 
# result <- crossing(
#   production = runif(30,20,30),
#   prix = runif(30,20,30),
#   charges = runif(30,20,30)
# ) %>%  mutate(
#   solde = production * prix - charges
# )
# 
# descript <- tibble(
#   moy = mean(result$solde),
#   mediane = median(result$solde),
#   nb_val = length(result$solde),
#   q1 = quantile(result$solde, 0.25),
#   q3 = quantile(result$solde, 0.75),
#   interv = q3 -q1
# )
# #
# #
# # graph_hist <- result %>%
# #   ggplot(aes(solde)) +
# #   geom_histogram(aes(y = after_stat(count / sum(count))),
# #                  binwidth = 200,
# #                  show.legend = FALSE,
# #                  alpha = 0.7 ) +
# #   scale_y_continuous(labels = scales::percent) +
# #   labs(
# #     title = "Titre",
# #     #subtitle = "Répartition de la marge ",
# #     x = "titre",
# #     y = "Fréquence",
# #     fill = ""
# #   ) +
# #   theme_bw() +
# #   theme(
# #     plot.title = element_text(size = 15L,
# #                               face = "bold"),
# #     plot.subtitle = element_text(size = 13L,
# #                                  face = "italic")
# #   )
# #
# # hist <- hist(result$solde, plot = FALSE)
# # xlim <- range(hist$breaks)
# # ylim  <- c(0, (ceiling(max(hist$density)*100)/10)-0.03)
# #
# #
# #
# # graph_hist <- graph_hist +
# #   aes(fill = "#XXXXX") +
# #   scale_fill_manual(values = c("#XXXXX" = "#77b5fe"))  +
# #   theme(
# #     legend.position = "none")
# #
# #
# #
# # w <- ggplotly(graph_hist) %>%
# #   layout(dragmode = "select") %>%
# #   config(
# #     modeBarButtonsToRemove = c('lasso2d',
# #                                'zoomIn2d',
# #                                'zoomOut2d',
# #                                'autoScale2d')
# #   )
# #
# #
# nb_result <- nrow(result)
# 
# seuil_mini <-  200
# 
# seuil_att <-  600
# 
# nom_solde <- "Marge"
# unite_euros <- "e"
# unite_prod <- "t"
# unite_prix <- "e/t"
# 
# 
# pc_mini <- ((result %>%
#                filter(solde  < seuil_mini ) %>%
#                nrow())*100/nb_result) %>%
#   round(.,digits = 1)
# 
# 
# pc_att <- ((result %>%
#               filter(solde  > seuil_att) %>%
#               nrow())*100/nb_result)%>%
#   round(.,digits = 1)
# 
# 
# tabl <- round(result, digits = 0) %>%
#   mutate(
#     group_solde = case_when(
#       solde < seuil_mini ~ paste(nom_solde, "< à", seuil_mini, unite_euros, sep = " " ),
#       solde > seuil_att ~  paste(nom_solde, "> à", seuil_att, unite_euros, sep = " "),
#       TRUE  ~ paste(nom_solde, "> à", seuil_mini, "et < à", seuil_att, unite_euros, sep = " ")
#     ),
#     pourcent = case_when(
#       solde < seuil_mini ~ pc_mini,
#       solde > seuil_att ~ pc_att,
#       TRUE ~  (100 - pc_mini - pc_att)
#     )) %>%
#   group_by(group_solde)  %>%
#   mutate(
#     mini_prod = min(production),
#     maxi_prod = max(production),
#     mini_prix = min(prix),
#     maxi_prix = max(prix),
#     mini_charges = min(charges),
#     maxi_charges = max(charges)
#   )  %>%
#   select(-c(1:4)) %>%
#   ungroup()  %>%
#   unique() %>%
#   arrange(group_solde) %>%
#   mutate(
#     Production = paste("De", mini_prod, "à", maxi_prod, unite_prod, sep = " "),
#     Prix = paste("De", mini_prix, "à", maxi_prix, unite_prix, sep = " "),
#     Charges = paste("De", mini_charges, "à", maxi_charges, unite_euros, sep = " ")
#   ) %>%
#   select(- starts_with("m"))
# 
# 
# # %>%
# #   t()
# #
# # tabl <- data.frame(tabl) %>%
# #   add_rownames()
# #
# # names(tabl) <- tabl[1,]
# #
# # tabl <- tabl[-c(1),]
# #
# # row.names(tabl) <- c("% des valeurs", "Production", "Prix", "Charges")
# #
#  library(flextable)
# 
# flextable(tabl)
# 
# dat2 <- result %>%
#   mutate(
#     group_solde = case_when(
#       solde < seuil_mini ~ paste(nom_solde, "< à", seuil_mini, unite_euros, sep = " " ),
#       solde > seuil_att ~  paste(nom_solde, "> à", seuil_att, unite_euros, sep = " "),
#       TRUE  ~ paste(nom_solde, "> à", seuil_mini, "et < à", seuil_att, unite_euros, sep = " ")
#     )) %>%
#   summarizor(., by = "group_solde")
# 
# # %>%
# #    as_flextable()
# 
# dat <- summarizor(tabl, by = "group_solde") %>%
#   as_flextable()
# dat
# 
# ft <- as_flextable(dat2)
# ft
# 
# flextable(dat3)
# 
# 
# dat3 <- dat2 %>%
#   filter(
#   stat != "missing",
#   stat != "mean_sd"
#   )
# 
# dat4 <- dat3 %>%
#   as_flextable(spread_first_col = TRUE, separate_with = "variable")
# 
# labelizor(x = dat4, j = "stat", labels = c(median_iqr  = "Médiane (EI)"))
# 
# 
# data   <- result %>%
#   summarise()
# 
# dat <- result %>%
#   mutate(
#     group_solde = case_when(
#       solde < seuil_mini ~ paste(nom_solde, "< à", seuil_mini, unite_euros, sep = " " ),
#       solde > seuil_att ~  paste(nom_solde, "> à", seuil_att, unite_euros, sep = " "),
#       TRUE  ~ paste(nom_solde, "> à", seuil_mini, "et < à", seuil_att, unite_euros, sep = " ")
#     )) %>%
#   pivot_longer(!c(solde, group_solde), names_to = "donnee", values_to = "data")
# 
# tabulator(
#   x = dat, rows = c("donnees"),
#   columns = "group_solde"
# )
# 
# 
# 
# ###---------------------------------------------------------------------
# #Pbq stats