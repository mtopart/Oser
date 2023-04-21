library(tidyverse)
library(flextable)

result <- crossing(
  production = runif(30,20,30),
  prix = runif(30,20,30),
  charges = runif(30,20,30)
) %>%  mutate(
  solde = production * prix - charges
)

seuil_mini <-  200
seuil_att <-  600
nom_solde <- "Marge"
unite_euros <- "e"
unite_prod <- "t"
unite_prix <- "e/t"
nb_result <- nrow(result)

tabl2 <- round(result, digits = 0) %>%
  mutate(
    group_solde = case_when(
      solde < seuil_mini ~ paste(nom_solde, "< à", seuil_mini, unite_euros, sep = " " ),
      solde > seuil_att ~  paste(nom_solde, "> à", seuil_att, unite_euros, sep = " "),
      TRUE  ~ paste(nom_solde, "> à", seuil_mini, "et < à", seuil_att, unite_euros, sep = " ")
    )) %>%
  pivot_longer(
      1:3,
      names_to = "indicateur",
      values_to = "data"
    )%>%
  group_by(group_solde, indicateur) %>%
  summarise(
    across(
      .cols = "data",
      .fns = list(
        Med = ~ median(.x),
        Mini = ~ min(.x),
        Maxi = ~ max(.x),
        pourcent = ~ round(length(.x)*100/nb_result)
      )),
      .groups = "drop"
    ) %>%
  ungroup() %>%
  mutate(
    Range = paste(data_Mini, data_Maxi, sep = " - ")
  ) %>% 
  mutate(
    indicateur = str_to_title( indicateur)
  )

 

tabl2b <- as_grouped_data(
  x = tabl2,
  groups = c("indicateur", "group_solde")
)  %>%
  as_flextable()



tabl3 <- tabl2 %>%
  select(-data_Mini, -data_Maxi) %>%
  mutate(
    across(
      .cols = everything(),
      .fns = ~as.character(.))) %>%
  pivot_longer(
    cols =  3:5
  )

tabl4 <- tabl3 %>%
  filter(
    name != "data_pourcent"
  )

tabulator(
  x= tabl4,
  rows = c("group_solde", "name"),
  columns = "indicateur",
  `essai`= as_paragraph(as_chunk(value))) %>%
    as_flextable(
      spread_first_col = TRUE, separate_with = "group_solde"
                 )  %>%
  bold(i = ~ !is.na(group_solde), j = 1, bold = TRUE) 


tabulator(
  x= tabl4,
  rows = c("indicateur", "name"),
  columns = "group_solde",
  `essai`= as_paragraph(as_chunk(value))) %>%
  as_flextable(
    spread_first_col = TRUE, separate_with = "indicateur"
  )  %>%
  bold(i = ~ !is.na(indicateur), j = 1, bold = TRUE)

#
# %>%
#   labelizor(
#     part = "header",
#     labels = stringr::str_to_title
#   )
#
#

essai <- tabl3 %>%
  select(- indicateur) %>%
  filter(name == "data_pourcent") %>%
  unique() %>%
  pivot_wider()

essai2 <- essai[[2]]

donnees <- tibble (
  production = runif(4,20,30),
  prix = runif(4,20,30),
  charges = runif(4,20,30)
)  %>%
  pivot_longer(
  everything(),
  names_to = "nom",
  values_to = "data"
) %>%
  group_by(nom) %>%
  mutate(
    num = 1:4
  )


data <- round(result, digits = 0) %>%
  mutate(
    group_solde = case_when(
      solde < seuil_mini ~ paste(nom_solde, "< à", seuil_mini, unite_euros, sep = " " ),
      solde > seuil_att ~  paste(nom_solde, "> à", seuil_att, unite_euros, sep = " "),
      TRUE  ~ paste(nom_solde, "> à", seuil_mini, "et < à", seuil_att, unite_euros, sep = " ")
    )) %>%
  pivot_longer(
    1:3,
    names_to = "indicateur",
    values_to = "data"
  )

ggplot(data, aes(x=group_solde, y=data, fill=indicateur)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()

