##### Debug
library(tidyverse)
library(plotly)

result <- crossing(
  production = runif(30,20,30),
  prix = runif(30,20,30),
  charges = runif(30,20,30)
) %>%  mutate(
  solde = production * prix - charges
)

descript <- tibble(
  moy = mean(result$solde),
  mediane = median(result$solde),
  nb_val = length(result$solde),
  q1 = quantile(result$solde, 0.25),
  q3 = quantile(result$solde, 0.75),
  interv = q3 -q1
)


graph_hist <- result %>%
  ggplot(aes(solde)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 binwidth = 200,
                 show.legend = FALSE,
                 alpha = 0.7 ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Titre",
    #subtitle = "Répartition de la marge ",
    x = "titre",
    y = "Fréquence",
    fill = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold"),
    plot.subtitle = element_text(size = 13L,
                                 face = "italic")
  )

hist <- hist(result$solde, plot = FALSE)
xlim <- range(hist$breaks)
ylim  <- c(0, (ceiling(max(hist$density)*100)/10)-0.03)    



graph_hist <- graph_hist +
  aes(fill = "#XXXXX") +
  scale_fill_manual(values = c("#XXXXX" = "#77b5fe"))  +
  theme(
    legend.position = "none")



w <- ggplotly(graph_hist) %>%    
  layout(dragmode = "select") %>%   
  config(
    modeBarButtonsToRemove = c('lasso2d',
                               'zoomIn2d',
                               'zoomOut2d',
                               'autoScale2d')
  )



