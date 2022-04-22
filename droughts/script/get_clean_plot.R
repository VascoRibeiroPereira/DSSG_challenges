# Libraries

library(dplyr)
library(readr)
library(ggplot2)

# Get Data

water_data_url <- "https://github.com/dssgPT/Plotting-Good-DSSG/blob/main/desafios/001_Seca_Em_Portugal_SNIRH/snirh_clean.csv?raw=true"
download.file(water_data_url, "droughts/scripts/files/water_data.csv")

# Explore water data

water_data <- read_csv("droughts/files/water_data.csv")

## Clean data and extract percentage
water_percent <- water_data[,-1] %>% 
  filter(medida == "percentagem") %>%
  mutate(nome_infraestrutura = toupper(nome_infraestrutura), 
         resumo_infraestrutura = as.numeric(resumo_infraestrutura)) %>% 
  na.omit()


## Find the Drainage basins with more variability in average of each year

varWater <- water_percent %>% 
  mutate(ano = year(data)) %>% 
  group_by(ano, nome_infraestrutura) %>% 
  summarise(media = mean(resumo_infraestrutura)) %>% 
  group_by(nome_infraestrutura) %>%
  summarise(variacao = var(media)) %>% 
  arrange(desc(variacao))

## Twelve worst cases name catch

mySearch <- head(varWater$nome_infraestrutura,12)

## Filter water_percent based on the mySearch Vector

water_subset <- water_percent %>% filter(nome_infraestrutura %in% mySearch) %>% 
  mutate(ano = year(data)) %>% 
  group_by(ano, nome_infraestrutura) %>% 
  summarise(media = mean(resumo_infraestrutura))


# Plot

average_plot <- water_subset %>% ggplot(aes(ano, media)) + 
  geom_point() + 
  stat_density2d(aes(color = ..level..)) + 
  facet_wrap(vars(nome_infraestrutura)) + 
  labs(title = "Bacias/Albufeiras com maior variabilidade percentual na quantidade de água armazenada entre 1999 e 2022",
       subtitle = "Destacam-se fortes tendencias de diminuição média de água nas albufeiras de Bravura, Minutos, Monte da Rocha e na bacia de Ribeiras do Algarve.",
       caption = "Fonte: SNIRH (dados tratados por Vasco Ribeiro Pereira)") + 
  xlab("Ano") + ylab("%Água") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(color = "grey50", face = "italic"),
        legend.position="none")



