#Libro R capítulo 5, Exploratory Data Analysis

library(tidyverse)

#Visualizando distribuciones
  # Variable categórica
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut))
#Hi, people
diamonds %>%
  count(cut)
  #Variable continua
ggplot(data=diamonds)+
  geom_histogram(mapping = aes(x=carat), binwidth=0.5)
diamonds %>%
  count(cut_width(carat,0.5))
