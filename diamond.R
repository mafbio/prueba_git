#Libro R capítulo 5, Exploratory Data Analysis

library(tidyverse)
view(diamonds)

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

#Siempre hay que probar con diferentes anchuras pq pueden revelar diferentes patrones
smaller<-diamonds%>%
  filter(carat<3)
ggplot(data= smaller,mapping=aes(x=carat))+
  geom_histogram(binwidth=0.1)

# Si se quieren ver varios histogramas a la vez
ggplot(data=smaller, mapping=aes(x=carat, color=cut))+
  geom_freqpoly(binwidth=0.1)


# Ver unusual values
ggplot(diamonds)+
  geom_histogram(mapping=aes(x=y),binwidth=0.5)+
  coord_cartesian(ylim=c(0,50))
#Se pueden revisar, para ver si tienen sentido.
unusual<-diamonds%>%
  filter(y<3|y>20)%>%
  arrange(y)
unusual

#Exercises
#1) 
ggplot(diamonds)+
  geom_histogram(mapping=aes(x=x),binwidth=0.5)

ggplot(diamonds)+
  geom_histogram(mapping=aes(x=x),binwidth=0.5)+
  coord_cartesian(ylim=c(0,50))

ggplot(diamonds)+
  geom_histogram(mapping=aes(x=x),binwidth=0.5)+
  coord_cartesian(xlim=c(0,50))

ggplot(diamonds)+
  geom_histogram(mapping=aes(x=z),binwidth=0.5)


ggplot(diamonds)+
  geom_histogram(mapping=aes(x=z),binwidth=0.5)+
  coord_cartesian(ylim=c(0,50))


ggplot(diamonds)+
  geom_histogram(mapping=aes(x=z),binwidth=0.5)+
  coord_cartesian(xlim=c(0,50))

#2)
ggplot(diamonds)+
  geom_histogram(mapping=aes(x=price),binwidth=0.5)

ggplot(diamonds)+
  geom_histogram(mapping=aes(x=price),binwidth=0.5)+
  coord_cartesian(ylim=c(0,50))

ggplot(diamonds)+
  geom_histogram(mapping=aes(x=price),binwidth=10)+
  coord_cartesian(ylim=c(0,50))
