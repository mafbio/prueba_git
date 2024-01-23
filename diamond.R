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

#3)
diamonds %>%
  filter(carat >= 0.99, carat <= 1) %>%
  count(carat)
#4)
ggplot(diamonds)+
geom_histogram(mapping=aes(x=price))+
  coord_cartesian(ylim=c(0,50))

#Missing Values
diamonds2<-diamonds%>%
  filter(between(y,3,20))
view(diamonds2)

diamonds2<-diamonds%>%
  mutate(y=ifelse(y<3|y>20, NA,y))

  #Con ggplot
ggplot(data=diamonds2, mapping=aes(x=x,y=y))+
  geom_point()
ggplot(data=diamonds2, mapping=aes(x=x,y=y))+
  geom_point(na.rm=TRUE)

library(nycflights13)
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour=sched_dep_time %/% 100,
    sched_min=sched_dep_time %% 100,
    sched_dep_time=sched_hour+sched_min/60
  )%>%
  ggplot(mapping=aes(sched_dep_time))+
  geom_freqpoly(
    mapping=aes(color=cancelled),
    binwidth=1/4
  )
#Exercises

#1 
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = y)) +
  geom_histogram()

diamonds %>%
  mutate(cut = if_else(runif(n()) < 0.1, NA_character_, as.character(cut))) %>%
  ggplot() +
  geom_bar(mapping = aes(x = cut))  
#2
mean(c(0, 1, 2, NA), na.rm = TRUE)

sum(c(0, 1, 2, NA), na.rm = TRUE)

mean(c(1,2,0,0)) 
#removes NA



#Covariation
# A categorical and continuous variable
ggplot(data=diamonds, mapping=aes(x=price))+
  geom_freqpoly(mapping=aes(color=cut), binwidth=500)
ggplot(diamonds)+geom_bar(mapping=aes(x=cut))

ggplot(
  data=diamonds, 
  mapping=aes(x=price,y=..density..)
)+
  geom_freqpoly(mapping=aes(color=cut), binwidth=500)
ggplot(data=diamonds, mapping = aes(x=cut,y=price))+
  geom_boxplot()
ggplot(data=mpg, mapping=aes(x=class, y=hwy))+
  geom_boxplot()
ggplot(data=mpg)+
  geom_boxplot(
    mapping=aes(
      x=reorder(class, hwy, FUN=median),
      y=hwy
    )
  )
ggplot(data=mpg)+
  geom_boxplot(
    mapping = aes(
      x=reorder(class, hwy, FUN= median),
      y=hwy
      )
    )+
      coord_flip()

#Exercises
#1
view(flights)
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot() +
  geom_boxplot(mapping = aes(y = sched_dep_time, x = cancelled))

#2
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), orientation = "x")
diamonds %>%
  mutate(color = fct_rev(color)) %>%
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = clarity, y = price))
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()
#3
library(ggstance)
ggplot(data=mpg)+
  geom_boxplot(
    mapping = aes(
      x=reorder(class, hwy, FUN= median),
      y=hwy
    ))
ggplot(data = mpg) +
  geom_boxploth(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy), orientation = "y")
#4
ggplot(data=diamonds, mapping = aes(x=cut,y=price))+
  geom_boxplot()
library(lvplot)

ggplot(data=diamonds, mapping = aes(x=cut,y=price))+
  geom_lv()
#5
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 1, scales = "free_y")
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin() +
  coord_flip()
#6

#Two categorcal variables
ggplot(data=diamonds)+
  geom_count(mapping=aes(x=cut,y=color))
library(dplyr)
diamonds%>%
  count(color,cut)
diamonds%>%
  count(color,cut)%>%
  ggplot(mapping=aes(x=color, y=cut))+
  geom_tile(mapping=aes(fill=n))
#Exercises 
#1

diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))

diamonds %>%
  count(color, cut) %>%
  group_by(cut) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))
#2
flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")
flights %>%
  group_by(month, dest) %>%                                 # This gives us (month, dest) pairs
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%                                        # group all (month, dest) pairs by dest ..
  filter(n() == 12) %>%                                     # and only select those that have one entry per month 
  ungroup() %>%
  mutate(dest = reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")
#3
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(y = color, x = cut)) +
  geom_tile(mapping = aes(fill = n))

