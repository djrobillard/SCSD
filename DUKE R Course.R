library(dplyr)
library(ggplot2)
library(readr)

##LESSON 1*

gapminder = read_csv("https://stat.duke.edu/~mc301/data/gapminder.csv")

gap07 <- gapminder %>%
  filter(year==2007)

ggplot(data=gap07,aes(x=gdpPercap,y=lifeExp,color=continent))+geom_point()

gap52 <- gapminder %>%
  filter(year==1952)

ggplot(data=gap52,aes(x=lifeExp,y=pop,size=gdpPercap,color=continent))+geom_point()

##Lesson 2##

glimpse(gapminder)
glimpse(starwars)

ggplot(data=starwars,aes(x=height,y=mass))+geom_point()

ggplot(data=starwars,aes(x=height,y=mass,color=gender))+geom_point()

ggplot(data=starwars,aes(x=height,y=mass))+facet_grid(.~gender)+geom_point()

ggplot(data=starwars,aes(x=height,y=mass))+facet_grid(gender~.)+geom_point()

ggplot(data=starwars,aes(x=height,y=mass))+facet_wrap(~eye_color)+geom_point()
