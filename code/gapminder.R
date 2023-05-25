library(tidyverse)
library(ggplot2)

library(readr)
gapminder_1997 <- read_csv("data/gapminder_1997.csv")
View(gapminder_1997)

name <- "Emily"

#use a question mark in front of any function to see the help page for that function
?read_csv()

Sys.Date()
getwd()

sum(4, 6)

round(3.14154)
round(3.14149, 3)

### GRAPHING PRACTICE ###

#making a scatter plot with gapminder data, geom_point() 
#assigning x and y variables in aes, assigning labels in labs()
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp) + 
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do wealthy countries live longer?") +
  geom_point()

#in aes, assign the color of the points to be categorized by continent variable, 
#and the size of the points be sized by population variable. 
#also use scale_color_brewer(palette = ) to choose a color palette to use for the assigned colors
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop) + 
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do wealthy countries live longer?") +
  geom_point() +
  scale_color_brewer(palette = "Set1")

#Editing the point size (population) to scale by millions
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) + 
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do wealthy countries live longer?", size = "population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set1")

#Different continents points are different shapes, in aes
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000, 
      shape = continent) + 
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do wealthy countries live longer?", size = "population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set1")

#adding labels next to the points, label each point with the country name 
#size the text as 2, and adjust it to the side so it is not covering the point
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) + 
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do wealthy countries live longer?", size = "population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  geom_text(aes(label = country), size = 2, hjust = 1.1)


### DATA EXPLORATION ###
library(readr)
gapminder_data <- read_csv("gapminder_data.csv")

#view dataframe
view(gapminder_data)

# 6x6 tibble - first six rows/columns of data
head(gapminder_data)

# dimensions of the dataframe
dim(gapminder_data)

#group lines by country, force each country to be one line rather than grouping by continent
ggplot(data = gapminder_data) + 
  aes(x = year, y = lifeExp, color = continent, group = country) +
  scale_color_brewer(palette = "Set1") +
  geom_line()

#make a boxplot using categorical x variable and continuous y variable
ggplot(data = gapminder_data) + 
  aes(x = continent, y = lifeExp) +
  geom_boxplot()

#make a violin plot using categorical x variable and continuous y variable
ggplot(data = gapminder_data) + 
  aes(x = continent, y = lifeExp) +
  geom_violin()

#layering plots - make sure what you want on top is the last geom
ggplot(data = gapminder_data) + 
  aes(x = continent, y = lifeExp) +
  geom_violin() +
  geom_point(aes(), size = 0.5)

#layering plots - use geom_jitter() for easier visualization 
#(still same y value, just spread out on x axis)
#putting aesthetics into geoms
ggplot(data = gapminder_data) + 
  aes(x = continent, y = lifeExp) +
  geom_violin() +
  geom_jitter(aes(size = pop/1000000)) +
  labs(size = "population by millions")

#mapping, put mapping = aes in the ggplot() function instead of its own thing for the global aesthetics
ggplot(data = gapminder_data, 
       mapping = aes(x = continent, 
                     y = lifeExp)) + 
  geom_violin(aes(fill = continent)) +
  geom_jitter(aes(), size = 0.5)

#adding color of violin plots, assigning a color
ggplot(data = gapminder_data, 
       mapping = aes(x = continent, 
                     y = lifeExp)) + 
  geom_violin(color = "purple", fill = "pink") +
  geom_jitter(aes(), size = 0.5)

#adding aesthetics to violin plots, assigning a color by another variable
my_plot <- ggplot(data = gapminder_data, 
       mapping = aes(x = continent, 
                     y = lifeExp)) + 
  labs(x = "Continent", y = "Life Expectency", 
       title = "Life Exp by Continent") +
  geom_violin(aes(fill = continent, color = continent))

#save most recent graph in plot viewer, will save in directory
ggsave("data_carp_plot.jpg", plot = my_plot, width = 6, height = 4)





