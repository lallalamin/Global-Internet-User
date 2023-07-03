library(ggplot2)
library(tidyverse)
library(plotly)
library(ggrepel)
library(dplyr)
library(socviz)
library(gapminder)

#setting up the dataset
internet <- read.csv("internet.csv")
country <- read.csv("world_population.csv")

internet <- internet %>% rename(country = Entity)
country <- country %>%
  mutate(country = Country.Territory) %>%
  select(country, Continent) %>%
  rename(continent = Continent)

internet_cont <- inner_join(internet, country, by = "country") %>% distinct()

#first graph

world_data <- internet %>% 
  select(country, Year, No..of.Internet.Users) %>% 
  filter(country == "World")

world <- ggplot(data = world_data, mapping = aes(x = Year, y = No..of.Internet.Users))
#how to use interval in this case
#pred_num <- predict(object = No..of.Internet.Users,newdata = world_data,interval = "confidence")
#subtitle is not showing
world1 <- world + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(breaks = c(0, 1000000000, 2000000000, 3000000000, 4000000000), labels = c("0", "1B", "2B", "3B", "4B")) +
  labs(y = "Internet User", title = "Global Internet User were growing faster in earlier year", subtitle = "1980-2020") +
  annotate(geom = "rect", xmin = 2016, xmax = 2020, ymin = 3500000000, ymax = 4700000000, fill = "red", alpha = 0.2) +
  annotate(geom = "text", x = 2016, y = 4200000000, label = "Rapidly growing", size = 3)

world1

#second graph
int_2020 <- internet_cont %>% filter(Year == 2020, Internet.Users... > 0.0000)

internet1_2020 <- ggplot(data = int_2020, mapping = aes(x = Internet.Users...,y = Cellular.Subscription,label = factor(country))) 

internet2_2020 <- internet1_2020 + geom_smooth(method = "lm", se = FALSE, color = "gray80") +
  geom_point(alpha = 0.2) +
  geom_point(data = subset(int_2020, Cellular.Subscription > 162.8000), mapping = aes(x = Internet.Users...,y = Cellular.Subscription, color = continent)) +
  guides(color = guide_legend(title = "Continent")) +
  theme(legend.position = "bottom")

internet3_2020 <- internet2_2020 + labs(x = "Internet Users", y = "Cellular Subscription per 100 people", title = "Top 7 Country that has the Most Access to Internet in 2020")

internet_2020 <- internet3_2020 + geom_text_repel(data = subset(internet_cont, Year == 2020 & Cellular.Subscription > 162.8000 & Internet.Users... > 0.0000), size = 2, aes( label = country)) +
  annotate(geom = "text", x = 0, y = 290, label = "This number can get over 100 when the average person has \n more than one subscription to a mobile service.", hjust = 0, size = 2.5) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), labels = c("0%" , "25%", "50%", "75%", "100%"))

internet_2020

#third graph
no_asiaandna <- internet_cont %>% filter(!continent %in% c("Asia", "North America")) 
asia <- internet_cont %>% filter(continent %in% "Asia" & Year > 2000 & No..of.Internet.Users > 0)
na <- internet_cont %>% filter(continent %in% "North America")

overall <- ggplot(data = internet_cont, mapping = aes(x = Year,y = No..of.Internet.Users, group = country, color = country))

overall <- overall + geom_line() + 
  guides(color = FALSE) +
  facet_wrap(~continent, ncol = 2) +
  scale_y_continuous(breaks = c(200000000, 400000000, 600000000, 800000000, 1000000000), labels = c("200M" , "400M", "600M", "800M", "1B")) +
  labs(x = "Year", y = "Internet Users", title = "Internet User in the Past 20 Years by Continent")

ggplotly(overall)



p <- ggplot(data = subset(no_asiaandna, Year > 2000 & No..of.Internet.Users > 0), mapping = aes(x = Year,y = No..of.Internet.Users, group = country, color = country))

four_cont <- p + geom_line() + 
  facet_wrap(~continent, ncol = 2) + 
  guides(color = FALSE) + 
  scale_y_continuous(breaks = c(25000000, 50000000, 75000000, 100000000, 150000000), labels = c("25M" , "50M", "75M", "100M", "150M")) +
  labs(x = "Year", y = "Internet Users", title = "Internet User in the Past 20 Years of the Four Continenet")

ggplotly(four_cont)


asia_g <- ggplot(data = subset(asia, Year > 2000 & No..of.Internet.Users > 0), mapping = aes(x = Year,y = No..of.Internet.Users, group = country, color = country))

asia_g <- asia_g + geom_line() + 
  guides(color = FALSE) +
  facet_wrap(~continent, ncol = 1) +
  scale_y_continuous(breaks = c(200000000, 400000000, 600000000, 800000000, 1000000000), labels = c("200M" , "400M", "600M", "800M", "1B")) +
  labs(x = "Year", y = "Internet Users", title = "Internet User in Asia in the Past 20 Years")

ggplotly(asia_g)


na_g <- ggplot(data = subset(na, Year > 2000 & No..of.Internet.Users > 0), mapping = aes(x = Year,y = No..of.Internet.Users, group = country, color = country))

na_g <- na_g + geom_line() + 
  guides(color = FALSE) +
  facet_wrap(~continent, ncol = 1) +
  scale_y_continuous(breaks = c(100000000, 200000000, 300000000), labels = c("100M" , "200M", "300M")) +
  labs(x = "Year", y = "Internet Users", title = "Internet User in North America in the Past 20 Years")

ggplotly(na_g)

#fourth graph
# create data for world coordinates using 
# map_data() function
world_coordinates <- map_data("world")
world_coordinates <- world_coordinates %>% rename(country = region)

#head(internet_cont)
#, plot.title = "# of Internet User in 2020"
#guides(color = guide_legend(title = "No of Internet User 2020"))


internet_cont["country"][internet_cont["country"] == "United States"] <- "USA"

internet_world <- left_join(world_coordinates, internet_cont)
internet_world <- internet_world %>% filter(Year == 2020)

p <- ggplot(subset(internet_world, Year = 2020), aes(x = long, y = lat, group = group, fill = No..of.Internet.Users))
p1 <- p + geom_polygon(color = "gray90", linewidth = 0.1) + 
  scale_fill_gradient(low = "#2E74C0", high = "#CB454A", labels=c("0M", "250M", "500M", "750M", "1B")) +
  labs(title = "Global Internet User in 2020")
ggplotly(p1)

