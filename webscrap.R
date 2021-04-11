# setwd()

# Load required packages 
library(tidyverse)
library(rvest)
library(tidyquant)
library(ggthemes)

## Website 
tiobe <- "https://www.tiobe.com/tiobe-index/"

## Scrape the data ----
data <- read_html(tiobe)

## capture nodes ----
data_nodes <- data %>% 
    html_nodes("table")

## get tables---
data_tables <- 
  html_table(data_nodes)

## Extract data ----
data_tables[[1]]
data_tables[[2]]
data_tables[[3]]
data_tables[[4]]

## Population ----
url2 <- "https://en.wikipedia.org/wiki/World_population"
pop <- read_html(url2)
pop_nodes <- html_nodes(pop, "table")
pop_tables <- html_table(pop_nodes, fill = TRUE)
pop_tables[[15]]

## Regional population -----
## Make data tidy 
regional_pop <- pop_tables[[15]]
regional_pop <- pivot_longer(regional_pop, cols = -1, 
                        names_to = "Regions")

## Rename column 
names(regional_pop)[names(regional_pop) == "value"] <- "Pop_Millions"

## replace comas and full stops with nothing ----
regional_pop$Pop_Millions <- 
  str_replace_all(regional_pop$Pop_Millions, ",", "")

regional_pop$Pop_Millions <- 
  str_replace_all(regional_pop$Pop_Millions, " ", "")

## separate rate and pop ----
regional_pop <- separate(regional_pop, Pop_Millions, 
         into = c("Pop_Millions", "Growth"), sep = "\\(")

## remove % and ) in Growth column ----
regional_pop$Growth <- str_replace_all(regional_pop$Growth, "%", "")
regional_pop$Growth <- str_replace_all(regional_pop$Growth, "\\)", "")
regional_pop

## Convert data types ----
regional_pop$Regions <- as.factor(regional_pop$Regions)
regional_pop$Pop_Millions <- as.numeric(regional_pop$Pop_Millions)
regional_pop$Growth <- as.character(regional_pop$Growth)
regional_pop$Growth <- as.numeric(regional_pop$Growth)

### Visualize data ----
regional_pop %>% ggplot(aes(x = Year, y = Pop_Millions, 
                            color = Regions, text = Regions)) + 
                             geom_line() + theme_bw()

regional_pop %>%filter(Regions != "World") %>% 
  ggplot(aes(x = Year, y = Growth, 
  color = Regions, text = Regions)) + 
  geom_line(size = 1.5) + 
  geom_vline(xintercept = 2020, 
  color = "black", lty = "dotted") + 
  labs( x = "year", y = "% of World Population", 
  title = "Regional Prop of World Population (2000 - 2050)") +
  theme_wsj(color = "grey", base_size = 7)


## Beer ratings 
url3 <- "https://www.beeradvocate.com/beer/top-rated/"
my_beers <- read_html(url3) %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  .[[1]]

my_beers
