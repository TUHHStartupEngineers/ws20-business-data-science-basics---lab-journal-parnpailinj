library(tidyverse) 
library(lubridate)
library(ggplot2)
library(maps)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
  
  
covid_trend_tbl <- covid_data_tbl %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  ))

deathrate_tbl <- covid_trend_tbl %>%  
  select(1,contains("countries"),6,10) %>% 
  set_names(c("date","countries","deaths","population")) %>% 
  mutate(date = as.Date(date, "%d/%m/%Y")) %>%
  arrange(date) %>%
  slice(1:52963) %>%
  group_by(countries) %>%
  mutate(deathrate = cumsum(deaths/population)) %>%
  summarize(Mortality_Rate  = max(deathrate)*100)%>%
  ungroup()  

world <- map_data("world") %>%
  rename(countries= region)

covid_map <- left_join(world, deathrate_tbl, by = "countries")

ggplot(covid_map,aes(long,lat,group =group)) +
  geom_polygon(aes(fill = Mortality_Rate), color = "white")+
  scale_fill_gradient(low="#FF3333", high="#330000")+
  labs(
    title    = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide",
    x = "Date: 11/02/2020", # Override defaults for x and y
    y = ""
  )
