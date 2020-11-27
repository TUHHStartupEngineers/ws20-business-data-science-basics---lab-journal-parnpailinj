library(tidyverse) 
library(lubridate)
library(ggplot2)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
  
  
covid_trend_tbl <- covid_data_tbl %>%
  select(1, contains("countries"),contains("continent"),contains("COVID")) %>% 
  set_names(c("date", "country", "continent", "cumulative_case")) %>% 
  mutate(cumulative_case = 100000*cumulative_case)  %>% 
  filter(country %in% c("Germany", "Spain",))





x <- 1:10
y1 <- 1:10
y2 <- 2:11
y3 <- 3:12
y4 <- 4:13
y5 <- 5:14
y6 <- 6:15
df <- data.frame(x, y1, y2, y3, y4, y5, y6)
ggplot(df, aes(x)) +
  geom_line(aes(y=y1),
            colour="red") +
  geom_line(aes(y=y2),
            colour="green") +
  geom_line(aes(y=y3),
            colour="blue") +
  geom_line(aes(y=y4),
            colour="yellow") +
  geom_line(aes(y=y5),
            colour="orange") +
  geom_line(aes(y=y6),
            colour="black")+
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M ")) +
  theme_dark() +
  labs(
    title    = "COVID-19 confirmed cases worldwide",
    subtitle = "as of 11/02/2020,Europe had more cases than the USA",
    x = "Year2020", # Override defaults for x and y
    y = "Cumulative Cases"
  ) 

