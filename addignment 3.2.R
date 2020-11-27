library(tidyverse)
library(vroom)
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "data-science/DS_101/00_data/patent.tsv",
  delim      = "\t",
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  assignee_id = col_character(),
  rawlocation_id = col_character(),
  type = col_double(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character(),
  sequence = col_double()
)

assignee_tbl <- vroom(
  file       = "data-science/DS_101/00_data/rawassignee.tsv",
  delim      = "\t",
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_select_tbl <- patent_tbl %>%
  select(number, date) %>%
  rename(
    patent_id = number
  )

assignee_select_tbl <- assignee_tbl %>%
  select(type, organization, patent_id)

patent_assignee_joined_tbl <- assignee_select_tbl %>%
  left_join(y = patent_select_tbl, by = c("patent_id" = "patent_id")) %>%
  select(type,date,organization,patent_id) %>%
  filter(type == 2) %>%
  separate(col  = date,
           into = c("year", "month", "date"),
           sep  = "-", remove = FALSE) %>%
  mutate(
    year  = as.numeric(year)
  ) %>%
  filter(year == 2019) %>%
  group_by(organization) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  slice(1:10)


write_rds(patent_assignee_joined_tbl,"assignment3-2.rds")
