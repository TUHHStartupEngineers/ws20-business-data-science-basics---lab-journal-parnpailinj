library(tidyverse)
library(vroom)
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")

col_types <- list(
  id = col_character(),
  type = col_double(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "data-science/DS_101/00_data/assignee.tsv",
  delim      = "\t",
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "data-science/DS_101/00_data/patent_assignee.tsv",
  delim      = "\t",
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

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



assignee_select_tbl <- assignee_tbl %>%
  select(id, type, organization) %>%
  rename(
    assignee_id = id
  )

patent_assignee_select_tbl <- patent_assignee_tbl %>%
  select(patent_id,assignee_id)

patent_select_tbl <- patent_tbl %>%
  select(number, date) %>%
  rename(
    patent_id = number
  )


patent_assignee_joined_tbl <- assignee_select_tbl %>%
  left_join(y = patent_assignee_select_tbl, by = c("assignee_id" = "assignee_id")) %>%
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

patent_assignee_joined_tbl