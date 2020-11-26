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



assignee_select_tbl <- assignee_tbl %>%
  select(id, type, organization) %>%
  rename(
    assignee_id = id
  )

patent_select_tbl <- patent_assignee_tbl %>%
  select(patent_id,assignee_id)


patent_assignee_joined_tbl <- assignee_select_tbl %>%
  left_join(y = patent_select_tbl, by = c("assignee_id" = "assignee_id")) %>%
  select(type,organization,patent_id) %>%
  filter(type == 2) %>%
  group_by(organization) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  slice(1:10)

result <- some() %>%
  pseudo() %>%
  data_wrangling() %>%
  operations()
write_rds(result,"assignment3-1.rds")

