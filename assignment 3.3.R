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
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_double()
)

uspc_tbl <- vroom(
  file       = "data-science/DS_101/00_data/uspc.tsv",
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


uspc_select_tbl <- uspc_tbl %>%
  select(patent_id, mainclass_id) 


patent_assignee_joined_tbl <- assignee_select_tbl %>%
  left_join(y = patent_assignee_select_tbl, by = c("assignee_id" = "assignee_id")) %>%
  select(type,organization,patent_id) %>%
  group_by(organization) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  slice(1:10)
patent_assignee_joined_tbl
# get the list of top10 result and put to filter below

top_mainclass_tbl <- assignee_select_tbl %>%
  left_join(y = patent_assignee_select_tbl, by = c("assignee_id" = "assignee_id")) %>%
  left_join(y = uspc_select_tbl, by = c("patent_id" = "patent_id")) %>%
  select(organization,mainclass_id) %>%
  filter(organization %in% c("International Business Machines Corporation", "Canon Kabushiki Kaisha","Samsung Electronics Co., Ltd.","General Electric Company","Kabushiki Kaisha Toshiba","Sony Corporation","Hitachi, Ltd.","Intel Corporation","Fujitsu Limited","NEC Corporation")) %>%
  group_by(mainclass_id) %>%
  summarise(
    count = n()
  ) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  slice(1:6) #row 1 is NA

top_mainclass_tbl

write_rds(top_mainclass_tbl,"assignment3-3.rds")