library(tidyverse)
library(rvest)

the_year <- 2020
the_month <-4

the_url <- "https://bkm.com.tr/secilen-aya-ait-sektorel-gelisim/?filter_year=2020&filter_month=1&List=Listele"
html_obj <- read_html(the_url) 

html_obj %>% html_structure()

table_list <- html_obj %>% html_table(fill=TRUE)
data_df <- as_tibble(table_list[[4]])
data_df %>% 
  rename(category=1,cc_txn=2,dc_txn=3,cc_val=4,,dc_val=5) %>% 
  slice(-(1:2)) %>% 
  mutate(
    across(
      -category,
      ~readr::parse_number(.,locale=locale(decimal_mark = ",",grouping_mark = "."))
    ),
    year=the_year,
    month=the_month
  )