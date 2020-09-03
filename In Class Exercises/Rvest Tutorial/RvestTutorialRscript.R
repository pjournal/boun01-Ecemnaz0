library(tidyverse)
library(rvest)

the_year <- 2020
the_month <-4

the_url <- "https://bkm.com.tr/secilen-aya-ait-sektorel-gelisim/?filter_year=2020&filter_month=1&List=Listele"
html_obj <- read_html(the_url) 

html_obj %>% html_structure()

html_df <- read_html(the_url) %>% html_table(fill=TRUE)


html_df %>% 
  # Since we do not have too many columns let's rename them manually 
  # number (num) or value (val) of transactions (txn) 
  # by credit card (cc) or debit card (dc)
  rename(category = 1, num_txn_cc = 2, num_txn_dc = 3, val_txn_cc = 4, val_txn_dc = 5) %>%
  # remove the first two rows because they are actually titles
  slice(-(1:2)) %>%
  # then convert every numeric value by using parse_number function from readr
  mutate(
    across(-category, 
           ~readr::parse_number(.,
                                locale=readr::locale(decimal_mark=",",grouping_mark = ".")
           )
    )
  )
