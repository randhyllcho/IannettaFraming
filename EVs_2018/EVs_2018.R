library(tidyverse)
library(rvest)
library(tidytext)


url <- "http://www.plugincars.com/cars"

page <- read_html(url)

titles <- html_nodes(page, "h3")
drive <- html_nodes(page, ".highlights")
type_detail <- html_nodes(page, ".type .field-content")
specs <- html_nodes(page, ".highlights strong")
detail_text <- html_nodes(page, "div .field-content p")

specs <- html_text(specs)
title <- html_text(titles)
drive_mode <- html_text(drive, trim = TRUE)

temp <- list()

for (i in seq(1,length(drive_mode),1)) {
  d = str_split(test, "[()]")[[i]][2]
  temp[[i]] = d
}

type <- html_text(type_detail)[seq(1,78, 2)]
class <- html_text(type_detail)[seq(2,78,2)]
range <- specs[seq(1,78,2)]
price <- specs[seq(2,78,2)]
power_mode <- unlist(temp)

ev_df <- tibble(title, power_mode, type, class, range, price)

ev_df <- ev_df %>% 
  mutate(range_miles = str_trim(range), 
         title = str_trim(title)) %>% 
  mutate_at(c("price", "range_miles"), parse_number) %>%
  mutate_at(c("type", "class", "power_mode"), as.factor) %>% 
  select(-range)

ev_df
write_csv(ev_df, "Evs_2018.csv")
