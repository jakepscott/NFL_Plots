# Get Libraries and Fonts-----------------------------------------------------------
library(listviewer)
library(tidyverse)
library(ggimage)
library(jsonlite)
library(scales)
library(here)
theme_set(theme_minimal(base_size = 12,base_family = "Roboto Condensed"))
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

# Get Data ----------------------------------------------------------------
#Extract the json
raw_538_json <- fromJSON("https://projects.fivethirtyeight.com/2020-nfl-predictions/data.json", simplifyVector = FALSE)

#Extract elo forecasts
weekly_raw <- raw_538_json$weekly_forecasts$forecasts %>% 
  enframe() %>% 
  unnest_wider(value) %>% 
  unnest_wider(types) %>% 
  select(-name) %>%
  unchop(cols = c(elo, rating))

weekly_elo <- weekly_raw %>% 
  select(-rating) %>% 
  unnest_wider(elo) %>% 
  mutate(measure = "ELO", .after = last_updated,
         name=ifelse(name=="OAK","LV",name))

team_meta <- espnscrapeR::get_nfl_teams() %>% 
  select(abb_name = team_short_name, team_color,logo)

data <- weekly_elo %>% filter(division=="NFC East" & measure=="ELO") %>% select(week,name,make_playoffs,current_wins,current_losses,current_ties) 


data <- data %>% left_join(team_meta,by=c("name"="abb_name")) %>% mutate(logo_fixed=case_when(week==16~logo))
asp_ratio <- 1.68

data %>%
  ggplot(aes(x=week,y=make_playoffs)) +
  geom_line(aes(color=team_color),lwd=2) +
  geom_image(aes(image = logo_fixed), size = 0.035, by = "width", asp = asp_ratio) +
  scale_color_identity()

data %>%
  ggplot(aes(x=week,y=make_playoffs)) +
  geom_line(aes(color=team_color),lwd=2) +
  geom_image(aes(image = logo_fixed), size = 0.035, by = "width", asp = asp_ratio) +
  scale_color_identity()

data %>% distinct(team_color)
