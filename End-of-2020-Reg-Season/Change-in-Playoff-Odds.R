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

data <- weekly_elo %>% select(week,name,conference, division,make_playoffs,current_wins,current_losses,current_ties) 

team_meta <- espnscrapeR::get_nfl_teams() 

data <- data %>% left_join(team_meta,by=c("name"="team_short_name")) 


# Full Season Changes by Division -----------------------------------------
data %>% 
  arrange(name,week) %>% 
  group_by(name) %>% 
  mutate(change=make_playoffs-lag(make_playoffs)) %>% 
  ungroup() %>% 
  filter(week>=10) %>% 
  group_by(division) %>% 
  mutate(mean_odds_change=mean(abs(change),na.rm=T)) %>% 
  ungroup() %>% 
  distinct(division,.keep_all = T) %>% 
  ggplot(aes(x=fct_reorder(division,mean_odds_change),y=mean_odds_change)) +
  geom_col(aes(fill=conference)) + 
  scale_fill_manual(values=c("#D50A0A","#013368")) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  coord_flip() +
  labs(title="The NFC Has Seen Wild Swings in Playoff \nOdds Since Week 10",
       subtitle = "Mean change in playoff odds for each team by division, as calculated by ELO",
       x="",
       y="Mean change in playoff odds week to week",
       caption = "Plot: @jakepscott2020 | Data: 538, Using @thomas_mock's Method") +
  theme(plot.title = element_text(size = rel(1.7)),
        plot.subtitle = element_text(size=rel(1),color="grey40", face = "italic"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        plot.title.position = "plot",
        axis.text.y = element_text(size=rel(1.3),face="bold"),
        legend.position = "none"
        )
ggsave(here("End-of-2020-Reg-Season/plots/Division_Odds_Changes.png"),dpi=600)


# All teams  -----------------------------------------------------
asp_ratio <- 1.5

data %>% 
  arrange(name,week) %>% 
  group_by(name) %>% 
  mutate(change=make_playoffs-lag(make_playoffs)) %>% 
  ungroup() %>% 
  filter(week>=10) %>% 
  group_by(name) %>% 
  mutate(mean_odds_change=mean(abs(change),na.rm=T)) %>% 
  ungroup() %>% 
  distinct(name,.keep_all = T) %>% 
  ggplot(aes(x=fct_reorder(name,mean_odds_change),y=mean_odds_change)) +
  geom_col(aes(fill=team_color,color=alternate_color)) + 
  geom_image(aes(image = logo), size = 0.075, by = "width", asp = asp_ratio) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip() +
  facet_wrap(~conference,scales = "free_y") +
  labs(title="Colts and Bears Fans Have Had an Intense \nSecond Half of the Season",
       subtitle = "Mean change in playoff odds for each team by division after week 10, as calculated by ELO",
       x="",
       y="Mean change in playoff odds week to week") +
  theme(plot.title = element_text(size = rel(1.7)),
        plot.subtitle = element_text(size=rel(.7),color="grey40", face = "italic"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        plot.title.position = "plot",
        axis.text.y = element_text(size=rel(1),face="bold"),
        strip.text = element_text(size=rel(1.8)),
        legend.position = "none"
  )

ggsave(here("End-of-2020-Reg-Season/plots/Team_Odds_Changes.png"),dpi=900)

