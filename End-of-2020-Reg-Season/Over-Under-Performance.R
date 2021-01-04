# Get Libraries and Fonts-----------------------------------------------------------
library(listviewer)
library(tidyverse)
library(ggimage)
library(jsonlite)
library(scales)
library(ggtext)
library(patchwork)
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

data_nologo <- weekly_elo %>% 
  select(week,name,conference, division,make_playoffs,current_wins,current_losses,current_ties) %>% 
  mutate(name=ifelse(name=="WSH","WAS",name))

team_info <- nflfastR::teams_colors_logos

data <- data_nologo %>% left_join(team_info,by=c("name"="team_abbr")) 

week_one_odds <- data %>% 
  group_by(name) %>% 
  slice_min(week,n=1) %>% 
  mutate(week_one_odds=make_playoffs) %>% 
  select(name,week_one_odds)

data <- data %>% 
  left_join(week_one_odds) %>% 
  mutate(overperformance=make_playoffs-week_one_odds) 

asp_ratio <- 1.5

(a <- data %>% 
  filter(week==16) %>% 
  ggplot(aes(x=fct_reorder(name,overperformance),y=overperformance)) +
  geom_col(aes(fill=team_color,color=team_color2)) + 
  geom_image(aes(image = team_logo_wikipedia), size = 0.03, by = "width", asp = asp_ratio) +
  geom_curve(aes(x = "HOU", y = .4, xend = "SF", yend = .02),
             curvature = -.3,
             angle = 100,
             color = "#aa0000",
             size = 1,
             arrow = arrow(length = unit(0.025, "npc"),
                           type = "closed")) +
  geom_curve(aes(x = "SEA", y = -.4, xend = "TB", yend = -.02),
             curvature = -.3,
             angle = 100,
             color = "#d50a0a",
             size = 1,
             arrow = arrow(length = unit(0.025, "npc"),
                           type = "closed")) +
  geom_richtext(y=-.4,x="GB",
                label="<span style = 'color:#d50a0a;'>The Buccaneers</span> had just a 38% chance of making the playoffs headed into week 1, but now <br>have guarenteed themselves a spot in the postseason, overperfoming by **62%**",
                label.color = NA,
                size=4.2) +
  geom_richtext(y=.4,x="DEN",
                label="<span style = 'color:#aa0000;'>The 49ers</span> were favored to go to the playoffs (70% chance), but a string of <br>injuries later and they have been eliminated, underperfoming by **70%**",
                label.color = NA,
                size=4.2) + 
  scale_x_discrete(expand = expansion(mult = 0.05)) +
  scale_y_continuous(position = "right",
                     breaks=c(-.8,-.6,-.4,-.2,0,.2,.4,.6,.8),
                     labels = function(x) paste0(x*100, "%")) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip() +
  labs(title="Which Teams Overperformed This Season? ",
       subtitle="<span style = 'color:#d50a0a;'>**Tampa Bay**</span> and <span style = 'color:#008e97;'>**Miami**</span> outperformed expectations, <span style = 'color:#aa0000;'>**San Francisco**</span> and <span style = 'color:#002244;'>**New England**</span>... not so much... ",
       y = "Chance of Making Playoffs in Week 17 Minus Chance in Week 1",
       x="",
       caption = "Plot: @jakepscott2020 | Data: 538, Using @thomas_mock's Method") +
    theme(plot.title =element_text(size=rel(2),face="bold"),
          plot.subtitle = element_markdown(size = rel(1.4),face="italic"),
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70"),
          plot.title.position = "plot",
          axis.text.y = element_blank(),
          legend.position = "none"
    ))

ggsave(here("End-of-2020-Reg-Season/plots/Overperformance.png"),dpi=600,
       width=14,
       height=8,
       units = "in")



# blank <- tibble(x=-6:6,y=-6:6)
# 
# 
# (b <- ggplot(blank,aes(x=-6:6),y=-1:1) +
#   annotate(geom = "segment", x = .5, xend = 4, y = -1, yend = -1, color = "#013369", 
#            arrow = arrow(angle = 30, length = unit(0.5, "lines")),
#            size=2) +
#   annotate(geom = "segment", x = -.5, xend = -4, y = -1, yend = -1, color = "#D50A0A", 
#            arrow = arrow(angle = 30, length = unit(0.5, "lines")),
#            size=2) +
#   annotate(geom="text",x=2.5,y=-.8,label="Overperformed",color="#013369",
#            size=5) +
#   annotate(geom="text",x=-2.5,y=-.8,label="Underperformed",color="#D50A0A",
#            size=5) +
#   coord_cartesian(ylim=c(-1.1,-.4),
#                   xlim=c(-6,6)) +
#   theme_void()) +
#   theme(plot.margin = margin(0,0,0,0),
#         axis.text = element_blank(),
#         axis.title = element_blank())
# 
# 
# b/a + plot_layout(ncol=1,heights = c(1,8)) +
#   plot_annotation(title="Test") &
#   theme(plot.margin = margin(0,0,0,0))
# 
# 
# 

