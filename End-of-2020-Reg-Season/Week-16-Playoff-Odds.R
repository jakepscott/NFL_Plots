# Get Libraries and Fonts-----------------------------------------------------------
library(listviewer)
library(tidyverse)
library(ggimage)
library(jsonlite)
library(scales)
library(ggtext)
theme_set(theme_minimal(base_size = 12,base_family = "Roboto Condensed"))
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))


# Get Facet Highlight code ------------------------------------------------
#From here: https://cedricscherer.netlify.app/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
element_textbox_highlight <- function(..., 
                                      hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL,
                                      hi.labels2 = NULL, hi.fill2 = NULL,
                                      hi.col2 = NULL, hi.box.col2 = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col,
           hi.labels2 = hi.labels2, hi.fill2 = hi.fill2, hi.col2 = hi.col2, hi.box.col2 = hi.box.col2)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element",
              "element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
  }
  if (label %in% element$hi.labels2) {
    element$fill <- element$hi.fill2 %||% element$fill
    element$colour <- element$hi.col2 %||% element$colour
    element$box.colour <- element$hi.box.col2 %||% element$box.colour
  }
  NextMethod()
}



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

team_meta <- espnscrapeR::get_nfl_teams() %>% 
  select(abb_name = team_short_name, team_color,logo)

data <- data %>% left_join(team_meta,by=c("name"="abb_name")) %>% mutate(logo_fixed=case_when(week==16~logo))



# Plot --------------------------------------------------------------------
asp_ratio <- 1.68

(plot <- data %>%
  ggplot(aes(x=week,y=make_playoffs)) +
  geom_line(aes(color=team_color),lwd=.8) +
  geom_image(aes(image = logo_fixed), size = 0.15, by = "width", asp = asp_ratio) +
  scale_color_identity() +
  scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                     labels = function(x) paste0(x*100, "%"),
                     expand = expansion(mult =  0.2)) +
  scale_x_continuous(expand = expansion(mult =  0.2),
                     breaks=c(0,4,8,12,16)) +
  labs(title="Playoff Probabilities heading into Week 17",
       subtitle = "Calculated using ELO. Grey indicates a team has clinched the division",
       y="",
       x="",
       caption = "Plot: @jakepscott2020 | Data: 538, Using @thomas_mock's Method") +
  facet_wrap(~division) +
  theme(plot.title = element_text(size = rel(1.7)),
        plot.subtitle = element_text(size=rel(1),color="grey40", face = "italic"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        plot.title.position = "plot",
        legend.position = "none",
        strip.text = element_textbox_highlight(
          size=rel(1.1), face="bold",
          fill = "white", box.color = "white", color = "black",
          halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(0, 0, 0, 0),
          #New cases rising in red
          hi.labels = (data %>% group_by(division) %>%
                         mutate(clinched=case_when(max(make_playoffs)==1~"Clinched",TRUE~"No")) %>% 
                         filter(clinched=="Clinched") %>% 
                         distinct(division) %>% 
                         pull(division)),
          hi.fill = "grey", hi.box.col = "grey", hi.col = "black"
        )))

ggsave(here("End-of-2020-Reg-Season/plots/playoff_odds.png"),dpi=600)
