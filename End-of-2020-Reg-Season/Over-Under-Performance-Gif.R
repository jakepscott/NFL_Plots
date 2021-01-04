library(gganimate)

ranked <- data %>% 
  arrange(week, overperformance) %>% 
  group_by(week) %>% 
  dplyr::mutate(rank = rank(overperformance, ties.method = "first"))
  


plot <- ranked %>% 
  ggplot(aes(x=rank,y=overperformance)) +
  geom_col(aes(fill=team_color,color=team_color2)) + 
  geom_image(aes(image = team_logo_wikipedia), size = 0.03, by = "width", asp = asp_ratio) +
  scale_x_discrete(expand = expansion(mult = 0.05)) +
  scale_y_continuous(position = "right",
                     breaks=c(-.8,-.6,-.4,-.2,0,.2,.4,.6,.8),
                     labels = function(x) paste0(x*100, "%")) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip(ylim = c(-.8,.8)) +
  labs(title="Which Teams Overperformed This Season? ",
       subtitle="Overperformance as of Week {closest_state}",
       y = "Chance of Making Playoffs in Week {closest_state} Minus Chance in Week 1",
       x="",
       caption = "Plot: @jakepscott2020 | Data: ELO Rating from 538, Using @thomas_mock's Method") +
  theme(plot.title =element_text(size=rel(2),face="bold"),
        plot.subtitle = element_text(size = rel(1.4),face="italic"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        legend.position = "none"
  ) +
  transition_states(
    week,
    transition_length = .25, 
    state_length = 1,
    wrap = FALSE
  ) 



animate(plot, height = 500, width = 500, fps=25,duration = 30, end_pause = 30)
anim_save(here("End-of-2020-Reg-Season/plots/Overperformance-by-week-full.gif"))

# ggsave(here("End-of-2020-Reg-Season/plots/Overperformance.png"),dpi=600,
#        width=14,
#        height=8,
#        units = "in")



