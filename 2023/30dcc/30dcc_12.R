# 30DayChartChallenge 2023
# Day 12. theme day: BBC News (Distribution)
# Topic: FY23 Fair Market Rent, 1-br and 2br, 2 FMR metro area
# Source: https://www.huduser.gov/portal/datasets/fmr/smallarea/index.html via Data Is Plural
# Reference graphic: https://bbc.github.io/rcookbook/

# load libraries
library(tidyverse)
library(readxl)

# read in data
rent = read_xlsx("data/fy2023_safmrs.xlsx") 
rent = rent |> janitor::clean_names() |> rename(ZCTA5CE10=zip_code)

# wrangle
d1 = rent |> #count(hud_metro_fair_market_rent_area_name, sort=TRUE) 
  filter(hud_metro_fair_market_rent_area_name %in% c("Pittsburgh, PA HUD Metro FMR Area","Chicago-Joliet-Naperville, IL HUD Metro FMR Area")) %>%
  select(ZCTA5CE10,hud_metro_fair_market_rent_area_name,safmr_1br,safmr_2br)

d2 = d1 |> pivot_longer(safmr_1br:safmr_2br) |>
  group_by(hud_metro_fair_market_rent_area_name, name) |>
  summarise(median=median(value)) |>
  mutate(bd = case_when(name=="safmr_1br"~"One-bedroom",TRUE~"Two-bedroom"),
         lab=glue::glue("{bd}\nmedian: {scales::dollar(median)}"))
         
# histogram
d1 |>
  ggplot() +
  geom_histogram(aes(x=safmr_2br, fill="Two-bedroom"),color="white",alpha=.8, size=.1,,binwidth = 50) +
  geom_histogram(aes(x=safmr_1br, fill="One-bedroom"), color="white",alpha=.8, size=.1,binwidth = 50) +
  geom_hline(yintercept=seq(2,82,2), size=.2, color="white") +
  geom_hline(yintercept=0, size=.6, color="black") +
  geom_segment(data=d2, aes(x=median, xend=median, y=0, yend=83), linetype="dashed") +
  geom_text(data=d2 %>% filter(name=="safmr_2br"), aes(x=median+20, y=75, label=lab), hjust=0, size=3, lineheight=1,color="grey20") +
  geom_text(data=d2 %>% filter(name=="safmr_1br", hud_metro_fair_market_rent_area_name=="Chicago-Joliet-Naperville, IL HUD Metro FMR Area"), aes(x=median-20, y=75, label=lab), hjust=1, size=3, lineheight=1, color="grey20") +
  geom_text(data=d2 %>% filter(name=="safmr_1br", hud_metro_fair_market_rent_area_name!="Chicago-Joliet-Naperville, IL HUD Metro FMR Area"), aes(x=median-20, y=75, label=lab), hjust=0, size=3, lineheight=1, nudge_x = -210, nudge_y = 5,color="grey20") +
  scale_color_manual(values=c("#127FA1","#991101"), guide="none") +
  scale_fill_manual(values=c("#127FA1","#991101")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(.01,.01)) +
  coord_cartesian(clip="off") +
  facet_wrap(~hud_metro_fair_market_rent_area_name, ncol=1) +
  cowplot::theme_minimal_grid(11) +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text = element_text(size=10.5),
        axis.text = element_text(size=10.5),
        legend.margin=margin(l=-20, t=4, b=3),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.2,"lines"),
        strip.text=element_text(hjust=0, face="bold", size=12, vjust=1, margin=margin(b=10)),
        axis.title=element_blank(),
        axis.ticks=element_line(color="black", size=.3),
        axis.ticks.length.x=unit(.25, "cm"),
        axis.ticks.y=element_blank(),
        plot.title.position = "plot",
        plot.title=element_text(size=14.5),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0,size=9.5, lineheight=1.2, margin=margin(t=-2)),
        plot.margin=margin(.5,.5,.3,.5,unit="cm")
        ) +
  labs(title="Small Area Fair Market Rents (SAFMRs), FY 2023",
       caption="___________________________________________________________________________________________\n#30DayChartChallenge 12. theme day BBC News | Source: Department of Housing and Urban Development")

ggsave("export/30dcc_12.png", bg="white")         