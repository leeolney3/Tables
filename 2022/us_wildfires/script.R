# Human-caused/lighting-caused wildfire acres in the US, 2015-2021
# Data source: [National Interagency Fire Center](https://www.nifc.gov/fire-information/statistics)

# Load libraries
library(tidyverse)
library(ggalluvial)
library(patchwork)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load font
font_add_google("Outfit")
f1 = "Outfit"

# Import data
human1= readr::read_csv("data/nifc/human-caused_acres.csv",show_col_types = FALSE)
lighting1= readr::read_csv("data/nifc/lighting_acres.csv",show_col_types = FALSE)

# Wrangle
human1a = human1 %>% select(-Total) %>% filter(Year>=2015) %>%
  mutate(across(where(is.character), as.double)) %>%
  pivot_longer(!Year) %>%
  drop_na() %>% mutate(grp="Human-caused Acres (by Geographic Area)")

lighting1a = lighting1 %>% select(-Total) %>% filter(Year>=2015) %>%
  mutate(across(where(is.character), as.double)) %>%
  pivot_longer(!Year) %>%
  drop_na() %>% mutate(grp="Lightning-caused Acres (by Geographic Area)")

lab1 = human1a %>% filter(Year==max(Year)) %>%
  arrange(value) %>%
  mutate(cs = cumsum(value),
         y=case_when(value==min(value)~0, TRUE~lag(cs) + value*.5))

lab2 = lighting1a %>% filter(Year==max(Year)) %>%
  arrange(value) %>%
  mutate(cs = cumsum(value),
         y=case_when(value==min(value)~0, TRUE~lag(cs) + value*.5))
         
# Color palette from https://blog.datawrapper.de/favorite-popular-chart-types/
pal = c("#01dca6", "#b83229", "#019176","#ff8902","#15607a","#1d81a2","#a9c2bb","#19a1cd","#ffbe48","#ff483b")         
         
# Plot
p1 = rbind(human1a, lighting1a) %>%
  ggplot() +
  geom_segment(data=tibble(y=seq(0,8000000,1000000), x=2014.8,xend=2021.3), aes(x=x, xend=xend, y=y, yend=y), inherit.aes = FALSE, size=.35, color="grey80") +
  geom_alluvium(aes(x = Year, y = value, alluvium = name,fill = name), alpha = .8, decreasing = FALSE, size=.2, show.legend = FALSE) +
  ggrepel::geom_text_repel(data=rbind(lab1,lab2), aes(x=Year+.2, y=y, label=name, color=name), size=3.3, show.legend=FALSE, direction="y", hjust=0, nudge_x =.62, segment.linetype="dotted", family=f1, box.padding = 0.1, min.segment.length = .2) +
  scale_y_continuous("Acres",labels=scales::label_number_si(), expand=c(0.0,0.0), breaks=seq(0,8000000,1000000),limits=c(0,8112688)) +
  scale_x_continuous("Year",limits=c(NA,2023.5), breaks=seq(2015,2021,1), expand=c(0,0)) +
  facet_wrap(~grp, ncol=1, scales="free") +
  scale_color_manual(values=pal) +
  scale_fill_manual(values=pal) +
  cowplot::theme_minimal_grid(10) +
  theme(text=element_text(family=f1),
        panel.grid = element_blank(),
        axis.ticks.length = unit(.38,"lines"),
        axis.title.x = element_text(hjust=.38),
        strip.text = element_text(hjust=0, size=10.5, margin=margin(b=8)),
        panel.spacing.y = unit(1.1,"lines"),
        plot.caption = element_text(size=9, color="grey30"),
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5,size=14, margin=margin(t=4, b=10))) +
  labs(caption="Source: National Interagency Fire Center",
       title="Wildfires in the US, 2015 - 2021")

ggsave("nifc_p1.png",p1, height=8, width=7, bg="#FFFBF7")
    

