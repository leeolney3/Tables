# Communications Technology Adoption in the U.S.
# Data source: (Charles Kenny and George Yang. 2022)[https://www.cgdev.org/publication/technology-and-development-exploration-data] by way of [Data Is Plural](https://www.data-is-plural.com/archive/2022-07-13-edition/)
## Citation: Charles Kenny and George Yang. 2022. “Technology and Development: An Exploration of the Data.” CGD Working Paper 617. Washington, DC: Center for Global Development. https://www.cgdev.org/publication/technology-anddevelopment-exploration-data

# Trying out a custom line plot with annotations (and lines outside axis)
## Plot Inspired by: [John Burn-Murdoch, Financial Times](https://www.ft.com/content/f472793d-543b-4ebd-9da6-1af5cbc1d73a), [The New York times](https://blog.datawrapper.de/wp-content/uploads/2022/07/image17-1.png), [Our World In Data](https://ourworldindata.org/better-learning)

# Load libraries
library(tidyverse)
library(MetBrewer)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import font
font_add_google("PT Sans")
f1 = "PT Sans"

# Import data
dfs = readr::read_csv("data/Kenny-Yang-tech-diffusion-final-datasets/CHATTING_SPLICED.csv")

# Plot
dfs1 = dfs %>% filter(group=="Consumption", category=="Communications", iso3c=="USA", variable!="telephone") %>%
  mutate(label = str_wrap(label,14)) %>%
  group_by(variable) %>%
  mutate(pmax = value/max(value))

dfs1 %>%
  ggplot() +
  # major y lines
  geom_segment(data= tibble(x=1950, xend=2020, y=seq(.25,1,.25)),aes(x=x, xend=xend, y=y, yend=y), size=.3, color="grey85") + 
  geom_segment(aes(y=0, yend=0, x=1950, xend=2020), size=.3, color="grey80") +
   # y ticks
  geom_segment(data= tibble(x=1949, xend=1950, y=seq(0,1,.25)),aes(x=x, xend=xend, y=y, yend=y), size=.3, color="black") +
  # x ticks
  geom_segment(data= tibble(x=seq(1950,2020,10), y=0, yend=-0.02),aes(x=x, xend=x, y=y, yend=yend), size=.3, color="black") + 
  # lines
  geom_line(data=dfs1 %>% filter(year>=1950),aes(x=year, y=pmax,group=variable, color=label), show.legend = FALSE, size=.55) +
  geom_line(data=dfs1 %>% filter(year<=1950),aes(x=year, y=pmax,group=variable, color=label), show.legend = FALSE, alpha=.3, size=.55) +
  # line labels
  geom_text(data=tibble(label=unique(dfs1$label), x=c(1984,2020.5,2005,2020.5,1951,1960,1974,1951,2020.5,1960),y=c(0.44,.95,1.05,1.05,0.4,.84,0.8,0.6,0.55,0.15)), aes(x=x, y=y,label=label, color=label), size=3, lineheight=.9, hjust=0, show.legend=FALSE, family=f1, fontface="bold") +
  # y axis text
  geom_text(data=tibble(x=1948.5,y=seq(0,1,.25)), aes(x=x, y=y, label=scales::percent(y)), hjust=1, size=3, family=f1) +
  # x axis text
  geom_text(data=tibble(x=seq(1950,2020,10), y=-0.045), aes(x=x, y=y, label=x), size=3, family=f1) +
  coord_cartesian(xlim=c(1949,2026), clip="off") +
  scale_x_continuous("Year",breaks=seq(1950,2020,10)) +
  scale_y_continuous("Value relative to peak",expand = expansion(mult = c(0.02, 0.02)), breaks=seq(0,1,.25), labels=scales::percent) +
  scale_color_manual(values=MetBrewer::met.brewer("Redon",10)) +
  cowplot::theme_map() +
  theme(text=element_text(family=f1),
        plot.title=element_text(size=12),
        plot.subtitle=element_text(size=10, margin=margin(b=0)),
        plot.caption=element_text(size=8, hjust=0, color="grey50",margin=margin(t=10))) +
  labs(title="Communications Technology Adoption in the U.S.", 
       subtitle="Percentage of maximum value by technology",
       caption=str_wrap("Source: Charles Kenny and George Yang. 2022. “Technology and Development: An Exploration of the Data.” CGD Working Paper 617. Washington, DC: Center for Global Development.",125))
       
ggsave("p1.png", p1, height=5, width=7, bg="#F9F9F9")       

