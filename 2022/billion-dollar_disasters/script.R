# Billion-dollar disasters (1982-2022)
# Data from [US National Centers for Environmental Information](https://www.ncei.noaa.gov/access/billions/events) by way of [Data Is Plural](https://www.data-is-plural.com/archive/2022-08-31-edition/)

# Load libaries
library(tidyverse)
library(lubridate)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load font
font_add_google("JetBrains Mono")
f1 = "JetBrains Mono"
font_add_google("Open Sans")
f2 = "Open Sans"

# Read in data
events = readr::read_csv("data/events-US-1982-2021.csv", skip=1) %>% janitor::clean_names()

# Wrangle
d1 = events %>% mutate(begin_date=lubridate::ymd(begin_date),
         begin_yr=lubridate::year(begin_date),
         bin = cut_interval(begin_yr, n = 8),
         grp=case_when(deaths>0~"1",TRUE~"0"),
         #grp=case_when(disaster=="Severe Storm"~"Severe Storm", TRUE~"Other"),
         value=total_cpi_adjusted_cost_millions_of_dollars/1000)

d2 = d1%>%
  group_by(bin) %>%
  summarise(min=min(begin_yr), max=max(begin_yr)) %>%
  ungroup() %>%
  mutate(min2=case_when(min==1988~min-1, TRUE~min)) %>%
  mutate(x=lubridate::ymd(glue::glue("{min2}0101")),
         xend=lubridate::ymd(glue::glue("{max}1231")))
         
# Plot
p1= d2 %>%
  ggplot(aes(x=x, y=0)) +
  geom_segment(aes(x=x,xend=xend, y=0, yend=0), size=.3)+
  geom_point(data=d1, aes(x=begin_date, y=0), shape=21, color="white",fill="white",stroke=.3, size=3) +
 geom_point(data=d1, aes(x=begin_date, y=0, fill=value), shape=21, color="black",stroke=.3, size=3, alpha=.8) +
  facet_wrap(~bin, ncol=1, scales="free_x", strip.position="right") +
  scale_x_date(date_breaks="1 year", date_label="%Y") +
  viridis::scale_fill_viridis(option="plasma", direction=-1,trans="pseudo_log",breaks=c(1.5,15,150)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_grid(9.8) +
  theme(text=element_text(family=f2),
        strip.text.y=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title=element_blank(),
        axis.ticks.y=element_blank(),
        panel.spacing = unit(1.05, "lines"),
        legend.position = "top",
        legend.title=element_text(size=9, face="bold"),
        legend.text=element_text(size=8.5),
        plot.title=element_text(size=14, family=f1, margin=margin(b=7)),
        plot.subtitle=element_text(size=8.8, lineheight=1,margin=margin(b=10)),
        plot.caption=element_text(hjust=0, color="grey30",margin=margin(t=10))) +
  labs(fill="Total CPI adjusted cost (billion of dollars)",
       subtitle="Between 1982 and 2021, there were 318 severe weather and climate events in the U.S. that have caused at least\n$1 billion in estimated direct losses (CPI-Adjusted). Including 20 Wildfire, 28 Drought, 151 Severe Storm, 56 Tropical\nCyclone, 35 Flooding, 20 Winter Storm, and 8 Freeze disaster events.",
       title="Billion-dollar Disasters",
       caption="Source: NOAA National Centers for Environmental Information (NCEI) U.S. Billion-Dollar Weather and Climate Disasters (2022).\nhttps://www.ncei.noaa.gov/access/billions/, DOI: 10.25921/stkw-7w73") +
  guides(fill = guide_colorbar(title.position = "top", barwidth = unit(12.9, "lines"), barheight = unit(.4, "lines")))

ggsave("p1.png", p1, height=7, width=7, bg="white")      