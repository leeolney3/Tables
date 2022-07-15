# Early movie theaters in Oregon 
# Data source: [Oregon Theater Project Database](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FGOUZ3) by way of [Data Is Plural]()
# Citation: Aronson, Michael; Peterson, Elizabeth, 2022, "Oregon Theater Project Database", https://doi.org/10.7910/DVN/FGOUZ3, Harvard Dataverse, V2, UNF:6:MnoF6Z1fUlJOTsz/JdWGig== [fileUNF]
# Plot inspired by [Gregor Aisch](https://blog.datawrapper.de/longest-terms-european-leaders/)

# Load libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load font
font_add_google("Outfit")
f1 = "Outfit"

# Import data
theaters = readr::read_csv("data/theaters-clean-2022-06-21.csv") %>% janitor::clean_names()

# Wrangle
df = theaters %>%
  select(theater_name, address, end_year,start_year, city,number_of_seats) %>%
  mutate(id=row_number(),
         years_op = end_year-start_year,
         seats =parse_number(number_of_seats)) %>%
  filter(years_op>0) %>%
  pivot_longer(end_year:start_year) %>%
  mutate(years_op=case_when(name=="start_year"~0, TRUE~years_op),
         city_group = case_when(city=="Portland, OR"~"Portland", TRUE~"Other cities in Oregon"),
         theater_name=case_when(theater_name=="Houston's Opera House"~"Houston's\nOpera\nHouse", TRUE~theater_name)) 
         
# Plot
p1 = df %>%
  ggplot(aes(value, years_op, group=id)) +
  geom_segment(data=tibble(x=1900, xend=2022, y=seq(25,125,25)),aes(x,y,xend=xend, yend=y),inherit.aes = FALSE, size=.3, color="grey90") +
  geom_segment(aes(x=1900, xend=2022, y=0, yend=0),inherit.aes=FALSE, size=.4, color="grey30") +
  geom_line(size=.3, show.legend = FALSE, color="grey65",alpha=.8) +
  geom_point(data=df %>% filter(name=="end_year", city_group!="Portland"), aes(value, years_op, fill="Other cities in Oregon"), shape=21, color="white", size=2) +
  geom_point(data=df %>% filter(name=="end_year", city_group=="Portland"), aes(value, years_op, fill="Portland"), shape=21, color="white", size=2, ) +
  # repel, right
  ggrepel::geom_text_repel(data=df %>% filter(name=="end_year",value==max(value)), aes(value, years_op, label=theater_name, color=city_group), size=3, hjust=0, direction="y",xlim=c(2030,NA), box.padding = .1, max.overlaps=20, segment.size=.3, segment.linetype="dotted",family=f1,show.legend=FALSE) +
  # repel labels Hollywood, Oriental Theatre
  ggrepel::geom_text_repel(data=df %>% filter(name=="end_year",id%in%c(128,85)), aes(value, years_op, label=theater_name,color=city_group), size=3, hjust=0, box.padding = .1, max.overlaps=20, segment.size=.3,family=f1, show.legend=FALSE) +
  # top
  geom_text(data=df %>% filter(name=="end_year",id %in% c(187,69,21,82,137)), aes(value, years_op, label=theater_name, color=city_group), size=3, show.legend=FALSE, nudge_y=3, nudge_x = 3, lineheight=.7,family=f1) +
  # right
  geom_text(data=df %>% filter(name=="end_year",between(value,1980,2000) | id %in% c(172,192,141)), aes(value, years_op, label=theater_name, color=city_group), size=3, hjust=0, show.legend=FALSE, nudge_x = 2,family=f1) +
  # left 
  geom_text(data=df %>% filter(name=="end_year",id%in%c(148,195,139,12,108)), aes(value, years_op, label=theater_name, color=city_group), size=3, hjust=1, show.legend=FALSE, nudge_x = -2, lineheight=.7, nudge_y = .1,family=f1)  +
  # wrap long labels
  geom_text(data=df %>% filter(name=="end_year",id%in%c(87,36)), aes(value, years_op, label=str_wrap(theater_name,10), color=city_group), size=3, hjust=.5, nudge_y=7, nudge_x=-2,show.legend=FALSE, lineheight=.7,family=f1) +
  # add y-axis title inside plot
  annotate(geom="text", x=1900.5, y=133, label="Total years in operation", hjust=0, size=3.5, color="grey50",fontface="bold",family=f1) +
  scale_color_manual(values=c("#8165C1","#F25F1B")) +
  scale_fill_manual(values=c("#8165C1","#F25F1B")) +
  scale_x_continuous(breaks=seq(1900,2020,20)) +
  coord_cartesian(xlim=c(1900,2060), expand=FALSE, clip="off") +
  cowplot::theme_minimal_grid(11,line_size = .3, color="grey90") +
  theme(text=element_text(family=f1),
        legend.position = "top",
        panel.grid.major.y=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=8.5),
        legend.title=element_blank(),
        legend.text=element_text(size=9.5),
        legend.margin=margin(l=-30),
        plot.title.position = "plot",
        plot.title=element_text(size=12),
        plot.caption.position = "plot",
        axis.ticks.length = unit(.4,"lines"),
        axis.ticks=element_line(size=.3),
        plot.caption = element_text(size=8,hjust=0, color="#808080",margin=margin(t=10))) +
  labs(title="Early Movie Theaters in Oregon",
       caption="Source: Aronson, Michael; Peterson, Elizabeth, 2022, Oregon Theater Project Database, https://doi.org/10.7910/DVN/FGOUZ3,\nHarvard Dataverse, V2, UNF:6:MnoF6Z1fUlJOTsz/JdWGig== [fileUNF]") +
  guides(fill = guide_legend(reverse=TRUE,override.aes = list(size=3)))
    
ggsave("p1.png",p1,height=7, width=6, bg="white")           