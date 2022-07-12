# SNL casts by gender (season 1 to 46) network plot
# Data from [snldb Github](https://github.com/hhllcks/snldb) and [snlarchives.net](http://www.snlarchives.net/) by way of [Data Is Plural](https://www.data-is-plural.com/archive/2022-07-06-edition/)
# Credits to Georgios Karamanis for network plot method
# Color palette from https://dopely.top/

# Load libraries
library(tidyverse)
library(lubridate)
library(ggtext)
library(tidygraph)
library(ggraph)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import font
font_add_google("Fira Sans")
f1 = "Fira Sans"

# Import data
actors= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/actors.csv")
appearances= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/appearances.csv")
casts= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/casts.csv")
characters= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/characters.csv")
episodes= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/episodes.csv")
hosts= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/hosts.csv")
impressions= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/impressions.csv")
seasons = readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/seasons.csv")
sketches= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/sketches.csv")
tenure= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/tenure.csv")
titles= readr::read_csv("https://raw.githubusercontent.com/hhllcks/snldb/master/output/titles.csv")

# Prepare data
d1 = casts %>% left_join(actors) %>%
  filter(type=="cast") %>%
  select(aid,sid,gender) 
  
d2 = d1 %>% filter(gender=="female")
d3 = d1 %>% filter(gender=="unknown")  
d4 = d1 %>% filter(!gender=="male") %>% count(aid, sort=T) %>% filter(n>5)

graph = d1 %>%
  select(from=sid, to=aid) %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(is_season = if_else(name %in% d1$sid, TRUE, FALSE),
         col=case_when(is_season==TRUE ~"Season",
                       is_season==FALSE & (name%in%d2$aid)~"Female", 
                       is_season==FALSE & (name%in%d3$aid)~"Unknown",
                       TRUE~"Male"),
         is_label = if_else(name %in% d4$aid, TRUE, FALSE)) 
         
# Plot
graph %>%
  ggraph(layout = "stress") +
  geom_edge_diagonal(color="#f2e9e4", alpha=.2, edge_width=.3) +
  geom_node_point(aes(size = if_else(is_season,3,1.3), color=factor(col, levels=c("Season","Female","Male","Unknown"))),
                  show.legend = FALSE) +
  geom_text(aes(x, y, label=if_else(is_season, name, NULL)), size=2, color="black", family=f1) +
  #ggrepel::geom_text_repel(aes(x, y, color=factor(col, levels=c("Season","Female","Male","Unknown")), label=if_else(is_label, name, NULL)), size=1.7, family=f1, show.legend = FALSE) +
  scale_size_identity() +
  scale_color_manual("",values=c("#e9ecef","#FFD704","#6942EF","#23CBC8")) +
  cowplot::theme_map(10) +
  theme(legend.direction = "horizontal",
        text=element_text(color="#f8f9fa", family=f1),
        plot.subtitle=element_markdown(),
        plot.caption=element_text(color="#dee2e6"),
        plot.background=element_rect(fill="#212529", color=NA),
        plot.margin=margin(.5,.5,.5,.5,unit="cm")) +
  labs(title="SNL casts, Season 1 to 46",
       subtitle="<span style='color:#6942EF'>**100 male**</span>, <span style='color:#FFD704'>**52 female**</span>, <span style='color:#23CBC8'>**2 unknown**</span>",
       caption="Source: snldb Github and snlarchives.net by way of Data is Plural")

ggsave("p1.png", height=8,width=8)         
