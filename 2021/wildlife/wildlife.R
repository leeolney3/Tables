# How are wildlife populations changing?
# Data from ourworldindata.org by way of MakeoverMonday (https://data.world/makeovermonday/2021w21)

library(tidyverse)
library(httr)
library(readxl)
library(ggtext)

library(showtext)
font_add_google("Lato") 
font_add_google("Libre Franklin")
showtext_auto()
f1 = "Lato"
f2 = "Libre Franklin"

# Get data
GET("https://query.data.world/s/ngprbmqsgo7hano2ifvkg35bvc5x5s", write_disk(tf <- tempfile(fileext = ".xlsx")))
wildlife <- read_excel(tf) %>% janitor::clean_names()

# Prepare data
tab = wildlife %>% mutate(stable_pos =  stable/2, 
               stable_neg = -1*stable/2,
               inc=-1*increasing_populations,
               dec=1*decreasing_populations) %>%
  dplyr::select(taxonomic_group, inc, stable_neg, stable_pos, dec) %>%
  pivot_longer(2:5) %>%
  mutate(
    taxonomic_group = case_when(taxonomic_group=="Fish" ~"Fishes", TRUE~taxonomic_group),
    taxonomic_group = factor(taxonomic_group, levels=c("Amphibians","Fishes","Birds","Mammals","Reptiles")))
    
# Plot
# reference: https://ourworldindata.org/living-planet-index-understanding

tab %>%
  ggplot(aes(y=fct_rev(taxonomic_group), x=value, fill=name)) +
  geom_col(width=0.45, show.legend=F) +
  scale_fill_manual(values=c("#c8553d","#489fb5","#033f63","#033f63")) +
  scale_x_continuous(expand=c(0,0)) +
  coord_cartesian(clip="off") +
  geom_text(data=tab %>% filter(name=="inc"|name=="dec"),
            aes(label=scales::percent(abs(value),accuracy=1)), position = position_stack(vjust = .5), 
            color="white", size=3, fontface="bold", family=f1) +
  # stable pct labels
  annotate(geom="text", y=c(1,2,3,4,5), x=rep(0,5), label=scales::percent(c(0.06,0.07,0.04,0.07,0.06), accuracy=1), 
           color="white",size=3, fontface="bold", family=f1) +
  # color labels
  annotate(geom="text", x=-c(-.25,0,.25), y=5.5, label=c("Declining Populations","Stable","Increasing Populations"),
           family=f1, color=c("#c8553d","#033f63","#489fb5"), fontface="bold", size=3.5) +
  theme_minimal() +
  theme(text=element_text(family=f2),
        panel.grid = element_blank(),
        axis.text.x=element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_text(size = 10, face="bold", family=f1, color="black"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=7.7, margin=margin(b=10)),
        plot.caption.position = "plot",
        plot.caption=element_text(size=7, family=f2, hjust=0, margin=margin(t=20)),
        plot.margin=margin(rep(.5,4),unit="cm"),
        plot.background = element_rect(fill="#fefcfb", color=NA)) +
  labs(title="How are wildlife populations changing?",
       subtitle="Shown is the share of studied populations in each taxonomic group with increasing, stable of declining abundance. The 2020 Living\nPlanet Index reported a 68% average decline in wildlife population since 1970.\n\nAround half of population are increasing and half are in the decline: to get a 68% average decline, the magnitude of declining population\nmust be much greater than the magnitude of increasing ones",
       caption="Data from Our World In Data, by way of MakeoverMonday | Plot inspired by Our World In Data") + 
  # add text
  annotate(geom="richtext", x=-c(-.3,0,.3), y=0.2, 
           label=c("43% of studied reptiles<br>had *declining* numbers",
                   "6% were neither<br>increasing or decreasing",
                   "51% of the studied reptiles<br>had *increasing* numbers"),
           family=f1, color=c("#c8553d","#033f63","#489fb5"), size=2.8, 
           fill = NA, label.color = NA) +
  annotate(geom="text", x= .05, y=4.6, label="Amphibians had the greatest share of population in decline ",
           size=2.8, color="#c8553d", fontface="italic", hjust=0) +
  # add arrows
  annotate(geom="segment", x=c(0,-.3,.3), y=rep(0.45,3), xend =c(0,-.3,.3), yend =rep(.75,3),color = "grey20",size=.3,
           arrow = arrow(length = unit(0.02, "npc")))
           
 