# Carnivorous mammals' diet
# Data: CarniDIET, by way of Data is Plural
# Source: https://github.com/osmiddleton/CarniDIET-Database

# Libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Titillium Web")
f1 = "Titillium Web"

# Data
diet = read_csv("data/diet.csv") 

fel = diet %>% filter(familyCarni=="Felidae") %>% count(commonNameCarni, sort=T) %>%
  filter(n>100) %>% pull(commonNameCarni)
  
can = diet %>% filter(familyCarni=="Canidae") %>% count(commonNameCarni, sort=T) %>% 
  filter(n>100) %>% pull(commonNameCarni)

selected = c(fel, can)

df = diet %>% 
  filter(commonNameCarni %in% selected) %>%
  filter(!is.na(foodType)) %>%
  mutate(foodgrp = fct_lump(foodType,4)) %>%
  count(familyCarni,commonNameCarni, foodgrp) %>%
  group_by(familyCarni,commonNameCarni) %>%
  mutate(pct=n/sum(n)) 
  
lev = df %>% filter(foodgrp=="Mammal") %>%
   arrange(pct) %>% pull(commonNameCarni)

# Plot
df %>%
  ggplot(aes(pct,factor(commonNameCarni, levels=lev))) +
  geom_col(aes(fill=factor(foodgrp, levels=c("Other","Invertebrate","Plant","Bird","Mammal"))),
           width=.7) +
  geom_text(data=df%>% filter(foodgrp=="Mammal"),aes(label=scales::percent(pct, accuracy=.1)), 
            size=3.2, hjust=1.1, family=f1) + 
  facet_wrap(~familyCarni, scales="free") +
  scale_fill_manual(values=c("#ced4da","#1d3557","#457b9d","#a8dadc","#FF775F")) +
  scale_x_continuous(expand=c(0,0), labels=scales::percent) +
  cowplot::theme_minimal_vgrid(12) +
  theme(text=element_text(family=f1),
        legend.margin=margin(l=-4.15, unit="cm"),
        legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=10),
        plot.subtitle=element_text(size=10),
        plot.caption=element_text(size=9, hjust=0),
        axis.title=element_blank(),
        plot.margin=margin(.5,.75,.3,.5, unit="cm"),
        strip.text=element_text(size=11.5, face="bold"),
        panel.grid=element_line(size=.3),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.spacing = unit(1.5, "lines")
        ) +
  guides(fill=guide_legend(reverse = T)) +
   labs(subtitle="Food type proportion of carnivorous mammals of the Canidae and Felidae family with more than 100 records",
       title="Carnivorous mammals' diet",
       caption="\nSource: Owen Middleton et al. CarniDIET, by way of Data Is Plural")
       
ggsave("carnivorous.png", height=6, width=8.5, bg="white")




