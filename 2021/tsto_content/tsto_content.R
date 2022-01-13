# Game content added in The Simpsons Tapped Out, from 2017 to 2021
# Data from wikisimpsons.com
# Plot inspired by @cnicault https://twitter.com/cnicault/status/1377690522815500290/photo/1

library(tidyverse)
library(ggtext)
library(lubridate)
library(cowplot)
options(dplyr.summarise.inform = FALSE)

library(showtext)
font_add_google("Roboto Condensed")
font_add_google("Roboto Mono")
showtext_auto()

# import data
df = read_csv("data/tsto_content.csv")

# wrangle
df1 = df %>% 
  mutate(date=dmy(date),year=year(date)) %>%
  filter(update>128, year>=2017) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  summarise(across(char:deco_skin, sum)) %>%
  pivot_longer(everything()) %>%
  mutate(game="tsto") %>%
  mutate(name2=case_when(str_detect(name, "skin")| name=="npc"~"other",TRUE~name)) 

df1a = df1 %>% group_by(game, name2) %>%
  tally(value) %>%
  mutate(prop=n/sum(n))

df1b = df1 %>%
  filter(name2 == "other") %>%
  mutate(prop=value/sum(value)) %>%
  select(-name2) %>%
  select(game, name2 = name, n=value, prop)
  
df2 = bind_rows(df1a, df1b) %>%
  mutate(name2 = recode(name2, char="Character", build ="Building", deco="Decoration", other="Other",
                        npc="NPC", char_skin="Character skin", build_skin="Building skin", deco_skin="Decoration skin")) %>%
  mutate(x =case_when(n>100 ~ "p1",TRUE~"p2")) %>%
  arrange(desc(prop)) %>%
  mutate(name2 = fct_rev(fct_inorder(name2)))

df2a = df2 %>% group_by(x) %>% arrange(x, desc(name2)) %>% mutate(cs = cumsum(prop), cs_n = cumsum(n))

poly2 <- tibble(x = c(1.25, 1.75, 1.75, 1.25),
               y = c(.999, .999, 0,0.9280576))

# plot
p1 = df2 %>% ggplot(aes(x=x, y=prop, fill=name2)) +
  geom_col(width=.5) +
  geom_text(aes(label=glue::glue("{name2}: {n}")),position = position_stack(vjust = 0.5), 
            color="white", size=2.9, family="Roboto Condensed") +
  annotate(geom="segment", x=1.25, xend=1.75, y=.999, yend=.999, size=.2) +
  annotate(geom="segment", x=1.25, xend=1.75, y=0.9280576, yend=0, size=.2) +
  geom_polygon(data=poly2, aes(x,y), fill="#377394", alpha=.2, color="#377394", size=.1) +
  # side labels
  geom_segment(data = df2a,aes(x=ifelse(x=="p1",.5,2.25), xend=ifelse(x=="p1",.75,2.5), y=cs, yend=cs), size=.2) +
  geom_text(data = df2a,aes(x=ifelse(x=="p1",.44,2.545), y=cs, label=cs_n), size=2.9, 
            family="Roboto Condensed") +
  # scales
  scale_fill_manual(values=c("#6dabc5","#012a4a","#528fad","#377394","#4f772d",
                             "#d8973c","#a4243b","#0f4970"), guide="none") +
  scale_x_discrete(expand=c(.5,.5)) +
  # theme
  theme_void(base_size = 10, base_family = "Roboto Mono") +
  theme(plot.title=element_text(hjust=.5,face="bold"),
        plot.subtitle = element_text(hjust=.5, size = 9, lineheight = 1.3, margin=margin(t=8, b=2)),
        plot.margin = unit(c(.5, .5, 0.4, 0), "cm"),
        plot.caption=element_text(hjust=.5, size=7),
        plot.background=element_rect(fill="#F9F9F9", color=NA)
        ) +
  labs(title="GAME CONTENT ADDED IN THE SIMPSONS: TAPPED OUT",subtitle="2017 to 2021",
       caption="Data from wikisimpsons.com")
       
# insert image
url<-"https://upload.wikimedia.org/wikipedia/en/4/4a/The_Simpsons_-_Tapped_Out.png"
img = magick::image_read(url)

p1b <- ggdraw() +
  draw_plot(p1) +
  draw_image(img,  x = 0.35, y = -0.25, scale = .2) 

ggsave("tsto.png", height=5, width=7)
