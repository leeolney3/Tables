# 2022-08-18
# Atomic gardening
# Data: [Mutant Variety Database](https://nucleus.iaea.org/sites/mvd/SitePages/Search.aspx) from International Atomic Energy Agency and the UN’s Food and Agriculture Organization

# Load libaries
library(tidyverse)
library(ggtext)
library(ggstream)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import data
mutant= readr::read_csv('data/Mutant_Variety_Search.csv') %>% janitor::clean_names()

# Load fonts
font_add_google("DM Sans")
f2 = "DM Sans"
font_add_google("IBM Plex Serif")
f2 = "IBM Plex Serif"
font_add_google("IBM Plex Sans")
f3 = "IBM Plex Sans"

# Plot 1
# plot inspired by [@ldbailey255](https://twitter.com/ldbailey255/status/1558471569894170624) and [@_ansgar](https://twitter.com/_ansgar/status/1553066487879016449)

data= mutant %>%
  mutate(grp=fct_lump(common_name,5)) %>%
  count(grp,registration)
  
data %>%
  ggplot(aes(registration,n, fill=factor(grp, levels=c("Rice","Barley","Chrysanthemum","Wheat","Soybean","Other")))) +
  geom_segment(data=tibble(x = seq(1925,2025,25),y = -100, yend = 100),aes(x=x, xend=x, y=y, yend=yend), colour = "grey10",size = 0.3, inherit.aes = FALSE, linetype="dotted") +
  geom_text(data=tibble(x = seq(1925,2025,25),y = 100),aes(x=x, y=y, label=x), inherit.aes = FALSE,angle=90, size=3, color="grey10", vjust=-.5, hjust=1, family=f1) +
  ggstream::geom_stream(bw = 0.5, colour = "#1a1110", size = 0.15,show.legend=FALSE) +
  geom_text(data=tibble(label=c("Rice","Barley","Chrysanthemum","Wheat","Soybean","Other"), x=c(2000,1975,1975,1980,1994,1985), y=c(40,30,7,-5,-12,-40)), aes(x,y, label=label), inherit.aes = FALSE, size=3, hjust=0, color=c("white","black","black","white","white","black"), family=f1) +
  annotate(geom="text",x=1987.5, y=-45, label="(238 genera)", size=2.2, family=f1) +
  scale_fill_manual(values=c("#106EA1","#32C0D2","#E0B165","#00969D","#953C4C","#C8D6DC")) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.01)))+
  cowplot::theme_minimal_grid(9.5) +
  theme(text=element_text(family=f1),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        axis.title.x=element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust=0,size=8.5, color="grey30"),
        plot.title.position = "plot",
        plot.title=element_text(size=15, margin=ggplot2::margin(b=8)),
        plot.subtitle=element_text(size=8.8,lineheight = 1.2, color="grey10", margin=ggplot2::margin(b=5)),
        plot.margin=ggplot2::margin(.5,.75,.5,.5, unit="cm")
        ) +
  labs(y="<- Number of mutant variety registered ->",
       title="Atomic Gardening",
       subtitle="According to the Mutant Variety Database, from 1929 to 2022, a total of 3 402 mutant varieties were registered from 85 countries.\n1985 recorded the highest number of registrations at 152 varieties from 22 countries. ",
       caption="Source: International Atomic Energy Agency and the UN's Food and Agriculture Organization") 
       
ggsave("p1.png", height=6, width=8, bg="#FEFAF1")     

# Plot 2   
data2 = mutant %>% 
  filter(between(registration,1960,2021)) %>%
  drop_na(usage_subcategory) %>%
  mutate(subcat=fct_lump(usage_subcategory,9),
         subcat=fct_infreq(subcat),
         subcat=fct_relevel(subcat,"Other",after=Inf)
         )
         
data2 %>%  
  count(subcat,registration) %>%
  ggplot(aes(registration, n, fill=subcat)) +
  geom_segment(data=tibble(x = seq(1960,2020,20),y = -85, yend = 85),aes(x=x, xend=x, y=y, yend=yend), colour = "grey10",size = 0.3, inherit.aes = FALSE, linetype="dotted") +
  ggstream::geom_stream(bw=.5, alpha=.95) +
  geom_text(data=tibble(x = seq(1960,2020,20),y = 85),aes(x=x, y=y, label=x), inherit.aes = FALSE,angle=90, size=3.2, vjust=-.5, hjust=1, family=f3) +
  scale_fill_manual(values=c("#039176","#B83229","#FE8902","#16607A","#FFCD74",
                             "#00DCA6","#88F5B9","#1AA1CD","#FE483B","#A9C2BB")) +
  cowplot::theme_minimal_grid(11) +
  theme(text=element_text(family=f3),
        plot.background=element_rect(fill="white", color=NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x=element_blank(),
        axis.text=element_blank(),
        axis.title.y=element_text(size=10.5,margin=ggplot2::margin(r=-3)),
        plot.title.position = "plot",
        plot.title=element_text(size=14, family=f2),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust=0, color="grey30", margin=ggplot2::margin(t=-5)),
        plot.margin=ggplot2::margin(.4,.4,.4,.4, unit="cm"),
        plot.subtitle=element_text(size=9.5, color="grey10"),
        legend.position = "top",
        legend.title = element_text(size=9.5),
        legend.text = element_text(size=9.3),
        legend.margin=ggplot2::margin(t=3, b=-9),
        #legend.direction = "vertical"
        ) +
  labs(y="Number of mutant variety registered",
       fill="Usage subcategory:",
       title="Atomic Gardening",
       subtitle="Number of mutant varieties by registered year and usage subcategory, from 1960 to 2021",
       caption="Source: International Atomic Energy Agency and the UN's Food and Agriculture Organization") +
  guides(fill=guide_legend(byrow=TRUE, title.position = "top")) +
  annotate(geom="segment", x=1985, xend=2002, y=57,yend=57, color="grey30", size=.3) +
  annotate(geom="richtext",x=2010, y=57, label="**152** mutant variety registrations<br>recorded in **1985**, including<br><span style='color:#B83229'>**60 flower**</span>, <span style='color:#039176'>**47 cereal**</span> and <span style='color:#FE8902'>**22<br>legume and pulses**</span> varieties.", size=2.8, family=f3, label.color = "grey30") +
  annotate(geom="segment", x=1985, xend=1985, y=-85, yend=85, color="white", linetype="dotted") 
  
ggsave("p2.png", height=7, width=7, bg="white")           