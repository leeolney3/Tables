# data
library(tidyverse)
df = expand.grid(year=seq(2020,2021,1), 
            grp=c("Labour market services","Active labour market measures"),
            type=c("Budget increased","Budget decreased","No change",
                   "Under discussion","Not yet known/too early to respond")) 
df1 = df %>% arrange(year,grp) %>%
  add_column(value= c(65,3,24,0,8,
               73,5,13,0,9,
               52,3,15,12,18,
               52,9,4,15,20)) %>%
  mutate(value=value/100) %>%
  filter(value>0) 

# font
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
font_add_google("Roboto Condensed")
font_add_google("Barlow Condensed","barlow")
f2 = "Roboto Condensed"
f3 = "barlow"


# slope chart
df1 %>% 
  #mutate(year=ifelse(year==2021,"B. 2021","A. 2020")) %>%
  group_by(year,grp) %>%
  ggplot(aes(x=grp, y=value,color=type)) +
  geom_line(aes(group=type), show.legend = F, size=1, alpha=.8) +
  geom_point(size=3.7, shape=15) +
  geom_text(data=df1 %>% filter(grp=="Labour market services"), aes(label=scales::percent(value, accuracy=1)),size=3.3, nudge_x = -0.17, fontface="bold") +
  geom_text(data=df1 %>% filter(grp!="Labour market services"), aes(label=scales::percent(value, accuracy=1)),size=3.3, nudge_x = 0.17,fontface="bold") +
  geom_hline(yintercept = 0) +
  facet_wrap(~year, ncol=2) +
  scale_color_manual(values=c("#980000","#1E327E","#457E28","#B05890","#ee9b00")) +
  scale_x_discrete(position="top", expand = c(.25,.25)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_grid(13.5) +
  theme(legend.position = "top",
        legend.spacing.y = unit(0.1, 'cm'),
        legend.justification = "center",
        strip.placement = "outside",
        text=element_text(family=f3),
        panel.grid.major.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(family=f3, size=11.7),
        panel.spacing = unit(2, "lines"),
        strip.text=element_text(size=11, face="bold", family=f2),
        plot.margin=margin(.5,1.5,.75,.5,unit="cm"),
        legend.margin=margin(l=-11),
        legend.text=element_text(color="grey10"),
        plot.title.position = "plot",
        plot.title=element_text(size=12, face="plain", lineheight = 1.2, family=f2)
        ) +
  labs(color=NULL,
       title="Public expenditure budget allocation for labour market servies and active labour market measures, percentage\nof countries by type of action")