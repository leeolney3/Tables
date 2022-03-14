# Global innovation index 2021
# Data source: WIPO
# Plot inspired by Washington Post <https://www.washingtonpost.com/sports/olympics/2022/02/22/olympic-winter-sport-dominance/>

library(tidyverse)
library(ggtext)

df = read_csv("bubble_fpd/bubble_fpd.csv")

# wrangle
dfr = df %>% filter(GIIYR==2021) %>%
  select(STATUS,NAME, RANK, GDP, INCOME) %>%
  mutate(GDPRANK = dense_rank(desc(GDP))) %>%
  select(STATUS,Economy=NAME, GII=RANK, GDP=GDPRANK, INCOME) %>%
  mutate(STATUS = factor(STATUS,
                         levels=c("Innovation leader","Achiever","At expectations","Below expectations"))) %>%
  mutate(INCOME= factor(INCOME, levels=c("High income","Upper-middle","Lower middle","Low income"))) %>%
  pivot_longer(GII:GDP) %>%
  mutate(x=case_when(name=="GDP"~1,name=="GII"~2))
  
# lines df
linedf = tribble(
  ~x, ~xend, ~y, ~yend,
  .5,2,1,1,
  .5,2,50,50,
  .5,2,100,100,
  .5,2,130,132,
)

# arrows df
dfarrow = dfr %>% filter(name=="GII") %>%
  filter(Economy=="Switzerland"| Economy=="Viet Nam"| Economy=="Rwanda") %>%
  select(Economy, name, value, x)
  
# plot slope chart
p1 = dfr %>%
  ggplot(aes(x=x, y=value)) +
  geom_segment(data=linedf,
               aes(x=x, xend=xend, y=y, yend=yend), size=.3, color="grey") +
  geom_line(aes(group=Economy, color=INCOME), size=.3, alpha=.7) +
  geom_point(aes(color=INCOME), alpha=.5, size=.5) +
  scale_y_reverse(breaks=c(1,50,100,130), labels=c("<b>Rank: 1st</b>","50th","100th","130th"),
                  limits=c(140,1), expand=c(.025,.025)) +
  scale_x_continuous(breaks=c(1,2), labels=c("GDP","GII"), position="top",
                     expand = expansion(mult = c(0, .5))) +
  scale_color_manual(values=c("#1F8DA6","#A378BF","#3F5D41","#F28F16")) +
  geom_text(data=dfr %>% filter(name=="GII", value==1), 
            aes(x=2.1,label="Switzerland has the\nhighest GII score (65.48)"),
            size=2.5, hjust=0, lineheight=1) +
  geom_text(data=dfr %>% filter(name=="GDP", value==1), 
            aes(x=0.6, label="Luxembourg has\nthe highest GDP\n(112875.12)"),
            size=2.5, hjust=0, vjust=1, lineheight=1) +
  geom_text(data=dfr %>% filter(name=="GII", Economy=="Viet Nam"), 
            aes(x=2.1, label="Viet Nam has the highest\nGII score in the lower\nmiddle income group"),
            size=2.5, hjust=0, lineheight=1) +
  geom_text(data=dfr %>% filter(name=="GII", Economy=="Rwanda"), 
            aes(x=2.1, label="Rwanda has the\nhighest GII score in the\nlow income group"),
            size=2.5, hjust=0, lineheight=1) +
  geom_segment(data=dfarrow,
               aes(x=2.09, xend=2.01, y=value, yend=value), 
               size=.3, color="grey30", arrow=arrow(length=unit(.15,"cm"))) +
  annotate(geom="curve", x=.89, xend=.99, y=5, yend=1, curvature=.2, size=.3,
           arrow=arrow(length=unit(.15,"cm"))) +
  cowplot::theme_minimal_grid(10) +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title.x.top  =element_blank(),
        axis.text.x.top=element_text(margin=margin(b=0,t=9), face="bold"),
        axis.title.y=element_blank(),
        plot.margin=margin(.3,0,.3,.3,unit="cm"),
        axis.text.y=element_markdown())
        
# y axis labels
dfr_lab = dfr %>% filter(name=="GII") %>%
  count(STATUS) %>%
  mutate(lab = glue::glue("**{STATUS}**<br>(n={n})"))  %>%
  select(STATUS, lab)
  
# pct bar data
dfr2 = dfr %>% filter(name=="GII") %>%
  count(INCOME, STATUS) %>%
  group_by(STATUS) %>%
  mutate(pct1 = n/sum(n)) %>%
  left_join(dfr_lab, by="STATUS") %>%
  mutate(lab= factor(lab, levels=c("**Innovation leader**<br>(n=25)","**Achiever**<br>(n=19)",
                                   "**At expectations**<br>(n=57)","**Below expectations**<br>(n=31)")))

# color labels
dfr3 = dfr2 %>% filter(lab=="**Below expectations**<br>(n=31)") %>%
  mutate(INCOME_lab=case_when(INCOME=="High income"~"High\nincome",
                          INCOME=="Upper-middle"~"Upper\nmiddle",
                          INCOME=="Lower middle"~"Lower\nmiddle",
                          INCOME=="Low income"~"Low\nincome"))

# plot pct bar                         
p2 = dfr2 %>%
  ggplot(aes(x=pct1, y=lab, fill=INCOME)) +
  geom_col(width=.4, color="white", size=1) +
  geom_text(aes(label=scales::percent(pct1, accuracy=.1,drop0trailing=T)),position = position_stack(vjust = .5), 
            size=2.5, vjust=-2) +
  geom_text(data=dfr3, 
            aes(label=INCOME_lab, color=INCOME),
            position = position_stack(vjust = .5),
            size=3, fontface="bold", vjust=-1.2
            ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0, .4))) +
  scale_fill_manual(values=c("#1F8DA6","#A378BF","#3F5D41","#F28F16")) +
  scale_color_manual(values=c("#1F8DA6","#A378BF","#3F5D41","#F28F16")) +
  cowplot::theme_minimal_grid(9) +
  theme(legend.position="none",
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(size=20),
        axis.text.y=element_markdown(margin=margin(r=5), lineheight = 1.2),
        axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=8, hjust=.4, margin=margin(r=5)),
        plot.title.position = "plot",
        plot.margin=margin(.3,.75,.3,.3,unit="cm")
        ) +
  guides(fill=guide_legend(reverse=T)) +
  labs(title="2021", fill="World Bank classifcations",
       x="Percentage of economies by income group", y="GII status") 
       
       
# combine pct bar and slope chart
# fig.height=4.5, fig.width=3.5
cowplot::plot_grid(p2,p1, ncol=1, rel_heights = c(1,2)) +
  cowplot::draw_line(x=c(.2,.44), y=c(.685,.685), color="grey50", size=.2) +
  cowplot::draw_line(x=c(.715,.956), y=c(.685,.685), color="grey50", size=.2) +
  cowplot::draw_line(x=c(.2,.2), y=c(.68,.69), color="grey50", size=.2) +
  cowplot::draw_line(x=c(.956,.956), y=c(.68,.69), color="grey50", size=.2)
  
# save
ggsave("wipo_wp.png", height=9, width=7, unit='in', bg="white")


# V2_1103
# combine the "Innovation leader" category under "Achiever"
# highlighting one economy, strengthen its line while fading those for all the other economies

library(tidyverse)
df = read_csv("bubble_fpd/bubble_fpd.csv")
library(ggtext)

dfr = df %>% filter(GIIYR==2021) %>%
  select(STATUS,NAME, RANK, GDP, INCOME) %>%
  mutate(GDPRANK = dense_rank(desc(GDP))) %>%
  select(STATUS,Economy=NAME, GII=RANK, GDP=GDPRANK, INCOME) %>%
  mutate(STATUS = factor(STATUS,
                         levels=c("Innovation leader","Achiever","At expectations","Below expectations"))) %>%
  mutate(INCOME= factor(INCOME, levels=c("High income","Upper-middle","Lower middle","Low income"))) %>%
  pivot_longer(GII:GDP) %>%
  mutate(x=case_when(name=="GDP"~1,name=="GII"~2))

linedf = tribble(
  ~x, ~xend, ~y, ~yend,
  .5,2,1,1,
  .5,2,50,50,
  .5,2,100,100,
  .5,2,130,132,
)

dfarrow = dfr %>% filter(name=="GII") %>%
  filter(Economy=="Switzerland"| Economy=="Viet Nam"| Economy=="Rwanda") %>%
  select(Economy, name, value, x)
dfr_highlight = dfr %>% filter(Economy %in% c("Viet Nam"))
dfr_others =  dfr %>% filter(!Economy %in% c("Viet Nam"))

p1 = dfr %>%
  ggplot(aes(x=x, y=value)) +
  geom_segment(data=linedf,
               aes(x=x, xend=xend, y=y, yend=yend), size=.3, color="grey") +
  geom_line(data=dfr_highlight, aes(group=Economy, color=INCOME), size=1, alpha=.8) +
  geom_line(data=dfr_others, aes(group=Economy, color=INCOME), size=.3, alpha=.4) +
  geom_point(aes(color=INCOME), alpha=.5, size=.5) +
  scale_y_reverse(breaks=c(1,50,100,130), labels=c("<b>Rank: 1st</b>","50th","100th","130th"),
                  limits=c(140,1), expand=c(.025,.025)) +
  scale_x_continuous(breaks=c(1,2), labels=c("GDP","GII"), position="top",
                     expand = expansion(mult = c(0, .5))) +
  scale_color_manual(values=c("#1F8DA6","#A378BF","#3F5D41","#F28F16")) +
  geom_text(data=dfr %>% filter(name=="GII", value==1), 
            aes(x=2.1,label="Switzerland has the\nhighest GII score (65.48)"),
            size=2.5, hjust=0, lineheight=1) +
  geom_text(data=dfr %>% filter(name=="GDP", value==1), 
            aes(x=0.6, label="Luxembourg has\nthe highest GDP\n(112875.12)"),
            size=2.5, hjust=0, vjust=1, lineheight=1) +
  geom_text(data=dfr %>% filter(name=="GII", Economy=="Viet Nam"), 
            aes(x=2.1, label="Viet Nam has the highest\nGII score in the lower\nmiddle income group"),
            size=2.5, hjust=0, lineheight=1) +
  geom_text(data=dfr %>% filter(name=="GII", Economy=="Rwanda"), 
            aes(x=2.1, label="Rwanda has the\nhighest GII score in the\nlow income group"),
            size=2.5, hjust=0, lineheight=1) +
  geom_segment(data=dfarrow,
               aes(x=2.09, xend=2.01, y=value, yend=value), 
               size=.3, color="grey30", arrow=arrow(length=unit(.15,"cm"))) +
  annotate(geom="curve", x=.89, xend=.99, y=5, yend=1, curvature=.2, size=.3,
           arrow=arrow(length=unit(.15,"cm"))) +
  cowplot::theme_minimal_grid(10) +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title.x.top  =element_blank(),
        axis.text.x.top=element_text(margin=margin(b=0,t=9), face="bold"),
        axis.title.y=element_blank(),
        plot.margin=margin(.3,0,.3,.3,unit="cm"),
        axis.text.y=element_markdown())



dfr_lab = dfr %>% filter(name=="GII") %>%
  mutate(STATUS= as.character(STATUS),
         STATUS= case_when(STATUS=="Innovation leader"~"Achiever", TRUE~STATUS)) %>%
  count(STATUS) %>%
  mutate(lab = glue::glue("**{STATUS}**<br>(n={n})"),
         STATUS= as.factor(STATUS)) %>%
  select(STATUS, lab)

dfr2 = dfr %>% filter(name=="GII") %>%
  mutate(STATUS= as.character(STATUS),
         STATUS= case_when(STATUS=="Innovation leader"~"Achiever", TRUE~STATUS)) %>%
  count(INCOME, STATUS) %>%
  group_by(STATUS) %>%
  mutate(pct1 = n/sum(n)) %>%
  left_join(dfr_lab, by="STATUS") %>%
  mutate(lab= factor(lab, levels=c("**Achiever**<br>(n=44)",
                                   "**At expectations**<br>(n=57)","**Below expectations**<br>(n=31)")))

dfr3 = dfr2 %>% filter(lab=="**Below expectations**<br>(n=31)") %>%
  mutate(INCOME_lab=case_when(INCOME=="High income"~"High\nincome",
                              INCOME=="Upper-middle"~"Upper\nmiddle",
                              INCOME=="Lower middle"~"Lower\nmiddle",
                              INCOME=="Low income"~"Low\nincome"))

p2 = dfr2 %>%
  ggplot(aes(x=pct1, y=lab, fill=INCOME)) +
  geom_col(width=.3, color="white", size=.7) +
  geom_text(aes(label=scales::percent(pct1, accuracy=.1,drop0trailing=T)),position = position_stack(vjust = .5), 
            size=2.5, vjust=-2.5) +
  geom_text(data=dfr3, 
            aes(label=INCOME_lab, color=INCOME),
            position = position_stack(vjust = .5),
            size=3, fontface="bold", vjust=-1.3
  ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0, .4))) +
  scale_fill_manual(values=c("#1F8DA6","#A378BF","#3F5D41","#F28F16")) +
  scale_color_manual(values=c("#1F8DA6","#A378BF","#3F5D41","#F28F16")) +
  cowplot::theme_minimal_grid(9) +
  theme(legend.position="none",
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(size=20),
        axis.text.y=element_markdown(margin=margin(r=5), lineheight = 1.2),
        axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=8, hjust=.4, margin=margin(r=5)),
        plot.title.position = "plot",
        plot.margin=margin(.3,.75,.3,.3,unit="cm")
  ) +
  guides(fill=guide_legend(reverse=T)) +
  labs(title="2021", fill="World Bank classifcations",
       x="Percentage of economies by income group", y="GII status") 

# final plot v2 (fig.height=4.5, fig.width=3.5)
cowplot::plot_grid(p2,p1, ncol=1, rel_heights = c(1,2)) +
  cowplot::draw_line(x=c(.206,.44), y=c(.685,.685), color="grey50", size=.2) +
  cowplot::draw_line(x=c(.715,.955), y=c(.685,.685), color="grey50", size=.2) +
  cowplot::draw_line(x=c(.206,.206), y=c(.68,.69), color="grey50", size=.2) +
  cowplot::draw_line(x=c(.955,.955), y=c(.68,.69), color="grey50", size=.2)

