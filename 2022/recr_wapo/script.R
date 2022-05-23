# 2022-05-23
# Recreating The Washington Post - US military aid to Ukraine chart (May 19 2022)
# Article link: https://www.washingtonpost.com/world/interactive/2022/biden-ukraine-military-aid-us/

# Libraries
library(tidyverse)
library(ggtext)


# Data
x = tribble(
  ~y, ~x, 
  .35,as.Date("2022-02-25"),
  .2,as.Date("2022-03-12"),
  .8,as.Date("2022-03-16"),
  .3,as.Date("2022-04-02"),
  .1,as.Date("2022-04-05"),
  .8,as.Date("2022-04-13"),
  .8,as.Date("2022-04-21"),
  .322,as.Date("2022-04-25"),
  .15,as.Date("2022-05-06"),
  .08,as.Date("2022-05-19"),
) %>%
  mutate(xmax=as.Date("2022-05-19"),
         ymax=cumsum(y),
         ymin=lag(ymax),
         ymin= replace_na(ymin,0))
         
# Plot
x %>% 
  ggplot(aes(x=x, y=y)) +
  # axis lines
  geom_segment(
    data = tibble(y = seq(0, 4, by = 1), x1 = as.Date("2022-01-20"), x2 =as.Date("2022-05-30")),aes(x = x1, xend = x2, y = y, yend = y),inherit.aes= FALSE,color= "grey91",size = .3) + 
  # steps
  geom_rect(aes(xmin= x, xmax=xmax, ymin=ymin, ymax=ymax),fill="grey", alpha=.3) +
  geom_segment(aes(x=x, xend=xmax, y=ymax, yend=ymax), size=.3) +
  geom_segment(aes(x=x, xend=x, y=ymin, yend=ymax), size=1) +
  geom_rect(inherit.aes = F, aes(xmin=as.Date("2022-01-30"), xmax=as.Date("2022-02-03"),
                                 ymin=0, ymax=2.7)) +
  
  # annotations
  annotate(geom="richtext", x=as.Date("2022-02-24"), y=4, label="Feb.24<br>**Invasion<br>starts**", hjust=0, vjust=.65, color="grey50", label.color=NA, size=3, label.padding = grid::unit(rep(2, 4), "pt")) +
  annotate(geom="richtext", x=as.Date("2022-04-02"), y=4, label="Apr.2<br>**Russia pushed<br>back from Kyiv**", hjust=0, vjust=.65, color="grey50", label.color=NA, size=3, label.padding = grid::unit(rep(2, 4), "pt")) +
  geom_vline(xintercept = c(as.Date("2022-02-24"),as.Date("2022-04-02")), color="#A09C99", linetype=c("solid","dashed"), size=.3) +
  annotate(geom="curve", x=as.Date("2022-02-14"), xend=as.Date("2022-02-01"), y=3.2, yend=2.75, size=.3, curvature = 0.4, arrow = arrow(length = unit(0.15, "cm"))) +
  annotate(geom="richtext", x=as.Date("2022-02-14"), y=3.3, label="Between 2014 \u2013 when<br> Russia annexed Crimea \u2013<br>and 2021, the United States<br>sent just <span style='color:#BE2C25'>$2.7 billion</span> in<br>military aid to Ukraine", hjust=0, vjust=1, label.color=NA, size=3.5)  +
  # annotate rhs
  annotate(geom="segment", x= as.Date("2022-05-21"), xend=as.Date("2022-05-21"), y=0, yend=3.9, size=.3) +
  annotate(geom="segment",x=as.Date("2022-05-21"), xend=as.Date("2022-05-23"), y=3.9, yend=3.9, size=.3) +
  annotate(geom="richtext", x=as.Date("2022-05-23"), label="Since the war's start<br>the United States<br>has pledged a total<br>of <span style='color:#BE2C25'>$3.9 billion</span><br>in military aid<br>to Ukraine",y=3.45, hjust=0, label.color=NA, size=3.5) +
  # scales 
  scale_x_date(breaks=as.Date(c("2022-02-01","2022-03-01","2022-04-01","2022-05-01")),
               labels=c("2014-2021","March","April","May"), expand=c(0,0), limits=c(as.Date("2022-01-20"), as.Date("2022-06-25"))) +
  scale_y_continuous(expand = expansion(mult = c(0, .03)),limits=c(0,4),
                     breaks=seq(0,4,1), labels=c("0","1","2","3","$4B")) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_grid(10, line_size = .3, color="grey80") +
  theme(axis.title=element_blank(),
        axis.text=element_text(color="grey50"),
        panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(margin=margin(t=2)),
        axis.ticks.length.x = unit(.2, unit="cm"),
        axis.ticks.x=element_line(color="#9C9895", size=.3),
        plot.caption=element_text(color="grey50")) +
  labs(caption="Source and original chart: The Washington Post")
  
ggsave("recr_wp.png", height=4, width=7, dpi=300, bg="white")