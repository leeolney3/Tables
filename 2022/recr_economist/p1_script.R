# Recreating The Economist: Third time's the charm chart
# Data: https://www.medrxiv.org/content/10.1101/2022.03.22.22272769v1.full-text, https://www.medrxiv.org/content/medrxiv/early/2022/03/22/2022.03.22.22272769/T2/graphic-4.large.jpg
# Original plot: https://blog.datawrapper.de/wp-content/uploads/2022/04/image24-3.png

library(tidyverse)
library(ggtext)
library(ggnewscale)

# Data
x1 = tribble(
  ~fw,~grp, ~y1, ~age_group,~x, ~xmin, ~xmax,
  "One dose","BioNTech",4,"20-59",85.0,69.1,92.7,
  "One dose","Sinovac",4,"20-59",60.9,40.6,74.3,
  "One dose","BioNTech",3,"60-69",59.9,29.3,77.3,
  "One dose","Sinovac",3,"60-69",55.1,30.9,70.9,
  "One dose","BioNTech",2,"70-79",71.5,48.9,84.1,
  "One dose","Sinovac",2,"70-79",33.9,8.1,52.5,
  "One dose","BioNTech",1,"80+",65,42.2,78.8,
  "One dose","Sinovac",1,"80+",35.0,8.8,53.7,
  "Two doses","BioNTech",4,"20-59",95.2,92.9,96.8,
  "Two doses","Sinovac",4,"20-59",91.7,87.8,94.4,
  "Two doses","BioNTech",3,"60-69",91.1,85.4,94.6,
  "Two doses","Sinovac",3,"60-69",82.6,74.2,88.2,
  "Two doses","BioNTech",2,"70-79",89.4,83.0,93.3,
  "Two doses","Sinovac",2,"70-79",80.8,72.8,86.5,
  "Two doses","BioNTech",1,"80+",84.5,75.5,90.2,
  "Two doses","Sinovac",1,"80+",60.2,43.9,71.8,
  "Three doses","BioNTech",4,"20-59",98.5,95.9,99.4,
  "Three doses","Sinovac",4,"20-59",98.5,95.2,99.5,
  "Three doses","BioNTech",3,"60-69",99.2,96.7, 99.8,
  "Three doses","Sinovac",3,"60-69",98.5,95.3,99.6,
  "Three doses","BioNTech",2,"70-79",99.5,96.0,99.9,
  "Three doses","Sinovac",2,"70-79",96.7,92.3,98.6,
  "Three doses","BioNTech",1,"80+",95.7,89.0,98.3,
  "Three doses","Sinovac",1,"80+",98.6,94.3,99.7
)

# Labels df
lab2 = x1 %>% mutate(id=row_number()) %>% filter(id==2) %>% 
  mutate(lab ="95% confidence interval")

lab1 = x1 %>% mutate(id=row_number()) %>% filter(id==1) %>% 
  mutate(lab ="Central estimate")
  
# Plot
p = x1 %>%
  mutate(y =case_when(grp=="Sinovac"~y1+.35, TRUE~y1+.6),
         ylab = y1+.5) %>%
  ggplot() +
  geom_vline(xintercept=0) +
  geom_segment(aes(x=xmin, xend=xmax, y=y, yend=y, color=grp),
               size=2, show.legend = F) +
  scale_color_manual(values=c("#7A9EC2","#FFAAA7")) +
  ggnewscale::new_scale_color() +
  geom_point(aes(x=x, y=y, color=grp), size=2.5) +
  scale_color_manual(values=c("#1C2C74","#E3120C")) +
  geom_text(aes(x=-10, y=ylab, label=age_group), hjust=0) +
  geom_richtext(data=lab1, aes(x=41, y=y1+.7,label=lab) ,hjust=0, color="#4C4C4C",
                label.color=NA,label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(data=lab2, aes(x=13, y=y1,label=lab) ,hjust=0, color="#4C4C4C",
                label.color=NA,label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_curve(data=lab2, aes(x=57.8, xend=x, y=y1+.7, yend=y1+.5), 
             color="#4C4C4C", curvature = -.25) +
  geom_curve(data=lab2, aes(x=37.5, xend=42, y=y1, yend=y1+.3), 
             color="#4C4C4C",curvature = .25) +
  scale_y_continuous(limits=c(1,5),expand = expansion(mult = c(0, .08))) +
  scale_x_continuous(limits=c(-10,100), breaks=seq(0,100,20), 
                     position="top", expand=c(0,0)) +
  facet_wrap(~factor(fw, levels=c("One dose","Two doses","Three doses")), 
             ncol=1, scales="free") +
  cowplot::theme_minimal_grid() +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.margin=margin(b=11, l=-5),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        strip.text.x=element_text(hjust=0, face="bold", margin=margin()),
        plot.title=element_text(size=14),
        plot.subtitle=element_text(size=12.25, margin=margin(b=10)),
        strip.placement = "outside",
        plot.margin=margin(.4,.5,.1,.1, unit="cm"),
        panel.spacing = unit(1.8, "lines"),
        plot.caption = element_text(hjust=0, margin=margin(t=15), 
                                    size=10, color="#818181")
        ) +
  labs(caption="Source: 'Vaccine effectiveness of two and three doses of BNT162b2 and CoronaVac\nagainst COVID-19 in Hong Kong', by M.E. McMenamin et al., 2022 (preprint)",
       title="Recreating The Economist: Third time's the charm chart",
       subtitle="Covid-19, estimated vaccine effectiveness against severe or fatal disease, by dosage and age group, %")
       
# Add tag
png("p1.png", width=8, height=9,unit='in',res=300)

# function from https://stackoverflow.com/questions/64656234/how-does-the-economist-make-these-lines-near-the-title-using-using-ggplot
annotate_npc <- function(x, y, height, width, ...) {
  grid::grid.draw(grid::rectGrob(
    x = unit(x, "npc"), y = unit(y, "npc"), height = unit(height, "npc"), width = unit(width, "npc"),
    gp = grid::gpar(...)
  ))
}

p
annotate_npc(x = 0.04, y = 1, height = 0.015, width = 0.06, fill = "#D8232A", col = NA)
annotate_npc(x = 0.031, y = .865, height = 0.002, width = 0.04, fill = "#484747", col = NA)
annotate_npc(x = 0.031, y = .585, height = 0.002, width = 0.04, fill = "#484747", col = NA)
annotate_npc(x = 0.031, y = .307, height = 0.002, width = 0.04, fill = "#484747", col = NA)

dev.off()
