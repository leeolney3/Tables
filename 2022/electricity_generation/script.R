# Electricity Generation, world, 1985-2021
# Data source: Our World In Data, https://ourworldindata.org/explorers/energy?facet=none&country=USA~GBR~CHN~OWID_WRL~IND~BRA~ZAF&Total+or+Breakdown=Select+a+source&Select+a+source=Fossil+fuels&Energy+or+Electricity=Electricity+only&Metric=Annual+generation


# Libraries 
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Ubuntu")
f1 = "Ubuntu"

# Data
e1 = read_csv("energy/energy(1).csv") %>% filter(Entity=="World") %>% select(3:4)
e2 = read_csv("energy/energy(2).csv") %>% filter(Entity=="World") %>% select(3:4)
e3 = read_csv("energy/energy(3).csv") %>% filter(Entity=="World") %>% select(3:4)
e4 = read_csv("energy/energy(4).csv") %>% filter(Entity=="World") %>% select(3:4)
e5 = read_csv("energy/energy(5).csv") %>% filter(Entity=="World") %>% select(3:4)
e6 = read_csv("energy/energy(6).csv") %>% filter(Entity=="World") %>% select(3:4)
e7 = read_csv("energy/energy(7).csv") %>% filter(Entity=="World") %>% select(3:4)
e8 = read_csv("energy/energy(8).csv") %>% filter(Entity=="World") %>% select(3:4)
e9 = read_csv("energy/energy(9).csv") %>% filter(Entity=="World") %>% select(3:4)
e10 = read_csv("energy/energy(10).csv") %>% filter(Entity=="World") %>% select(3:4)

df = list(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10) %>% reduce(full_join, by="Year")

df1 = df %>% pivot_longer(!Year) %>%
  mutate(lab= str_to_lower(name),
         lab = str_remove_all(lab," \\s*\\([^\\)]+\\)"),
         lab=str_remove_all(lab, "from"),
         lab=str_remove_all(lab, "electricity"),
         lab=str_to_title(str_trim(lab))) %>%
  filter(Year>=1985) %>%
  filter(lab %in% c("Coal","Oil","Gas","Nuclear","Renewables"))
  
lev = df1 %>% filter(Year==max(Year)) %>% arrange(value) %>% pull(lab)

# Plot
df1 %>%
  mutate(lab=factor(lab, level=lev)) %>%
  filter(lab %in% c("Fossil Fuels","Coal","Oil","Gas","Nuclear","Renewables")) %>%
  ggplot(aes(x=Year, y=value, alluvium = lab)) +
  geom_segment(data = tibble(y = seq(0, 10000, by = 5000), x1 = 1984, x2 = 2022),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,color = "grey91",size = .6) +
  geom_segment(data = tibble(y = seq(15000,25000,5000), x1 = c(1995,2005,2013), x2 = 2022),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,color = "grey91",size = .6) +
  ggalluvial:: geom_alluvium(aes(fill = lab, colour = lab),
                alpha = .75, decreasing = FALSE) +
  scale_y_continuous(position="right", expand=c(.0,.0), labels=scales::comma,
                     breaks=seq(0,25000,5000)) +
  annotate(geom="text", label=c("Coal","Renewables","Gas","Nuclear","Oil"), 
           x=rep(2014,5), y=c(19000,12000,6000,2300,500), color=c("white","white","white","black","white"),
           family=f1, size=4.5, hjust=0) +
  scale_x_continuous(breaks=seq(1990,2021,10), expand=c(0,0)) +
  scale_fill_manual(values=c("#5D326E","#FFCB00","#C13502","#007F18","#052E81")) +
  scale_color_manual(values=c("#5D326E","#FFCB00","#C13502","#007F18","#052E81"))  +
  theme_minimal(14) +
  theme(text=element_text(family=f1),
        legend.position = "none",
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.line.x = element_line(size=.5),
        axis.ticks.length.x=unit(.25, "cm"),
        axis.ticks.x=element_line(size=.5),
        plot.margin=margin(.5,.5,.5,.5, unit="cm")
        ) +
  annotate(geom="text", x=1985, y=27500, hjust=0, vjust=1,family=f1, size=8,
           label="Electricity Generation") +
  annotate(geom="text", x=1985, y=25700, hjust=0, vjust=1,family=f1, size=3.8, lineheight=1,
           label="in TWh, world, from 1985 to 2021\nRenewable sources include hydropower, solar,\nwind, geothermal, bioenergy, wave and tidal.") +
  annotate(geom="text", x=1985, y=22700, hjust=0, vjust=1,family=f1, size=3.2,color="grey30",
           label="Source: Our World in Data, based on BP Statistical\nReview of World Energy & Ember")
           
ggsave("eg.png",height=7,width=7, bg="white")

