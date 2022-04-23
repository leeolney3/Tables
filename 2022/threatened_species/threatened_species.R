# Estimated % of threatened species in 2021 (Mammals, Birds, Reptiles, Amphibians, Horseshoe Crabs, Gymnosperms)
# Data source: iucnredlist.org

library(tidyverse)
library(ggforce)
library(scales)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google(name = "Dosis", family = "dosis")
f1 = "dosis"

# Data
x = tribble(
  ~class, ~ext1,
  "Mammals",26,
  "Birds",13,
  "Reptiles",21,
  "Amphibians",41,
  "Horseshoe Crabs",100,
  "Gymnosperms", 41
) %>%
  mutate(ext0 = 100-ext1) %>%
  pivot_longer(!class)

# Wrangle  
x1 = x %>% 
  mutate(class=str_to_title(class)) %>%
  group_by(class) %>%
  mutate(name=factor(name),
         share= value/sum(value),
         ymax = cumsum(share),
         ymin = c(0, head(ymax, n= -1))
         ) %>%
  mutate_at(vars(starts_with("y")), rescale, to=pi*c(-.5,.5), from=0:1) 
  
# Plot
x1 %>%  
  ggplot + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .5, r = 1, start = ymin, end = ymax, fill=name), show.legend = F) + 
  geom_text(data= x1 %>% filter(name=="ext1"),aes(x=-.75, y=.15, label=percent(share, accuracy = 1)),
            color="white", fontface="bold", family=f1) +
  scale_fill_manual(values=c("white","#9d0208")) +
  coord_fixed() +
  facet_wrap(~class, ncol=3) +
  cowplot::theme_map(13) +
  theme(panel.spacing = unit(2.5, "lines"),
        strip.text=element_text(face="bold"),
        text=element_text(family=f1),
        plot.title=element_markdown(hjust=.5),
        plot.subtitle = element_text(hjust=.5, margin=margin(b=15), size=9.5),
        plot.caption=element_text(size=9, hjust=0, lineheight = 1.2),
        plot.margin=margin(.5,.5,.5,.5, unit="cm")) +
  labs(title="Estimated % of <span style='color:#9d0208'>**threatened species**</span> in 2021",
       subtitle="Threatened spp. as % of extant data sufficient evaluated species, according to IUCN Red List version 2021-3",
       caption="\nNote: Threatened species are those listed as Critically Endangered (CR), Endangered (EN) or Vulnerable (VU)\nData source: iucnredlist.org")
       
# Save plot
ggsave("threatened_species.png", width=7, height=4.5, bg="white")


