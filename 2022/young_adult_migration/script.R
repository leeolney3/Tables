# Young adults migration
# Data from migrationpatterns.org by way of [Data is Plural]https://www.data-is-plural.com/archive/2022-08-10-edition/

# Load libraries
library(tidyverse)
library(sf)
library(ussf)
library(ggtext)
library(ggnewscale)
library(ggsci)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load font
font_add_google("Inter")
f1 = "Inter"

# Import data
od_inc = readr::read_csv("MigrationPatternsData/od_inc.csv")
od_pooled = readr::read_csv("MigrationPatternsData/od_pooled.csv")

# Get communiting zone shp from {ussf}
sf = ussf::boundaries(
  geography = c("cz"),
)

# Bind pooled and income data then join sf
df1= od_pooled %>% filter(o_cz==38300) %>% mutate(pool=fct_recode(pool, `All`="pooled"))
df2 = od_inc %>% filter(o_cz==38300) %>%
  mutate(pool=fct_recode(pool,`Bottom 20%`="Q1", `Second 20%`="Q2",`Middle 20%`="Q3",`Fourth 20%`="Q4",`Top 20%`="Q5"))
df3 = rbind(df1, df2)

sf1 = sf %>% right_join(df3, by=c("place"="d_cz_name"))  

# Plot
p1 = ggplot() +
  geom_sf(data=sf1, size=.05, color="black", fill="white") +
  geom_sf(data=sf1 %>% filter(d_cz==38300), aes(fill=pr_d_o), color="black", size=.05) +
  ggsci::scale_fill_material(name="Young adults who\ndid not move",palette="amber", labels=scales::percent) +
  ggnewscale::new_scale_fill() +
  geom_sf(data=sf1 %>% filter(d_cz!=38300) %>% filter(pr_d_o>0), aes(fill=pr_d_o), size=.05, color="black") +
  ggsci::scale_fill_material(name="Young adults\nwho moved",palette="deep-purple", labels=scales::percent) +
  guides(fill=guide_colourbar(order=1,barwidth = unit(9, "lines"))) +
  facet_wrap(~pool, ncol=2) +
  cowplot::theme_map(9.5) +
  theme(text=element_text(family=f1),
        legend.position = "top",
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        legend.key.width = unit(1,"lines"),
        legend.key.height = unit(.5,"lines"),
        plot.caption=element_text(size=7, color="grey40", hjust=0),
        plot.subtitle = element_markdown(lineheight=1.2, margin=margin(b=8)),
        plot.title = element_text(size=10.5),
        plot.margin=margin(.3,.3,.2,.3,unit = "cm")
        ) +
  labs(subtitle="Where individuals move between childhood (as measured by their location at age 16) and young adulthood<br>(as measured by their location at age 26), by **parental income level**", 
       title="Where did Los Angeles, AZ & CA adults move to?",
       caption="Data source: migrationpatterns.org")
       
ggsave("p1.png", p1, height=7.5, width=6.2, bg="#fafafa")        


 