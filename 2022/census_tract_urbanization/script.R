# Census tract urbanization (https://www.data-is-plural.com/archive/2022-03-30-edition/)
# Changes in housing unit estimates, 2019 from 2010, Harris County, Texas
# Data: https://osf.io/fzv5e/

# Libraries
library(tidyverse)
library(tigris)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("PT Sans")
f1 = "PT Sans"

# Data 
data= read_csv("data/HHUUD10_long.csv")

# Wrangle
t1 <- tracts("TX", "Harris County")

h19 = data %>% filter(STATE=="TX", COUNTY=="Harris", YEAR==2019|YEAR==2010) %>% 
  select(GEOID=GEOID10, YEAR, HU) %>%
  group_by(GEOID) %>%
  mutate(diff=diff(HU)) %>%
  mutate(grp = case_when(diff>0~"Increase", diff<0~"Decrease", diff==0~"No change")) %>%
  ungroup() %>%
  filter(YEAR==2019)

t19 = t1 %>% left_join(h19, by="GEOID") 
#t19 = t1 %>% left_join(h19, by="GEOID") %>% filter(!is.na(grp))

t19$grp[is.na(t19$grp)] <- "No data"

# Plot
ggplot() +
  geom_sf(data=t1, size=.1, color="black", fill="#d4dddd") +
  geom_sf(data=t19, aes(fill=factor(grp, levels=c("Increase","No change","Decrease","No data"))), 
          size=.1, color="black", alpha=.9, key_glyph="point") +
  scale_fill_manual(name="", values=c("#EE5C3D","#30C0D1","#056AAA","#e2e3e1"), na.value = "#E2E3E1") +
  coord_sf(expand=F) +
  cowplot::theme_map(13) +
  theme(legend.position = "top",
        legend.justification = "left",
        text=element_text(family=f1),
        legend.margin=margin(l=-7),
        plot.margin=margin(.2,.2,.2,.2,unit="cm"),
        plot.caption=element_text(color="#595959", hjust=0, size=8.5, lineheight = 1.2)) +
  guides(fill=guide_legend(override.aes = list(size=3.5, shape=21, color="transparent"))) +
  labs(title="Change in housing unit estimates",
       subtitle="2019 from 2010, Harris County, Texas",
       caption="\nSource: Markley, S.N., Holloway, S.R., Hafley, T.J. et al. Housing unit and urbanization estimates for the continental U.S. in consistent\ntract boundaries, 1940–2019. Sci Data 9, 82 (2022). https://doi.org/10.1038/s41597-022-01184-x") +
  guides(fill=guide_legend(override.aes = list(shape=21, size=4)))
  
ggsave("p1.png", bg="white", height=6.6, width=7)

