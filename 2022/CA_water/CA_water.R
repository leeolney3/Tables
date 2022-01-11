# CALIFORNIA Household water supply shortage reports count (2014 to 2021)
# Data from California Open Data, by way of DataIsPlural
# Dataset download: https://data.ca.gov/dataset/household-water-supply-shortage-reporting-system-data
# Corresponding article: https://www.latimes.com/projects/california-farms-water-wells-drought/ 

library(tidyverse)
library(sf)
library(lubridate)

library(showtext)
font_add_google("Lato") 
showtext_auto()

# Import data
reports = read_csv("data/householdwatersupplyshortagereportingsystemdata.csv",show_col_types = FALSE) %>%
  janitor::clean_names()

# CA shp 
ca_counties = read_sf("data/cb_2020_us_county_20m/cb_2020_us_county_20m.shp") %>% 
  janitor::clean_names() %>%
  st_transform(4326) %>% 
  filter(state_name=="California")

# Wrangle   
reports = reports %>% 
  mutate(date = mdy(report_date),
         year= year(date)) %>%
  rename(x = longitude, y = latitude)

reports21 = reports %>% filter(between(year, 2014, 2021))

# map
ggplot() +
  geom_sf(data=ca_counties,size = 0.29, fill = "transparent", color = "grey50") +
  geom_hex(data=reports21 %>% drop_na(x,y), 
           aes(x, y), bins=55, alpha=.8,color = "grey15", size=.35) +
  #geom_point(data = reports %>% drop_na(x,y), aes(x, y),alpha=.5) 
  #scale_fill_stepsn(colors = pal, n.breaks = 6, trans="pseudo_log") +
  rcartocolor::scale_fill_carto_c(palette="ag_Sunset", direction = -1,trans="pseudo_log",
                                  breaks=c(1,10,100,500)) +
  coord_sf() + 
  theme_void(base_family = "Lato") +
  theme(legend.position=c(0.74,0.75),
        legend.direction = "horizontal",
        legend.title=element_text(lineheight = 0.39, size=rel(2.8)),
        legend.text=element_text(size=rel(2.2), margin=margin(t=-10)),
        plot.caption=element_text(size=rel(2.1), color="grey20"),
        plot.background=element_rect(fill="#d6e2e9", color=NA), 
        plot.margin=margin(rep(.5,4), unit="cm")) +
  guides(fill=guide_colorbar(title.position = "top",
                             barwidth = unit(10, "lines"),
                             barheight = unit(.4, "lines"),)) +
  labs(fill="CALIFORNIA\nHousehold water supply\nshortage reports count\n(2014 to 2021)",
       caption="#Data source: data.ca.gov")

# save 
ggsave("CA_water.png", height=7, width=6, unit="in")

