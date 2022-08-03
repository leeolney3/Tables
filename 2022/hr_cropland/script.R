# Croatia, count of agriculture parcels per 10km grid by primary crop type (orchard, olive grove, vineyard and mixed perennial crops)
# Data: Maja Schneider et al.'s [EuroCrops](https://github.com/maja601/EuroCrops) by way of [Data is Plural](https://www.data-is-plural.com/archive/2022-08-03-edition/)
# Dataset retrieved from: https://zenodo.org/record/6937139
# HR 10km grid from: https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/croatia-shapefile

# Load libaries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(scico)
library(patchwork)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE

# Import shp files
hr1 = read_sf("HR/HR_2020_EC21.shp") #crop sf
hr2 = read_sf("Croatia_shapefile/hr_10km.shp") #grid sf
hr2 = hr2 %>% mutate(grid_id = row_number()) 
hr3 = ne_countries(scale=10,country="croatia", returnclass = "sf") #country sf

# Transform shp files
hr1a = st_transform(hr1, crs = raster::crs(hr2)) #crop 
hr3a = st_transform(hr3, crs = raster::crs(hr2)) #country 

# Font
font_add_google("Barlow Condensed", "barlow")
f1 = "barlow"

# Theme
theme1= cowplot::theme_map(9) +
  theme(text=element_text(family=f1),
        legend.title=element_text(size=11),
        legend.position = c(.6,.5),
        legend.direction = "horizontal")

# Count of parcels by crop name
hr %>% st_drop_geometry() %>% 
  mutate(EC_trans_n=str_to_lower(EC_trans_n)) %>%
  count(EC_trans_n, sort=T)
        
# Plot
## Olive grove
hr1b=hr1a %>% filter(EC_trans_n=="Olive grove")
hr2$n_colli = lengths(st_intersects(hr2, hr1b))
hr2a = filter(hr2, n_colli > 0)
p1 =ggplot() +
  geom_sf(data=hr2a, aes(fill=n_colli),size=.2, alpha=.8, color="white") +
  geom_sf(data=hr3a, fill="transparent", size=.2) +
  scico::scale_fill_scico(name="Olive grove",palette="bamako", direction=-1) +
  theme1 +
  guides(fill=guide_colorbar(title.position = "top",title.hjust = .5, barheight = unit(.5, "lines"),barwidth = unit(7, "lines")))
  
## Vineyard
hr1b=hr1a %>% filter(EC_trans_n=="Vineyard")
hr2$n_colli = lengths(st_intersects(hr2, hr1b))
hr2a = filter(hr2, n_colli > 0)
p2 = ggplot() +
  geom_sf(data=hr2a, aes(fill=n_colli),size=.2, alpha=.8, color="white") +
  geom_sf(data=hr3a, fill="transparent", size=.2) +
  scico::scale_fill_scico(name="Vineyard",palette="bamako", direction=-1) +
  theme1 +
  guides(fill=guide_colorbar(title.position = "top",title.hjust = .5, barheight = unit(.5, "lines"),barwidth = unit(7, "lines")))
  
## Orchard
hr1b=hr1a %>% filter(EC_trans_n=="Orchard")
hr2$n_colli = lengths(st_intersects(hr2, hr1b))
hr2a = filter(hr2, n_colli > 0)
p3 = ggplot() +
  geom_sf(data=hr2a, aes(fill=n_colli),size=.2, alpha=.8, color="white") +
  geom_sf(data=hr3a, fill="transparent", size=.2) +
  scico::scale_fill_scico(name="Orchard",palette="bamako", direction=-1) +
  theme1 +
  guides(fill=guide_colorbar(title.position = "top",title.hjust = .5, barheight = unit(.5, "lines"),barwidth = unit(7, "lines")))
  
## Mixed perennial crops
hr1b=hr1a %>% filter(EC_trans_n=="Mixed perennial crops")
hr2$n_colli = lengths(st_intersects(hr2, hr1b))
hr2a = filter(hr2, n_colli > 0)
p4 = ggplot() +
  geom_sf(data=hr2a, aes(fill=n_colli), size=.2, alpha=.8, color="white") +
  geom_sf(data=hr3a, fill="transparent", size=.2) +
  scico::scale_fill_scico(name="Mixed perennial crops",palette="bamako", direction=-1) +
  theme1 +
  guides(fill=guide_colorbar(title.position = "top",title.hjust = .5, barheight = unit(.5, "lines"),barwidth = unit(7, "lines")))
  
# Combine plots
c1 = (p3 + p1) /
  (p2 + p4) 

c2 = c1 + 
  plot_annotation(title="Croatia Cropland",
                  subtitle="Count of agriculture parcels per 10km grid, by parcel’s primary crop type",
                  caption="Source: Maja Schneider et al. (2021), EEA and Natural Earth") &
  theme(text=element_text(family=f1),
        plot.title=element_text(size=19, face="bold",hjust=.5),
        plot.subtitle=element_text(hjust=.5, size=12),
        plot.caption=element_text(size=11, color="grey30"),
        plot.margin=margin(.3,.2,.2,0,unit="cm"))
        
# Save
ggsave("p1.png",c2,height=9, width=9, bg="white")
        