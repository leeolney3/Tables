# Wastewater treatment plants, contiguous U.S.
# Data: HydroWASTE, available at https://figshare.com/articles/dataset/HydroWASTE_version_1_0/14847786/1
# Layer map inspired by Moriah Taylor, https://twitter.com/moriah_taylor58/status/1499206363729305602

library(tidyverse)
library(sf)
library(layer)
library(ragg)
library(cowplot)

data= read_csv("HydroWASTE_v10/HydroWASTE_v10.csv") %>% janitor::clean_names()

df_us = data %>% filter(country=="United States") %>%
  select(Y=lat_wwtp, X= lon_wwtp, pop_served,waste_dis,level)

s1 = st_as_sf(df_us, coords = c("X", "Y"), crs = "WGS84")
s2 = st_crop(s1, xmin=-124.736342, ymin=24.521208, xmax=-66.945392, ymax=49.382808)

sec = st_as_sf(s2 %>% filter(level=="Secondary"))
adv = st_as_sf(s2 %>% filter(level=="Advanced"))
pri = st_as_sf(s2 %>% filter(level=="Primary"))

tilt_sec= tilt_map(sec)
tilt_adv = tilt_map(adv, y_shift=50)
tilt_pri = tilt_map(pri, y_shift=100)

maps_list <- list(tilt_sec, tilt_adv, tilt_pri)

# plot

plot <- plot_tiltedmaps(maps_list,
                layer = c("value", "value", "value"),
                palette = c("magma", "magma", "magma"))
                
plot = plot + theme(plot.margin=margin(1,.5,.75,.5, unit="cm"))

# annotate plot
ggdraw(plot) +
  draw_label("Wastewater treatment plants, contiguous U.S.",
             x=0.01, y=0.99, hjust=0, vjust=1, size=15, color="grey30") +
  draw_label("Source: Ehalt Macedo, H., Lehner, B., Nicell, J., Grill, G., Li, J., Limtong, A., and Shakya, R.: Distribution and characteristics of wastewater\ntreatment plants within the global river network, Earth Syst. Sci. Data, 14, 559–577, https://doi.org/10.5194/essd-14-559-2022, 2022. ",
             x=0.01, y=0.01, hjust=0, vjust=0, size=8.5, color="grey30") +
  draw_label("Level: Primary\n(n=47)", size=10, x=0.01, y=.85, 
             hjust=0, vjust=1, lineheight = 1.2) +
  draw_label("Level: Advanced\n(n=5,559)", size=10, x=0.01, y=.55, 
             hjust=0, vjust=1, lineheight = 1.2) +
  draw_label("Level: Secondary\n(n=9,213)", size=10, x=0.01, y=.25, 
             hjust=0, vjust=1, lineheight = 1.2)                
             
# save plot
ggsave("wwtp.png",height=7, width=8, unit="in", bg="white")