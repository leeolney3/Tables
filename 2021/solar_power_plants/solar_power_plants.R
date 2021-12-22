# Data: The 20 Largest Solar Power Plants (MakeoverMonday 2021w37)
# Data source: Wikipedia, https://data.world/makeovermonday/2021w37

library(httr)
library(readxl)
library(tidyverse)
library(gt)
library(gtExtras)

# get data
GET("https://query.data.world/s/3enbbl7vm2p65oit3r3wfd3nygoiru", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

# {gt} table with gt_merge_stack and gt_color_box
df %>% select(Location=Name, Country, "Capacity (MWp)"="MWp (Megawatts Peak Capacity)", "Land Area (km²)" = "Sq Km", Year) %>%
  arrange(desc("Capacity (MWp)")) %>%
  mutate(Country=str_trim(Country)) %>%
  gt::gt() %>%
  #fmt_symbol_first(column=Capacity, suffix=" MWp") %>%
  #fmt_symbol_first(column="Land Area", suffix=" km²") %>%
  #gt_hulk_col_numeric(Capacity:"Land Area", trim = TRUE) 
  gt_merge_stack(col1 = Location, col2 = Country) %>%
  gt_color_box("Capacity (MWp)", domain=c(550,2245), width=100, palette = "wesanderson::Zissou1", 
               use_paletteer = TRUE) %>%
  gt_color_box("Land Area (km²)", domain=c(4.90,77.70), width=100, palette = "wesanderson::Zissou1", 
               use_paletteer = TRUE) %>%
  gt_theme_538() %>%
  tab_header(title = "The 20 largest solar power plants in the world, by capacity") %>%
  tab_source_note(source_note="Source Article: SolarPower.guide | Data Source: Wikipedia")