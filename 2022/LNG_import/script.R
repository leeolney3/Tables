# U.S. LNG imports
# Data source: https://www.eia.gov/naturalgas/data.php#imports

# Libraries 
library(tidyverse)
library(readxl)
library(lubridate)
library(reactable)
library(reactablefmtr)

# Import .xls
df1 <- read_excel("data/NG_MOVE_IMPC_S1_M.xls", sheet = 'Data 1', skip = 2, col_names = TRUE)

# Prepare data
df2 = df1 %>%
  mutate(Date=as.Date(Date, format= "%Y-%m-%d")) %>%
  pivot_longer(!Date) %>%
  filter(grepl('From', name)) %>%
  separate(name, sep="From", c("Type","Country")) %>%
  mutate(Type=str_remove(Type, "U.S."),
         Type=str_remove(Type,"Imports"),
         Type=str_trim(Type),
         Country=str_replace(Country, " \\s*\\([^\\)]+\\)", ""),
         Country=str_trim(Country)
         ) %>%
  filter(Type=="Liquefied Natural Gas") 
  
range(df2$Date)  
  
df_timeline = df2 %>%
  select(Country, Date, value) %>%
  mutate(value=replace_na(value,0)) %>%
  complete(Date) %>%
  group_by(Country) %>%
  summarise(timeline=list(value)) 

df_max = df2 %>%
  group_by(Country) %>%
  filter(value==max(value, na.rm = T)) %>%
  select(Country, max_date= Date, max_val= value) %>%
  mutate(max_date = format(as.Date(max_date), "%Y %b"))

df_tab = df2 %>% 
  filter(!is.na(value)) %>%
  filter(value!=0) %>%
  group_by(Country) %>% 
  summarise(total=sum(value, na.rm=T)) %>%
  ungroup() %>%
  left_join(df_timeline, by="Country") %>%
  left_join(df_max, by="Country") %>%
  arrange(total) %>%
  mutate(Country=fct_rev(fct_inorder(Country)),
         Country=fct_relevel(Country, "Other Countries", after = 18)) %>%
  arrange(Country)
  
# Table
rtable = reactable(
  df_tab,
  theme = fivethirtyeight(centered = TRUE, header_font_size = 11),
  pagination = FALSE,
  columns = list(
    Country = colDef(maxWidth = 170),
    total = colDef(name="Total (MMcf)",maxWidth = 90, format=colFormat(separators=TRUE)),
    max_date=colDef(name="Max (ym)",maxWidth = 90),
    max_val=colDef(name="Max (MMcf)",maxWidth = 90, format=colFormat(separators=TRUE)),
    timeline = colDef(name="Timeline (1973 to 2021)",
                      cell = react_sparkline(
                        df_tab,
                        line_color = "#7209b7",
                        height = 40,
                        #highlight_points = highlight_points(max = "red", last="green"),
                        labels=c("max")
                      )
    )
  )
) %>% 
  google_font(font_family = "Roboto") %>%
  add_title("U.S. LNG Imports", margin = margin(0, 0, 10, 0)) %>%
  add_subtitle("Table showing U.S. Liquefied natural gas (LNG) imports from January 1973 to December 2021, arranged in descending order of total imports in MMCF.", font_size = 16, margin = margin(0, 0, 10, 0)) %>%
  add_source("Table created with {reactablefmtr} •  Data: eia.gov", font_size = 14, margin=margin(10,0,0,0))
  
# Save .png
rtable %>% save_reactable("rtable.png")
  