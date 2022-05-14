# The Simpsons: Tapped Out mobile game super premium items, as of 13 May 2022
# Data collected from simpsonswiki.com

# Libraries
library(tidyverse)
library(gt)
library(gtExtras)

# Data
df = read_csv("tsto_super_premium.csv")

df1 = df %>% 
  mutate(donuts=as.numeric(donuts),
         type=as.factor(type)) 
         
# Table
tab1 = df %>%
  gt() %>%
  gt_theme_538() %>%
  opt_table_font(
    font = list(google_font(name = "Karla"))) %>%
  gt_img_rows(img) %>%
  fmt_missing(columns = notes, missing_text = "") %>%
  tab_options(table.font.size = px(12),
              data_row.padding = px(1)) %>%
  cols_width(type~px(80)) %>%
  cols_label(event.date="event date",
             donuts="donuts purchased",
             item="item name",
             img="") %>%
  tab_header(title= "The Simpsons: Tapped Out mobile game super premium items",
             subtitle="Table of 33 super premium items including building, character, costume, decoration and npc, in-game as of 2022-05-13.") %>%
  tab_source_note(source_note = "Data from simpsonswiki.com")
  
# Save table
gtsave_extra(tab1, "super_premium.png")          

