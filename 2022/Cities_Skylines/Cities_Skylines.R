# Cities Skylines in-game maps (connections and buildable land area)
# Data from Cities Skylines, Paradox Interactive by way of skylines.country

library(tidyverse)
library(gt)
library(gtExtras)

cs_raw = read_csv("data/cities_skylines.csv")

# Table 1: 57 maps
cs = cs_raw %>% select(-sc, -`...10` ) %>%
  mutate(hi= case_when(highways>0 ~ "https://skylines.country/assets/images/icons/highway.svg"),
         ra= case_when(rails>0 ~ "https://skylines.country/assets/images/icons/rails.svg"),
         sh= case_when(shipping_channels>0 ~ "https://skylines.country/assets/images/icons/ship.svg"),
         ai= case_when(airways>0 ~ "https://skylines.country/assets/images/icons/plane.svg"),
         bla= parse_number(buildable_land_area),
         total = highways+rails+shipping_channels+airways)
         

table1 = cs %>% 
  select(group: theme, bla, hi:ai, total, highways:airways) %>%
  arrange(desc(total), desc(bla)) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_img_rows(columns=hi, height=20) %>%
  gt_img_rows(columns=ra, height=20) %>%
  gt_img_rows(columns=sh, height=20) %>%
  gt_img_rows(columns=ai, height=20) %>%
  cols_label(hi="", ra="",sh="",ai="",
             bla=md("buildable<br>land area"),
             shipping_channels=md("shipping<br>channels")) %>%
  cols_align(c(bla,highways), align="center") %>%
  cols_align(c(hi, ra, sh, ai), align="right") %>%
  gt_merge_stack(col1="map", col2="group") %>%
  tab_spanner(hi:airways,label="connections") %>%
  gt_color_box(c(highways, rails, shipping_channels, airways), domain=0:4,width = 60,
               palette=c("#F2CDF2","#D69D6E","#698A30","#355E5F","#221657")) %>%
  gt_color_box(bla, domain=range(cs$bla), suffix="%", palette="rcartocolor::Sunset") %>%
  gt_plt_bar(total, width=30, scale_type = "number", color="grey", text_color = "black") %>%
  tab_header(title=md("**Cities Skylines** in-game maps: Buildable land area and connections"),
             subtitle=md("Buildable land area percent and connections count of 57 *Cities Skylines* in-game maps available through the base game, downloadable content (DLC) and content creator pack (CCP), as of January 25, 2022. Table arranged in decending order of total connections and buildable land area percent")) %>%
  tab_source_note(source_note = md("<br>Data: Cities Skylines, Paradox Interactive by way of skylines.country")) %>%
  tab_style(
    style = list(
      cell_text(color="grey50",size=px(12))
    ),
    locations = list(
      cells_column_spanners(spanners = "connections")
 )) %>%
  tab_style(
    style = list(
      cell_text(color="grey50",size=px(12))
    ),
    locations = list(
      cells_column_labels()
 )) 
 
gtsave_extra(cs_table, "cs_table.png", expand=50)
 
# Table 2: 20 maps with the most connections and buildable area
cs20 = cs %>% arrange(desc(total), desc(buildable_land_area)) %>%
  mutate(rank=row_number()) %>%
  filter(rank<=20)
  
table2 = cs20 %>%
  select(group: theme, bla, hi:ai, total, highways:airways) %>%
  arrange(desc(total), desc(bla)) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_img_rows(columns=hi, height=20) %>%
  gt_img_rows(columns=ra, height=20) %>%
  gt_img_rows(columns=sh, height=20) %>%
  gt_img_rows(columns=ai, height=20) %>%
  cols_label(hi="", ra="",sh="",ai="",
             bla=md("buildable<br>land area"),
             shipping_channels=md("shipping<br>channels")) %>%
  cols_align(c(bla,highways), align="center") %>%
  cols_align(c(hi, ra, sh, ai), align="right") %>%
  gt_merge_stack(col1="map", col2="group") %>%
  tab_spanner(hi:airways,label="connections") %>%
  gt_color_box(c(highways, rails, shipping_channels, airways), domain=0:4,width = 60,
               palette=c("#F2CDF2","#D69D6E","#698A30","#355E5F","#221657")) %>%
  gt_color_box(bla, domain=range(cs20$bla), suffix="%", palette="rcartocolor::Sunset") %>%
  gt_plt_bar(total, width=30, scale_type = "number", color="grey", text_color = "black") %>%
  tab_header(title=md("**Cities Skylines** in-game maps with the most connections and buildable land area"),
             subtitle=md("Table of twenty in-game maps with the most total connections and buildable land area out of the fifty-seven available as of 25 January 2022, sorted in decending order.")) %>%
  tab_source_note(source_note = md("<br>Note: In-game maps refer to maps available through the City Skylines base game, downloadable content (DLC) and content creator pack (CCP). <br>Data: Cities Skylines, Paradox Interactive by way of skylines.country")) %>%
  tab_style(
    style = list(
      cell_text(color="grey50", size=px(12))
    ),
    locations = list(
      cells_column_spanners(spanners = "connections")
 )) %>%
  tab_style(
    style = list(
      cell_text(color="grey50", size=px(12))
    ),
    locations = list(
      cells_column_labels()
 )) %>%
  tab_options(source_notes.font.size = "13px")
 