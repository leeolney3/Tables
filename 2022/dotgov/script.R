# 2022-04-29
# Registered domains in the .gov zone
# Data source: https://github.com/cisagov/dotgov-data

library(tidyverse)
library(gt)
library(gtExtras)

# Data
full= read_csv("https://raw.githubusercontent.com/cisagov/dotgov-data/main/current-full.csv") %>% janitor::clean_names()

federal = read_csv("https://raw.githubusercontent.com/cisagov/dotgov-data/main/current-federal.csv") %>% janitor::clean_names()

dim(full)
dim(federal)

full %>% summarise_all(funs(n_distinct(.))) %>% pivot_longer(everything())
federal %>% summarise_all(funs(n_distinct(.))) %>% pivot_longer(everything())

# Table data
df = full %>% 
  mutate(state=case_when(state=="Puerto Rico"~"PR", TRUE~state)) %>%
  filter(state %in% geofacet::us_state_contiguous_grid1$code) %>%
  mutate(state_name = state.name[match(state,state.abb)],
         state_name= case_when(state=="DC"~"District of Columbia",
                               TRUE~state_name)) 
                               
x1 = df %>% 
  count(state_name, state) %>%
  mutate(vec= paste0("https://raw.githubusercontent.com/coryetzkorn/state-svg-defs/master/SVG/",state,".svg")) %>%
  select(-state)

x2 = df %>% 
  count(state_name, domain_type) %>%
  pivot_wider(names_from=domain_type, values_from=n) 
  
x3 = x2 %>% left_join(x1, by="state_name") %>%
  select(12,1,11,2,3,5,6,7,8,4,9,10) %>%
  replace(is.na(.), 0)
  
# gt table
        t1 = x3 %>%
  arrange(desc(n)) %>%
  slice(1:15) %>%
  gt() %>%
  gt_theme_538() %>%
  opt_table_font(font=list(google_font(name="Roboto"))) %>%
  gt_img_rows(columns=vec, height=30) %>%
  cols_label(vec="", state_name="State", n="Total",
             `Federal - Executive`="Federal- Executive",
             `Federal - Judicial`="Federal- Judicial",
             `Federal - Legislative`="Federal- Legislative"
             ) %>%
  tab_style(style = cell_text(color = "black",size=px(12),
                              transform = "capitalize"),
            locations = cells_column_labels(everything())) %>%
  tab_options(table.font.size = px(13.5),
              data_row.padding = px(1),
              heading.title.font.size=px(19),
              heading.subtitle.font.size = px(14),
              source_notes.font.size = px(13)) %>%
  cols_width(c(County, State, Tribal, n) ~px(50),
             vec~px(50),
             c(City, Interstate)~px(55),
             state_name~px(150),
             `Independent Intrastate`~px(75),
             starts_with("Federal")~px(70)) %>%
  tab_spanner(columns = 4:12, label="Domain Type") %>%
  tab_style(style = cell_text(color = "black",size=px(13),transform = "capitalize"),
      locations = cells_column_spanners(everything())) %>%
  tab_style(style=cell_text(weight=400),
            locations=cells_body(column=c(state_name, n))) %>%
  gt_color_rows(columns=n, palette="ggsci::green_material") %>%
  tab_header(title=md("**Registered domains in the .gov zone**"),
             subtitle=md("15 States in contiguous U.S with the most number of total registered domains in the .gov zone, as of April 29, 2022. The US Government's executive, legislative, and judicial branches are represented, as are US-based state, territory, tribal, city, and county governments."))  %>%
  tab_source_note(source_note = md("<br>Source: github.com/cisagov/dotgov-data (retrieved April 29, 2022)")) %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Libre Franklin"), weight='800',align = "left", color="#343a40")),
    locations = cells_title(groups = "title")
  ) 
  
# Save
gtsave_extra(t1,"t1.png")                         

