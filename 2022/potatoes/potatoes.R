# Potato production in Europe (EU47+UK)
# Data from Our World In Data
# Data link : https://ourworldindata.org/explorers/global-food?region=Europe&facet=none&Food=Potatoes&Metric=Exports&Per+Capita=true&country=USA~DEU~FRA~GBR~BRA~ZAF

library(tidyverse)
library(ggtext)
library(geofacet)
library(ggnewscale)

library(showtext)
font_add_google("Fira Sans Condensed") 
showtext_auto()
f1 = "Fira Sans Condensed"

# Import data
data = read_csv("data/global-food-potato.csv",show_col_types = FALSE) %>%
  janitor::clean_names()

# Part 1: Geofacet slope chart 
# Reference: https://rud.is/b/2022/01/04/starting-2022-off-with-a-fairly-complex-ggplot2-recreation-plot/

# Wrangle  
data1 = data %>% 
  mutate(country=case_when(country=="Czechia"~"Czech Republic",TRUE~country)) %>%
  filter(country %in% eu_grid1$name)  
  
data2 = data1 %>% 
  filter(year==2018) %>%
  mutate(group=case_when(exports_per_capita_kg>imports_per_capita_kg~"Exports (kg) per capita > Imports (kg) per capita in 2018")) 
  
# Plot
data1 %>%
  #filter(between(year,2000,2018)) %>%
  filter(year==2000|year==2018) %>%
  ggplot(aes(year, production_per_capita_kg)) +
  geom_rect( data = data2,
      aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,fill = group),
      alpha = 1/6, color = NA
      ) +
  scale_fill_manual(
      name = NULL,
      values = c("#613dc1"),
      na.translate = FALSE
    ) +
  geom_rect(
      data = data2 %>% filter(is.na(group)),
      aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
      alpha = 1/7, color = NA, fill = "#999999",
    ) + 
  geom_hline(yintercept = 0, color="grey10", size=.3) +
  geom_line(aes(group=country)) +
  new_scale_fill() +
  geom_point(data=data1 %>% filter(year==2000|year==2018), 
             aes(fill=factor(year)), 
             show.legend=F,size = 2, stroke = .5, shape = 21, color="white") +
  geom_text(data=data1 %>% filter(year==2000|year==2018), 
            aes(label=round(production_per_capita_kg), y=production_per_capita_kg+100, color=factor(year),
                hjust=ifelse(year==2018,0,0.5)), 
            size=2.8, show.legend = F, family=f1, fontface="bold") +
  scale_color_manual(values=c("#999999","#613dc1")) +
  scale_fill_manual(values=c("#999999","#613dc1")) +
  facet_geo(~ country, grid = "eu_grid1") +
  scale_x_continuous(limits=c(1997,2022), breaks=c(2000,2018)) +
  scale_y_continuous(breaks=c(0,300,600)) +
  coord_cartesian(clip="off", expand=F) +
  cowplot::theme_minimal_hgrid(10.5) +
  theme(text=element_text(family=f1),
        plot.margin=margin(1,1,.7,1,unit="cm"),
        panel.spacing = unit(15, "pt"),
        panel.grid.major.y = element_line(size=.3),
        axis.text=element_text(color="#999999", size=7),
        axis.ticks.x=element_line(color="grey10"),
        axis.line.x=element_line(color="grey10", size=.3),
        axis.ticks.y=element_blank(),
        legend.position = "top",
        legend.justification="center",
        plot.title=element_markdown(hjust=.5, size=14),
        plot.caption.position = "plot",
        plot.caption=element_text(color="grey20", hjust=0)
        ) +
  labs(x=NULL, y=NULL,
       title="Potato production (kg) per capita in Europe, <span style='color:#999999'>2000</span> and <span style='color:#613dc1'>2018</span>",
       caption="Data source: https://ourworldindata.org/") 
       
# Part 2: Table
library(gt)
library(gtExtras)

# Wrangle
df18 = data1 %>% filter(year==2018) %>%
  select(2:3, contains("capita")) %>%
  rename_all(~stringr::str_replace(.,"_per_capita","")) %>%
  select(!contains("tonnes")) %>%
  select(country,production_kg,land_use_m2,
         imports_kg,exports_kg,domestic_supply_kg,
         food_supply_kg_per_year,food_kg,animal_feed_kg,other_uses_kg,supply_chain_waste_kg) %>%
  mutate(fl = case_when(country=="Austria"~"https://hatscripts.github.io/circle-flags/flags/at.svg",
                        country=="Belgium"~"https://hatscripts.github.io/circle-flags/flags/be.svg",
                        country=="Bulgaria"~"https://hatscripts.github.io/circle-flags/flags/bg.svg",
                        country=="Croatia"~"https://hatscripts.github.io/circle-flags/flags/hr.svg",
                        country=="Cyprus"~"https://hatscripts.github.io/circle-flags/flags/cy.svg",
                        country=="Czech Republic"~"https://hatscripts.github.io/circle-flags/flags/cz.svg",
                        country=="Denmark"~"https://hatscripts.github.io/circle-flags/flags/dk.svg",
                        country=="Estonia"~"https://hatscripts.github.io/circle-flags/flags/ee.svg",
                        country=="Finland"~"https://hatscripts.github.io/circle-flags/flags/fi.svg",
                        country=="France"~"https://hatscripts.github.io/circle-flags/flags/fr.svg",
                        country=="Germany"~"https://hatscripts.github.io/circle-flags/flags/de.svg",
                        country=="Greece"~"https://hatscripts.github.io/circle-flags/flags/gr.svg",
                        country=="Hungary"~"https://hatscripts.github.io/circle-flags/flags/hu.svg",
                        country=="Ireland"~"https://hatscripts.github.io/circle-flags/flags/ie.svg",
                        country=="Italy"~"https://hatscripts.github.io/circle-flags/flags/it.svg",
                        country=="Latvia"~"https://hatscripts.github.io/circle-flags/flags/lv.svg",
                        country=="Lithuania"~"https://hatscripts.github.io/circle-flags/flags/lt.svg",
                        country=="Luxembourg"~"https://hatscripts.github.io/circle-flags/flags/lu.svg",
                        country=="Malta"~"https://hatscripts.github.io/circle-flags/flags/mt.svg",
                        country=="Netherlands"~"https://hatscripts.github.io/circle-flags/flags/nl.svg",
                        country=="Poland"~"https://hatscripts.github.io/circle-flags/flags/pl.svg",
                        country=="Portugal"~"https://hatscripts.github.io/circle-flags/flags/pt.svg",
                        country=="Romania"~"https://hatscripts.github.io/circle-flags/flags/ro.svg",
                        country=="Slovakia"~"https://hatscripts.github.io/circle-flags/flags/sk.svg",
                        country=="Slovenia"~"https://hatscripts.github.io/circle-flags/flags/si.svg",
                        country=="Spain"~"https://hatscripts.github.io/circle-flags/flags/es.svg",
                        country=="Sweden"~"https://hatscripts.github.io/circle-flags/flags/se.svg",
                        country=="United Kingdom"~"https://hatscripts.github.io/circle-flags/flags/gb.svg",
                        )) %>%
  mutate(rank = dense_rank(desc(production_kg))) %>%
  relocate(rank, fl) %>%
  arrange(rank)
  
df18 = df18 %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  select_all(funs(gsub("_", " ", .))) %>%
  rename_at(vars(4:13), funs(str_sub(., 1, -3))) %>%
  rename_all(funs(str_to_sentence(.))) %>%
  select_all(funs(trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")))
  
tab = df18 %>% filter(Rank<=15) %>%
  gt() %>%
  gt_img_rows(Fl) %>%
  cols_label(`Food supply kg per ye`=md("Food supply<br>per year"),
             "Supply chain waste" = md("Supply chain<br>waste"),
             "Food" =md("Direct<br>human food"),
             "Other uses" ="Industrial uses",
             Fl ="") %>%
  tab_spanner(label="Allocated for", columns=10:12) %>%
  fmt_symbol_first(column = c(4,6:13), suffix = " kg", last_row_n = 15) %>%
  fmt_symbol_first(column = 5, suffix = " m2", last_row_n = 15) %>%
  gt_hulk_col_numeric(column = 4:13, trim=TRUE) %>%
  cols_hide(c(1,9)) %>%
  gt_theme_538() %>%
  # Header and source note
  tab_header(title="Potatoes per capita, 2018",
             subtitle ="15 countries in EU27 and the UK with the most potato production per capita in 2018.") %>%
  tab_source_note(source_note = gt::html("Data source: ourworldindata.org and fao.org")) %>%
  tab_options(source_notes.font.size = "14px",
              footnotes.font.size = "13px",
              heading.title.font.size = "22px",
              heading.subtitle.font.size = "15px") %>%
  # Footnote
  tab_footnote(footnote=md("*Land use: The amount of cropland used for production.*"),
               locations=cells_column_labels(columns = `Land use`)) %>%
  tab_footnote(footnote=md("*Domestic Supply: Supply available after trade, calculated as production plus imports, minus exports.*"),
               locations=cells_column_labels(columns = `Domestic supply`)) %>%
  tab_footnote(footnote=md("*Industrial uses: The quantity that is allocated to industrial uses such as biofuel, pharmaceuticals or textile products.*"),
               locations=cells_column_labels(columns = `Other uses`)) %>%
  tab_footnote(footnote=md("*Supply chain waste: The quantity that is lost or wasted in supply chains through poor handling, spoiling, lack of refrigeration and damage from the field to retail. It does not include consumer waste.*"),
               locations=cells_column_labels(columns = `Supply chain waste`)) %>%
  # style
  tab_style(style=list(cell_text(size=14.5)),
            locations = cells_body(columns=4:13)) %>%
  tab_style(style=list(cell_text(color="grey20", weight = "lighter")),
            locations= cells_column_labels()) %>%
  tab_style(style=list(cell_text(color="grey20", weight = "lighter")),
            locations= cells_column_spanners()) %>%
  tab_style(style=list(cell_text(color="grey20", weight = "lighter")),
            locations= cells_footnotes()) %>%
  tab_style(
    style=list(cell_text(weight="normal")),
    location=cells_body(columns=Country)) %>%
  tab_style(
    style = list(
      cell_text(align = "center")),
    locations = cells_column_labels(4:13)
  ) %>%
  cols_width(4:13 ~px(100))
  
# Save table
gtsave(table1, "potato_gt.png",expand = 50)
  