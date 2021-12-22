# Global internal displacement 
# Citation: Internal Displacement Monitoring Centre (IDMC). “Global Internal Displacement Database” Data (2020). https://www.internal-displacement.org/database/displacement-data

# Part 1: Weather disaster events and displacement ------------------------
## Data: Disaster events 2008-2020 (new displacement) per hazard type (IDMC, 2020), last updated May 20, 2020.
## Code adapted from [Benjamin Nowak, 2021](https://twitter.com/BjnNowak/status/1472562727684124688)

library(readxl)
library(tidyverse)
library(lubridate)
options(dplyr.summarise.inform = FALSE) # suppress summarise info

# import data
raw = read_excel("data/IDMC_GIDD_disasters_internal_displacement_data_2020-1639983644922.xlsx")


# 1.1: Base table data ----------------------------------------------------

# dates
df = raw %>% 
  janitor::clean_names() %>%
  mutate(date=str_sub(date_of_event_start, 1,10),
         date=ymd(date),
         mth_yr = format_ISO8601(date, precision = "ym"))
range(df$date)

# get weather related-hazard type with 13 years data 
type = df %>% 
  group_by(hazard_type) %>%
  summarise(n=n_distinct(year)) %>%
  arrange(desc(n)) %>% 
  filter(n==13) %>%
  pull(hazard_type)

df1 = df %>% 
  filter(hazard_category=="Weather related",hazard_type %in% type)

# base table df
base =  df1 %>%
  group_by(hazard_type) %>%
  summarise(event_n = n(),
            displaced_n = sum(disaster_new_displacements),
            territory_n = n_distinct(country_territory))


# 1.2: Line plot ----------------------------------------------------------

data = df1 %>% 
  group_by(year, hazard_type) %>%
  summarise(disp = sum(disaster_new_displacements),
            ev = n_distinct(date)) %>%
  ungroup() %>%
  group_by(hazard_type) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(event = ev/max(ev),
         displacement = disp/max(disp)) %>%
  select(-disp, -ev) %>%
  pivot_longer(!c(year, hazard_type))
  
# line plot function
fun_plot <- function(data){
  trend <- ggplot(data,aes(x=year,y=value, color=name))+
    geom_point(size=7) + geom_line(size=3) +
    scale_x_continuous(breaks=c(2012,2016)) +
    scale_y_continuous(breaks=c(0,.5,1), label=scales::percent) +
    coord_cartesian(clip="off") +
    ggsci::scale_color_npg() +
    cowplot::theme_minimal_grid(35) +
    theme(legend.position = "none",
          axis.title=element_blank())
  return(trend)
}

line_plot = data %>%
  group_by(hazard_type) %>%
  nest() %>%
  mutate(gg=purrr::map(data, fun_plot)) %>%
  select(hazard_type = hazard_type, gg)
  

# 1.3: World map ----------------------------------------------------------

library(sf)

# prepare world map
states <- st_as_sf(maps::map(database="world", plot = FALSE, fill = TRUE))
country_to_remove <- c(
  'Antarctica','Greenland', 'French Southern and Antarctic Lands'
)
# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

states <- states %>%
  mutate(Area=ID)%>%
  select(-ID)%>%
  filter(Area %!in% country_to_remove)

area_df = df1 %>% 
  count(iso3, Area=country_territory, hazard_type) %>%
  mutate(Area=case_when(
    Area=='Bolivia (Plurinational State of)'~'Bolivia',
    Area=='China, Taiwan Province of'~'Taiwan',
    Area=='China, Taiwan Province of'~'China',
    Area=='Congo'~'Republic of Congo',
    str_detect(Area,'Ivoire')~'Ivory Coast',
    Area=='Czechia'~'Czech Republic',
    Area=="Democratic People's Republic of Korea"~'North Korea',
    Area=='Iran (Islamic Republic of)'~'Iran',
    Area=='Republic of Korea'~'South Korea',
    Area=='Russian Federation'~'Russia',
    Area=='United Kingdom of Great Britain and Northern Ireland'~'UK',
    Area=='United Republic of Tanzania'~'Tanzania',
    Area=='United States'~'USA',
    Area=='Venezuela (Bolivarian Republic of)'~'Venezuela',
    Area=='Viet Nam'~'Vietnam',
    TRUE~Area
  ))

states2 = left_join(states, area_df, by="Area")

# world map function
mapping <- function(data){
  pl <- ggplot()+
    geom_sf(data = states,size=.2) +
    geom_sf(data =states2 %>% filter(hazard_type==data), aes(fill=n),size=.2) +
    rcartocolor::scale_fill_carto_c(palette="ag_GrnYl", direction=-1) +
    theme_void()+
    theme(plot.margin=margin(0,0,0,0,"cm"),
          legend.position = "none")
  
  return(pl)

}

map_plot = data %>%
  dplyr::group_by(hazard_type) %>%
  nest() %>%
  mutate(plot=purrr::map(hazard_type, mapping)) %>%
  select(hazard_type, plot)
  

# 1.4: {gt} table ---------------------------------------------------------

library(gt)
library(gtExtras)

table1 = base %>% 
  mutate(peak=NA,
         event_terr = NA) %>%
  select(hazard_type, event_n, displaced_n, peak ,territory_n, event_terr) %>%
  gt() %>%
  gt_theme_nytimes() %>%
  # add line plot
  gt::text_transform(
    locations = cells_body(columns=peak),
    fn = function(x){
      purrr::map(
        line_plot$gg, gt::ggplot_image, 
        height = px(90), aspect_ratio = 1.6
  )}) %>%
  # add world map
  gt::text_transform(
    locations = cells_body(columns=event_terr),
    fn = function(x){
      purrr::map(
        map_plot$plot, gt::ggplot_image, 
        height = px(100), aspect_ratio = 2
  )}) %>%
  # columns
  cols_label(
    hazard_type="Hazard type",
    event_n = "Events reported",
    displaced_n = "New displacements",
    territory_n = "Territories affected",
    peak = gt::html("Yearly total<br>relative to peak<br><span style='color:#4DBBD5FF'>Event</span> | <span style='color:#E64B35FF'>Displaced</span>"),
    event_terr = html("Events by territory")
  ) %>% 
  fmt_number(columns = c("event_n", "displaced_n"),decimals = 0) %>%
  cols_align(columns=c("event_n","displaced_n","territory_n"), align="center") %>%
  # labels
  tab_header(
    title="Weather Disaster Events and Displacements (2009 - 2020)",
    subtitle=md("Events reported and new displacements as a result of **flood, storm, wet mass movement and wildfire**,<br>from Jan 1, 2009 to Dec 31, 2020, according to IDMC.")) %>%
  tab_source_note(source_note = gt::html("Data source: Internal Displacement Monitoring Centre (IDMC)<br>Code adapted from Benjamin Nowak @BjnNowak")) %>%
  tab_footnote(
    footnote = md("*Hazard type represent the main hazard that triggered displacement*"),
    locations = cells_column_labels("hazard_type")
  ) %>%
  tab_footnote(
    footnote = md("*New displacements correspond to the estimated number of internal displacements over a given period of time (reporting year). Figures may include individuals who have been displaced more than once.*"),
    locations = cells_column_labels("displaced_n")
  ) %>%
  tab_footnote(
    footnote = md("*Number of events reported by geographical regions, where regions colored green have a higher count htan regions colored yellow*"),
    locations = cells_column_labels("event_terr")
  ) %>%
  tab_options(source_notes.font.size = "13px",
              footnotes.font.size = "12px")

table1


# Part 2: Internal displacement 2020 --------------------------------------
## Dataset: 2020 Internal Displacement (IDMC, 2020), last updated May 20, 2020.

# import data
df20 = read_csv("data/IDMC_GIDD_internal_displacement_data_2020-1633852410407.csv") %>% clean_names()

# prepare data
df20b = df_int %>% mutate(region = countrycode(iso3, origin="iso3c", destination="continent", warn=F),
                      subregion= countrycode(iso3, origin="iso3c", destination="un.regionsub.name", warn=F)) %>%
  mutate(region = case_when(iso3=="AB9"~ "Africa", 
                            iso3=="XKX"~"Europe",
                            TRUE~region)) %>%
  mutate(subregion = case_when(iso3=="AB9"~ "Sub-Saharan Africa", 
                               iso3=="XKX"~"Eastern Europe",
                               iso3=="TWN"~"Eastern Asia",
                               TRUE~subregion))  

# {gt} table
table2 = df20b %>% group_by(region, subregion) %>%
  summarise(across(conflict_new_displacement:total_number_of_idps, list(sum), na.rm=T)) %>%
  ungroup() %>%
  select(region, subregion, total_new_displacement_1,conflict_new_displacement_1,disaster_new_displacement_1,
         total_number_of_idps_1, conflict_total_number_of_id_ps_1,disaster_total_number_of_id_ps_1) %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(title = str_to_upper("2020 Internal Displacement"),
             subtitle=md("In 2020, there is a **total of 40.5M new displacements**, of which, 9.8M is a result of <span style = 'color:#6c757d;'>*conflict and violence*</span> from 42 countries and territories, and 30.7M is a result of <span style = 'color:#6c757d;'>*disasters*</span> in 145 countries and territories. As of the end of 2020, the **total number of **IDPs** is 55M**, where 48M comes from <span style = 'color:#6c757d;'>*conflict and violence*</span> in 59 countries and territories, and 7M from <span style = 'color:#6c757d;'>*disasters*</span> in 103 countries and territories.")) %>%
  tab_source_note(source_note="Data source: Internal Displacement Monitoring Center (IDMC)") %>%
  cols_label(
    total_new_displacement_1 = html("Total"),
    total_number_of_idps_1 = html("Total"),
    conflict_new_displacement_1 = html("Conflict"),
    conflict_total_number_of_id_ps_1 = html("Conflict"),
    disaster_new_displacement_1=html("Disaster"),
    disaster_total_number_of_id_ps_1=html("Disaster")) %>%
  tab_spanner(label="New Displacements",columns=total_new_displacement_1:disaster_new_displacement_1) %>%
  tab_spanner(label="Number of IDPs", columns =total_number_of_idps_1:disaster_total_number_of_id_ps_1) %>%
  tab_footnote(
    footnote = md("New displacements correspond to the estimated number of internal displacements over a given period of time (reporting year)."),
    locations = cells_column_spanners("New Displacements")
  ) %>%
  tab_footnote(
    footnote = md("Represents the total number of IDPs (“stock”) as of the end of the year. It could be understood as the total number of people living in a situation of displacement as of the end of the reporting year."),
    locations = cells_column_spanners("Number of IDPs")
  ) %>%
  gt_color_rows(columns=total_new_displacement_1:disaster_new_displacement_1, 
                palette="RColorBrewer::Blues",type="continuous") %>%
  gt_color_rows(columns=total_number_of_idps_1:disaster_total_number_of_id_ps_1, 
                palette="RColorBrewer::Oranges", type="continuous") %>%
  fmt_number(total_new_displacement_1:disaster_total_number_of_id_ps_1, use_seps = T, drop_trailing_zeros = T) %>%
  tab_options(table.font.size = "13px",
              heading.title.font.size = "18px") %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Libre Franklin"), weight='800',align = "left",color='#203B46')),
    locations = cells_title(groups = "title")
  ) 

table2