# gt table inspired by https://www.cbc.ca/news/politics/census-2021-release-population-cities-1.6344179 

library(tidyverse)
library(gt)
library(gtExtras)

# east_north_central division
test = tribble(
  ~"State",~"2016",~"2021",~"% Change",~"vec",
  "Illinois",4070000, 4260000,4.8,"https://raw.githubusercontent.com/coryetzkorn/state-svg-defs/master/SVG/IL.svg",
  "Indiana",4650000,5000000,7.6,"https://raw.githubusercontent.com/coryetzkorn/state-svg-defs/master/SVG/IN.svg",
  "Michigan", 1280000, 1340000,5,"https://raw.githubusercontent.com/coryetzkorn/state-svg-defs/master/SVG/MI.svg",
  "Ohio",747100, 775610,3.8,"https://raw.githubusercontent.com/coryetzkorn/state-svg-defs/master/SVG/OH.svg",
  "Wisconsin",519720,210550,-1.8,"https://raw.githubusercontent.com/coryetzkorn/state-svg-defs/master/SVG/WI.svg",
  "East North Central Division",11250000,11880000,5.6,NA,
) %>%
  mutate(bar1= `2016`,
         bar2= `2021`) %>%
  select("vec",State,`2016`,bar1,`2021`,bar2,"% Change")
  
# east_north_central division
test %>%
  gt() %>%
  gt_img_rows(columns=vec, height=40) %>%
  cols_label(vec="", bar1="",bar2="") %>%
  gt_plt_bar_pct(bar1, fill="#ee9b00") %>%
  gt_plt_bar_pct(bar2, fill="#ca6702") %>%
  cols_width(bar1~px(150),
             bar2~px(150)) %>%
  cols_width(State~px(140)) %>%
  cols_align(columns = "% Change", align="center") %>%
  fmt_number(c("2016","2021"), decimals=2, drop_trailing_zeros = T, suffixing = T) %>%
  fmt_percent("% Change", scale_values = F, drop_trailing_zeros = T) %>%
  gt_theme_538() %>%
  tab_style(style = cell_text(color = "black",size=px(15),transform = "capitalize"),
            locations = cells_column_labels(everything())) %>%
  tab_style(
    style = list(
      cell_text(color = "#762A84",weight = "bold")
      ),
    locations = cells_body(
      columns = `% Change`,
      rows = `% Change` < 0
    )
  ) %>%
   tab_style(
    style = list(
      cell_text(color = "#1B7837",weight = "bold")
      ),
    locations = cells_body(
      columns = `% Change`,
      rows = `% Change` > 0
    )
  ) %>%
   tab_style(
    style = list(
      cell_text(size=px(15))
      ),
    locations = cells_body()
  ) %>%
  opt_table_font(font=list(google_font(name="Roboto"))) %>%
  tab_header(title=md("Sample Table 1"),
             subtitle=md("Table created in R using {gt} and {gtExtras}")) %>%
  tab_source_note(source_note = md("<br>Note: Table contains randomly generated data")) %>%
  tab_options(heading.padding = px(5))