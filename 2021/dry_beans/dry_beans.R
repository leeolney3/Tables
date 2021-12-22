# Data: Multiclass Classification of Dry Beans (MakeoverMonday 2021w14)
# Data source: UCI Machine Learning Repository, https://data.world/makeovermonday/2021w14 

library(httr)
library(readxl)
library(tidyverse)
library(gt)
library(gtExtras)
library(wesanderson)
library(rcartocolor)

# get data
GET("https://query.data.world/s/7f4qfr7ayt4isczwyrmclzr5xtkpx7", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)


# {gt} Table 1: Summary with bullet chart and histogram
# reference: https://gist.github.com/jthomasmock/923604deb65682b0364c9220a85ddb36
# reference: https://twitter.com/erdirstats/status/1444706781033345030/photo/1

df %>% mutate(target_col = mean(MajorAxisLength, na.rm=T)) %>%
  group_by(Class, target_col) %>%
  summarise(n=n(), 
            min=min(MajorAxisLength, na.rm=T),
            med = median(MajorAxisLength, na.rm = TRUE),
            max = max(MajorAxisLength, na.rm = TRUE),
            sd = sd(MajorAxisLength, na.rm = TRUE),
            mean = mean(MajorAxisLength, na.rm = TRUE),
            bullet = mean(MajorAxisLength, na.rm = TRUE),
            histogram = list(MajorAxisLength),
            .groups="drop"
            ) %>%
  ungroup() %>%
  select(Class, n, min, med, max, sd, mean, bullet,target_col, histogram) %>%
  gt() %>%
  gt_plt_bullet(column=bullet, target=target_col,width=20,colors=c("#ddbea9","black")) %>%
  # histogram and density plots
  gtExtras::gt_sparkline(
    histogram,
    type = "histogram",
    line_color = "#474747FF",
    fill_color = "#474747FF",
    bw = 0.75,
    same_limit = TRUE
  ) %>%
  # format decimals
  fmt_number(columns = min:max, decimals = 0) %>%
  fmt_number(columns = sd:mean, decimals = 1) %>%
  # header
  tab_header(
    title = md("**Dry Beans Length by Type**"),
    subtitle = md("Major axis length, distance between the ends of the longest line that can be drawn from a bean")
  ) %>%
  # create groups of columns
  tab_spanner(
    label = "Summary statistics",
    columns = n:mean
  ) %>%
  tab_spanner(
    label = "Graphics",
    columns = bullet:histogram
  ) %>%
  # change column names to appear in the table
  cols_label(
    Class = html("Type"),
    n = html("Count"),
    min = html("Min"),
    med = html("Median"),
    max = html("Max"),
    sd = html(("St. dev.")),
    bullet = html(("Mean Bullet")),
  ) %>%
  # footnotes
  tab_footnote(
    footnote = "KOKLU, M. and OZKAN, I.A., (2020), “Multiclass Classification of Dry Beans Using Computer Vision and Machine Learning Techniques.” Computers and Electronics in Agriculture, 174, 105507.
DOI: https://doi.org/10.1016/j.compag.2020.105507",
    locations = cells_title(groups = "subtitle")) %>%
  tab_footnote(
    footnote = md("**Bullet** chart of class mean compared to overall mean"),
    locations = cells_column_labels(columns = target_col)
  ) %>%
  tab_footnote(
    footnote = md("**Bullet** chart of class mean compared to overall mean"),
    locations = cells_column_labels(columns = bullet)
  ) %>%
  # add source
  tab_source_note(source_note="Data source: UCI Machine Learning Repository") %>%
  gt_plt_dot(
    n,
    Class,
    palette=c("#adb5bd","#ced4da","#979dac","#001233","#7d8597","#5c677d","#33415c")) %>%
  # set col width
  cols_width(Class ~ px(150)) %>%
  cols_width(n:mean ~ px(75)) %>%
  cols_width(histogram ~ px(90)) %>%
  gt_theme_nytimes() %>%
  # table options
  tab_options(footnotes.font.size = "11px",
              source_notes.font.size = "11px",
              data_row.padding = px(10),
              table.font.size = "14px",
              heading.subtitle.font.size = "14px",
              column_labels.border.bottom.width = px(0.5)) %>%
  gt_color_box(columns=min, domain=184:426,palette="wesanderson::Zissou1") %>%
  gt_color_box(columns=med, domain=247:591,palette="wesanderson::Zissou1") %>%
  gt_color_box(columns=max, domain=308:739,palette="wesanderson::Zissou1") %>%
  cols_align(
    align = "center",
    columns = n:histogram
  )
  
# {gt} Table 2: Summary with histogram and density plot
# reference: https://twitter.com/erdirstats/status/1444706781033345030/photo/1
# reference: https://gist.github.com/jthomasmock/6b811725989f78a9fa3454323502126b

df %>% group_by(Class) %>%
  summarise(n=n(), 
            min=min(MajorAxisLength, na.rm=T),
            med = median(MajorAxisLength, na.rm = TRUE),
            max = max(MajorAxisLength, na.rm = TRUE),
            mean = mean(MajorAxisLength, na.rm = TRUE),
            sd = sd(MajorAxisLength, na.rm = TRUE),
            histogram = list(MajorAxisLength),
            density=list(MajorAxisLength),
            .groups="drop"
            ) %>% 
  arrange(desc(med)) %>%
  gt() %>%
  # histogram and density plots
  gtExtras::gt_sparkline(
    histogram,
    type = "histogram",
    line_color = "#474747FF",
    fill_color = "#474747FF",
    bw = 0.75,
    same_limit = TRUE
  ) %>%
  gtExtras::gt_sparkline(
    density,
    type = "density",
    line_color = "#006d77",
    fill_color = "#d9ed92",
    bw = 0.75,
    same_limit = TRUE
  ) %>%
  # format decimals
  fmt_number(columns = min:max, decimals = 0) %>%
  fmt_number(columns = mean:sd, decimals = 1) %>%
  # header
  tab_header(
    title = md("**Multiclass Classification of Dry Beans**"),
    subtitle = md("Major axis length, distance between the ends of the longest line that can be drawn from a bean")
  ) %>%
  # create groups of columns
  tab_spanner(
    label = "Summary statistics",
    columns = n:sd
  ) %>%
  tab_spanner(
    label = "Graphics",
    columns = histogram:density
  ) %>%
  # change column names to appear in the table
  cols_label(
    n = html("Count"),
    min = html("Min"),
    med = html("Median"),
    max = html("Max"),
    sd = html(("St. dev.")),
    density = html(("Density Plot")),
  ) %>%
  # footnotes
  tab_footnote(
    footnote = "KOKLU, M. and OZKAN, I.A., (2020), “Multiclass Classification of Dry Beans Using Computer Vision and Machine Learning Techniques.” Computers and Electronics in Agriculture, 174, 105507.
DOI: https://doi.org/10.1016/j.compag.2020.105507",
    locations = cells_title(groups = "subtitle")) %>%
  # add source
  tab_source_note(source_note="Data source: UCI Machine Learning Repository") %>%
  gt_plt_dot(
    n,
    Class,
    palette="rcartocolor::Bold") %>%
  # set col width
  cols_width(Class ~ px(150)) %>%
  gt_theme_nytimes() %>%
  # table options
  tab_options(footnotes.font.size = "12px",
              source_notes.font.size = "12px",
              data_row.padding = px(10),
              heading.subtitle.font.size = "14px") %>%
  gt_color_box(columns=min, domain=184:426,palette="wesanderson::Zissou1") %>%
  gt_color_box(columns=med, domain=247:591,palette="wesanderson::Zissou1") %>%
  gt_color_box(columns=max, domain=308:739,palette="wesanderson::Zissou1") %>%
  cols_align(
    align = "center",
    columns = n:sd
  )
