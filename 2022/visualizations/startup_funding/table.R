library(tidyverse)
library(gt)
library(gtExtras)

tabdf = tribble(
  ~Country,~Company,~Funding,~Type,~Date,~Product,
  "US","Litter Otter",4.2,"pre-seed","May 2021","Children's mental health",
  "US","Brave Care",10,"series A","May 2021","Kids' urgent care",
  "US","A Kids Book About",7,"series A","April 2021","Children's literature",
  "India","PlayShifu",17,"series B","April 2021","STEM toys",
  "US","Gabb Wireless",14,"series A","April 2021","Safe cell phone for kids",
  "US","Healthy Root Dolls",1,"pre-seed","February 2021","Inclusive dolls",
  "US","Toybox Labs",1.6,"seed","February 2021","3D printing for kids",
  "US","Tiny Earth Toys",0.275,"seed","February 2021","Educational toy subscriptions",
) %>%
  mutate(flag = case_when(Country=="US"~"data/us.svg",Country=="India"~"data/in.svg"),
         Type= str_to_title(Type),
         Funding=(1000000*Funding),
         ) %>%
  relocate(flag,.after =Country) %>%
  relocate(Type,.before="Funding") %>%
  mutate(percentile = Funding/max(Funding)*100)
  
tabdf %>% gt() %>%
  gt_theme_538() %>%
  gt_img_rows(flag,img_source = "local", height=25) %>%
  cols_label(flag="",Country="", percentile="Percentile (funding)") %>%
  tab_options(table.font.size = px(15)) %>%
  tab_header(title="2021 Startup Funding in the Children's Products and Service Space") %>%
  fmt_currency(columns="Funding", drop_trailing_dec_mark = TRUE, suffixing=T, decimals = 1) %>%
  tab_style(style=list(cell_text(weight="bold")),
            locations=cells_body(columns=Company)) %>%
  gt_color_rows("Funding", palette = "ggsci::indigo_material") %>%
  tab_style(style = list(cell_text(color="#b83229")),
    locations = cells_body(columns = Type,rows = Type=="Pre-Seed")) %>%
  tab_style(style = list(cell_text(color="#019176")),
    locations = cells_body(columns = Type,rows = Type=="Seed")) %>%
  tab_style(style = list(cell_text(color="#ff8902")),
    locations = cells_body(columns = Type,rows = Type=="Series A")) %>%
  tab_style(style = list(cell_text(color="#15607a")),
    locations = cells_body(columns = Type,rows = Type=="Series B")) %>%
  cols_hide(Country) %>%
  tab_style(style=list(cell_text(style="italic", color="grey20")),
            locations=cells_body(columns=Product))  %>%
  gt_plt_percentile(column=percentile, width=35) %>%
  cols_align(columns=c(percentile,Date), align="center") %>%
  tab_style(style = cell_text(font = google_font("IBM Plex Mono"),size = px(19),weight = 500),
      locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(color = "grey50",font = google_font("IBM Plex Mono"), transform = "uppercase"),locations = cells_column_labels(everything()))  %>%
  tab_style(style = cell_text(font = google_font("IBM Plex Mono"),weight =  400,size=px(13)),
              locations = cells_body()) %>%
  cols_width(Type ~ px(85),
             Company~px(160),
             Date ~px(130)) %>%
  tab_options(data_row.padding = px(7),
              heading.padding = px(7),
              )