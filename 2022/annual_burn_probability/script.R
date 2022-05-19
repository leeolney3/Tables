# Properties burn probability, national overview
# Data from First Street Foundation 5th National Assessment: Fueling the Flames (May 16, 2022) pp14, available at: https://firststreet.org/research-lab/published-research/article-highlights-from-fueling-the-flames/
# Exercise: trying out gt_two_column_layout()

# Libraries
library(tidyverse)
library(gt)
library(gtExtras)

caption = "Source: First Street Foundation"

# Table 1 data 
t1_title= "Top 20 counties with the highest number of properties with at least 0.03%<br>annual burn probability in 2022"

b2 = c("1 Riverside County, CA", "2 Maricopa County, AZ", "3 Los Angeles County, CA", "4 San Bernardino County, CA", "5 Polk County, FL", "6 Pima County, AZ", "7 San Diego County, CA", "8 Kern County, CA", "9 Ocean County, NJ", "10 Pasco County, FL", "11 Tarrant County, TX", "12 Clark County, NV", "13 El Paso County, CO", "14 Hillsborough County, FL", "15 Lee County, FL", "16 Brevard County, FL", "17 Pinal County, AZ", "18 Mohave County, AZ", "19 Volusia County, FL", "20 Valencia County, NM")

b3 = c(684400, 683300, 514500, 471700, 335100, 283200, 277400, 236300, 220000, 210500, 210100, 208200, 200100, 198200, 197900, 194000, 193300, 191000, 189600, 184500)

b4 = c(77.2, 43.3, 24.6, 57.4, 87.8, 66.4, 37.3, 58.2, 52.3, 79.3, 32.3, 27.3, 80.1, 42.1, 42.3, 65.2, 80.4, 73.5, 71, 93.6)

b5 = c(0.8, 12, 13.9, 5, 0.8, 9.4, 22, 12.1, 19.4, 1.9, 34.6, 38.7, 1.9, 9.3, 14.8, 1.9, 5.4, 11.9, 5, 2.5)

t1 = tibble("County"=b2,
       "# of properties with at least 0.03% risk this year"=b3,
       x0=b3,
       "% properties with at least 0.03% risk this year"=b4,
       x1=b4,
       "% difference in properties with at least 0.03% risk over 30 years"=b5,
       x2=b5) %>%
  mutate(Rank=row_number(),
         County=gsub('[0-9.]','',County)) %>%
  relocate(Rank, .before=County)
  
# Table 2 data
t2_title = "Top 20 counties with the highest percent of properties with at least 0.03%<br>annual burn probability in 2022"

c2 = c("1 Los Alamos County, NM", "2 Mason County, TX", "3 Harding County, NM", "4 Colfax County, NM", "5 Gillespie County, TX", "6 Menard County, TX", "7 Hooker County, NE", "8 Arthur County, NE", "9 San Miguel County, NM", "10 Guadalupe County, NM", "11 Custer County, SD", "12 Kimble County, TX", "13 Carson City, NV", "14 Winkler County, TX", "15 McPherson County, NE", "16 Santa Fe County, NM", "17 Baca County, CO", "18 Gila County, AZ", "19 Coryell County, TX", "20 Schleicher County, TX")

c3 = c(8300, 6300, 5200, 24100, 27000, 5000, 1800, 1100, 28100, 700, 12500, 8600, 19600, 6100, 1600, 74500, 600, 31400, 28200, 4100)

c4 = c(98.9, 98.8, 98, 97.9, 97.9, 97.8, 97.8, 97.8, 97.7, 97.5, 97.3, 97.1, 96.9, 96.9, 96.9, 96.6, 96.6, 96.5, 96.5, 96.5)

c5 = c(0, 0, 0.5, 0.1, 0, 0, 0.4, 0.1, 0.3, 0, 0.4, 0.7, 0, 0.1, 0, 0.4, 0, 1.8, 0, 0)

t2 = tibble("County"=c2,
       "# of properties with at least 0.03% risk this year"=c3,
       "% properties with at least 0.03% risk this year"=c4,
       "% difference in properties with at least 0.03% risk over 30 years"=c5) %>%
  mutate(Rank=row_number(),
         County=gsub('[0-9.]','',County)) %>%
  relocate(Rank, .before=County)
  
# two column layout table
# reference: https://jthomasmock.github.io/gtExtras/reference/gt_two_column_layout.html
tab1 = t1 %>% gt() %>% 
  gt_theme_dark() %>%
  tab_options(table.font.size = px(12),
              heading.title.font.size = px(13)) %>%
  cols_width(3:5~px(100)) %>%
  tab_header(title=md(t2_title),subtitle=caption) %>%
  fmt_number(column=3, decimals = 0, sep_mark = " ") %>%
  tab_style(style=cell_text(size=px(11), transform="capitalize"), locations=cells_column_labels(everything())) %>%
  fmt_percent(4:5, scale_values = F, decimals=1) %>%
  tab_style(style = list(cell_fill(color = "#5a5a5a")),locations = cells_body(columns = 3)) %>%
  tab_style(style = list(cell_fill(color = "#5a5a5a")),locations = cells_column_labels(columns = 3))

tab2 = t2 %>% gt() %>% 
  gt_theme_dark() %>%
  tab_options(table.font.size = px(12),
              heading.title.font.size = px(13)) %>%
  cols_width(3:5~px(100)) %>%
  tab_header(title=md(t2_title),subtitle=caption) %>%
  fmt_number(column=3, decimals = 0, sep_mark = " ") %>%
  tab_style(style=cell_text(size=px(11), transform="capitalize"), locations=cells_column_labels(everything())) %>%
  fmt_percent(4:5, scale_values = F, decimals=1) %>%
  tab_style(style = list(cell_fill(color = "#5a5a5a")),locations = cells_body(columns = 4)) %>%
  tab_style(style = list(cell_fill(color = "#5a5a5a")),locations = cells_column_labels(columns = 4))

listed_tables <- list(tab1, tab2)

gt_two_column_layout(listed_tables)


