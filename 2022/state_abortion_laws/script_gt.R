# 2022-07-01
# Trying out new version of {gt}: version [0.6.0](https://www.rstudio.com/blog/changes-for-the-better-in-gt-0-6-0/)
# Table of State abortion laws, data from [Guttmacher Institute](https://www.guttmacher.org/state-policy/explore/overview-abortion-laws) by way of [Data Is Plural](https://www.data-is-plural.com/archive/2022-06-29-edition/)

# Load libraries
library(tidyverse)
library(gt)

# Import data
df = readr::read_csv("data/state_abortion_laws.csv")

# Transform uppercase col names to title
names(df) = stringr::str_to_title(names(df)) 

# Table
t1 =df %>% 
  gt() %>%
  opt_row_striping() %>%
  sub_missing(
    columns = everything(),
    missing_text = "") %>%
  tab_options(table.width = pct(100),
              table.font.size = px(11),
              table.font.color = "grey40",
              column_labels.font.size = px(10),
              data_row.padding = px(3),
              column_labels.border.bottom.color = "grey30",
              column_labels.border.bottom.width = px(2),
              row.striping.background_color = "#fafafa",
              table_body.hlines.color = "#f6f7f7",
              heading.subtitle.font.size = px(12.5),
              heading.title.font.size = px(18),
              container.width = px(1280),
              column_labels.padding = px(1.5),
              source_notes.padding = px(5)
              ) %>%
  tab_style(style = cell_text(font = google_font("Libre Franklin"),weight = 700, color="black"),
      locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(color="black"),
      locations = cells_title(groups = "subtitle")) %>%
  tab_style(style = cell_text(font = google_font("Karla"),weight =  400),
      locations = cells_body()) %>%
  tab_style(style = cell_text(color="black"),
      locations = cells_body()) %>%
  tab_style(style = cell_text(color="grey50"),
      locations = cells_source_notes()) %>%
  tab_style(style = list(cell_fill(color = "#EFC56C")),
    locations = cells_body(rows = 52)) %>%
  tab_spanner(str_to_title("PUBLIC FUNDING OF ABORTION"), 7:8) %>%
  tab_spanner(str_to_title("PROVIDERS MAY REFUSE TO PARTICIPATE"), 10:11) %>%
  tab_spanner(str_to_title("MANDATED COUNSELING INCLUDES INFORMATION ON:"), 12:14) %>%
  tab_header(title="Overview of State Abortion Law", 
             subtitle="As of June 30, 2022  •  Source: Guttmacher Institute") %>%
  tab_source_note(md("▼ Permanently enjoined; law not in effect.<br>*   Exception in case of threat to the patient's physical health.<br>†   Exception in case of rape or incest.<br>‡   Exception in case of life endangerment only. A 2016 New York Attorney General opinion determined that the state’s law conflicts with U.S. Supreme Court rulings on abortion, and that abortion care is permissible under the U.S.<br> Constitution to protect a pregnant person's health, or when the fetus is not viable.<br>Ω  Exception in case of fetal abnormality.<br>ϴ  Despite a court order, the state Medicaid program does not pay for medically necessary abortions.<br>ξ   Only applies to surgical abortion. In New Mexico, some but not all advanced practice clinicians may provide medication abortion.<br>Ф  Law limits abortion provision to OB/GYNs.<br>€ A court has temporarily blocked enforcement of a Mississippi law that would have banned abortion at 15 weeks after the patient’s last menstrual period.<br>§   Enforcement temporarily enjoined by court order; policy not in effect.<br>þ   Both parents must consent to the abortion.<br>ξ   Specified health professionals may waive parental involvement in certain circumstances.<br>◊   In South Dakota, the waiting period excludes weekends or annual holidays and in Utah the waiting period is waived in cases of rape, incest, fetal defect or if the patient is younger than 15.<br>β  Parental involvement required for minors under the age of 16.<br>")) %>%
  cols_width(1~px(50),
             3:5~px(90),
             12:14~px(70),
             16~px(110),
             c(2,6:11,15)~px(80))
             
# Save table
gtsave(t1, "t1.png")             
