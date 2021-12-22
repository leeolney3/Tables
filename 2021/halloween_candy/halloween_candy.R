# Data: Halloween Candy Ranking
# Data source: FiveThirtyEight, https://fivethirtyeight.com/videos/the-ultimate-halloween-candy-power-ranking/) by way of [Data Is Plural](https://www.data-is-plural.com/archive/2021-10-20-edition/

library(tidyverse)
library(gt)
library(gtExtras)

candy = read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv")

skimr::skim(candy)

# {gt} Table
candy %>%
  arrange(desc(winpercent)) %>%
  slice(1:10) %>%
  mutate(competitorname= str_replace(competitorname,"Õ","'"),#clean name
         win=round(winpercent,2), sugar=round(sugarpercent,2), price=round(pricepercent,2),
         sugarpercent= sugarpercent*100, pricepercent=pricepercent*100,
         target_col_sugar=50, target_col_price=50, rank=row_number()) %>%
  select(rank, competitor= competitorname, win, winpercent, sugar, sugarpercent, target_col_sugar, price, pricepercent, 
         target_col_price) %>%
  gt() %>%
  fmt_symbol_first(column = c(win), suffix = "%") %>%
  gt_plt_bar_pct(column = winpercent, scaled = TRUE, fill = "#036666", background = "#99e2b4") %>%
  cols_width(c(winpercent, sugarpercent, pricepercent)~px(120)) %>%
  gt_plt_bullet(column = sugarpercent, target = target_col_sugar, width = 30,
               colors = c("#ee9b00", "black"))  %>%
  gt_plt_bullet(column = pricepercent, target = target_col_price, width = 30,
               colors = c("#9597BF", "black")) %>%
  cols_label(winpercent = "",
             target_col_sugar="",
             target_col_price="") %>%
  cols_align(columns=c(win, sugar, price), align="left") %>%
  gt_theme_nytimes() %>%
  tab_style(
    style = list(
      "font-variant: small-caps;",
      cell_text(weight = "normal", size=px(18))
    ),
    locations = cells_body(columns = competitor)
  ) %>%
  tab_header(title = "Halloween Candy Ranking",
             subtitle="Top 10 halloween candy from readers vote on head-to-head matchups among 85 confections. The survey conducted by FiveThirtyEight show that Reese’s Peanut Butter Cups and their spinoffs took four of the top 10 spots.") %>%
  tab_source_note(source_note = "Data source: FiveThirtyEight") %>%
  tab_footnote(
    footnote = "The overall win percentage according to 269,000 matchups.",
    locations = cells_column_labels(columns = win)) %>%
  tab_footnote(
    footnote = "The percentile of sugar it falls under within the data set.",
    locations = cells_column_labels(columns = sugar)) %>%
  tab_footnote(
    footnote = "The unit price percentile compared to the rest of the set.",
    locations = cells_column_labels(columns = price)) %>%
  tab_style(style = cell_text(size = px(13), color = "grey20", style="italic"), 
        locations = cells_footnotes()) %>%
  tab_style(style=cell_text(color="black"), locations=cells_column_labels())
