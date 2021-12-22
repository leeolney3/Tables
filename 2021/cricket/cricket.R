# Data: World Cup Criket (TidyTuesday 2021w49)
# Data source: ESPN Cricinfo by way of Hassanasir, https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-11-30/readme.md

library(tidyverse)
library(lubridate)
library(gt)
library(gtExtras)

# import data
matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

# prepare data
m1 = matches %>%
  mutate(date = mdy(match_date)) %>%
  mutate(match_date2 = case_when(is.na(date)~ paste0(str_sub(match_date, start=1L, end=6L), 
                                       str_sub(match_date, start=10L, end=15L)),
                                 TRUE~match_date)) %>%
  mutate(mdate = mdy(match_date2),
         myear = year(mdate)) 
         
m2 = m1 %>% 
  mutate(winner=case_when(winner=="Pakistan awarded the match (opposition conceded)" ~"Pakistan",
                          TRUE~winner)) %>%
  count(winner, sort=T) %>%
  slice(1:10)

m3 = m1 %>% count(winner, mdate) %>%
  filter(!grepl('XI', winner),
         !grepl('Match tied', winner)) %>%
  filter(winner!="No result") %>%
  mutate(winner=case_when(winner=="Pakistan awarded the match (opposition conceded)" ~"Pakistan",
                          TRUE~winner)) 

m4 = m3 %>%
  complete(mdate, winner) %>%
  mutate(n=replace_na(n,0)) %>%
  group_by(winner) %>% 
  summarise(timeline = list(n)) %>%
  filter(winner %in% m2$winner)

# {gt} Table 1: 10 Teams with the most wins from Jan 01, 1996 to Dec 31, 2005
tab = m2 %>% left_join(m4) 

t1 = tab %>%
  gt() %>%
  gt_theme_nytimes() %>%
  gt_sparkline(timeline,width=120, label=FALSE,
               range_colors = c("#353535", "blue"),
               line_color = "#353535") %>%
  cols_label(winner="Team", 
             n="Total wins", 
             timeline=html("&nbsp &nbsp &nbsp &nbsp Wins Timeline from 1996-01-01 to 2005-12-31")) %>%
  tab_header(title="Cricket Winners", 
             subtitle="10 Teams with the most wins from Jan 01, 1996 to Dec 31, 2005") %>%
  tab_source_note(source_note="#TidyTuesday Week 49, data from ESPN Cricinfo by way of Hassanasir.") %>%
  tab_options(source_notes.padding = px(10),
              source_notes.font.size = px(11)) %>%
  tab_style(
    style = list(cell_text(align = "left")),
    locations = cells_column_labels(columns=c(timeline))
  ) 
  
t1

# {gt} Table 2: Nine teams with most matches played, from 1996 to 2005
played = m1 %>%
  pivot_longer(cols=c(team1, team2), values_to="team") %>%
  count(team) %>% rename(played=n)

wins = m1 %>%
  count(winner) %>% rename(wins=n, team=winner)

tied_df = m1 %>%
  filter(winner=="Match tied" | winner=="Match tied (D/L method)") %>%
  pivot_longer(cols=c(team1, team2), values_to="team") %>%
  count(team) %>% rename(tied = n)

year_df = m1 %>%
  pivot_longer(cols=c(team1, team2), values_to="team") %>%
  group_by(team) %>%
  summarise(from=min(myear), to=max(myear)) %>%
  mutate(year= glue::glue("{from}-{to}")) %>%
  select(team, year)
  
joined = played %>%
  left_join(wins, by="team") %>%
  left_join(tied_df, by="team") %>%
  replace(is.na(.), 0) %>%
  mutate(losses=played-wins-tied) %>%
  left_join(year_df, by="team") %>%
  slice_max(played, n=9) %>%
   mutate("win%" = round(wins/played,4),
         "loss%" = round(losses/played,4),
         "tied%" = round(tied/played,4),
         ) 
         
pct = joined %>% select(team, 7:8) %>%
  pivot_longer(!team) %>%
  group_by(team) %>%
  summarise(list_pct=list(value))

count = joined %>% select(team, wins, losses) %>%
  pivot_longer(!team) %>%
  group_by(team) %>%
  summarise(list_count=list(value))
  
table2 = joined %>% 
  left_join(count,by = "team") %>%
  left_join(pct,by = "team") %>%
  select(team, played, list_count, list_pct, tied, "tied%") %>%
  gt() %>%
  gt_theme_nytimes() %>%
  fmt_percent(column="tied%") %>%
  gt_plt_bar_stack(column=list_count,
                   width=60,
                   labels = c("Wins", "Losses"),
                   palette=c("#219ebc","#fd9e02"),
                   position="stack") %>%
  gt_plt_bar_stack(column=list_pct,
                   width=60,
                   position="stack",
                   palette=c("#126782","#fb8500"),
                   labels = c("Wins %", "Losses %"),
                   fmt_fn = scales::percent_format(scale=,accuracy=0.1)) %>%
  gt_plt_dot(played, team,
             palette = "nord::polarnight",
             max_value = 319) %>%
  cols_align(c("tied", "played", "tied%"), align = "center") %>%
   tab_header(title="ICC Cricket World Cup ODI Results", 
             subtitle="Nine teams with most matches played, from 1996 to 2005") %>%
  tab_source_note(source_note="#TidyTuesday Week 49, data from ESPN Cricinfo by way of Hassanasir.") %>%
  tab_options(source_notes.padding = px(10),
              source_notes.font.size = px(12)) %>%
  gt_color_rows(columns="tied%", palette="ggsci::teal_material")