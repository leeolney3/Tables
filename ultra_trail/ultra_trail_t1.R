# Data: Ultra Trail Running (TidyTuesday 2021w44)
# Data source: Benjamin Nowak and International Trail Running Association, https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-26/readme.md

library(tidyverse)
library(gt)
library(gtExtras)

# import data
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# finishers df
df = ultra_rankings %>% 
  left_join(race, by="race_year_id") %>% 
  filter(!is.na(gender),!is.na(time_in_seconds), !is.na(distance), distance != 0, !is.na(rank)) %>% 
  mutate(gender=recode(gender, "M"="Man", "W"="Woman"),
         time_in_hours = time_in_seconds/3600,
         sp = distance/time_in_hours,
         runner=str_to_title(runner),
         year=year(date)) 

# finishers df without speed outliers
df1 = df %>%
  mutate(zscore_sp = (sp- mean(sp))/ sd(sp)) %>% #get zscore
  filter(between(zscore_sp,-3,3)) #drop outliers
  
# {gt} Table 1: Speed summary

sum_sp = df1 %>% filter(distance>160) %>% arrange(sp) %>%
  group_by(gender) %>%
  summarise(n=n(),min=min(sp), median=median(sp), max=max(sp), st.dev=sd(sp), histogram= list(sp), 
            skew=skewness(sp),kurtosis = kurtosis(sp), .groups="drop") %>%
  mutate(grp = "Speed (km/h)")

sum_dist = df1 %>% filter(distance>160) %>% arrange(distance) %>%
  group_by(gender) %>%
  summarise(n=n(),min=min(distance), median=median(distance), max=max(distance), st.dev=sd(distance), 
            histogram= list(distance), skew=skewness(distance),kurtosis = kurtosis(distance),.groups="drop") %>%
  mutate(grp = "Distance (km)")

sum1 = rbind(sum_sp, sum_dist)

sum1 %>% mutate(across(where(is.numeric), round, 2)) %>% 
  group_by(grp) %>% 
  gt() %>% 
  gt_theme_nytimes() %>%
  tab_header(title = "Ultra Trail Running Finishers",
             subtitle="Summary of speed and distance by gender for races 100 miles or more, from 2012-01-14 to 2021-09-03") %>%
  # histogram and density plots
  gtExtras::gt_sparkline(
    histogram,
    type = "histogram",
    line_color = "#474747FF",
    fill_color = "#474747FF",
    bw = 0.75, same_limit = FALSE
  ) %>%
  cols_width(gender:st.dev ~ px(80),
             skew:kurtosis ~ px(70))

# {gt} Table 2: Most First Place Ultra Runners
# reference: https://bjnnowak.netlify.app/2021/10/04/r-beautiful-tables-with-gt-and-gtextras/
# reference: https://twitter.com/jakepscott2020/status/1453154730600017932/photo/1

# get names of 10 runners with the most first places
runner10 = df %>% filter(rank==1) %>% count(runner, sort=T) %>% slice(1:10) %>% pull(runner)
df10 = df %>% filter(runner %in% runner10) 

# gt_plt_winloss data
winloss = df10 %>% 
  mutate(wins_1 = ifelse(rank==1,1,0)) %>%
  group_by(runner) %>% arrange(runner,date) %>%
  summarise(outcomes = list(wins_1))

# gt_plt_bar_stack data
bar = df %>% filter(runner %in% runner10) %>%
  mutate(rank_grp = case_when(rank<=3 ~"Top 3",
                              rank>3~"Other Rank"),
         rank_grp= fct_rev(rank_grp)) %>%
  group_by(runner,rank_grp,.drop = FALSE) %>% tally() %>%
  ungroup() %>%
  group_by(runner) %>%
  summarise(bar=list(n))
  
# table data
tab = df %>% filter(runner %in% runner10) %>%
  mutate(wins_1 = ifelse(rank==1,1,0)) %>%
  group_by(runner, nationality) %>%
  summarise(first_place=length(wins_1[wins_1==1]),completed=n(), 
            min_date=min(date), max_date=max(date)) %>%
  ungroup() %>%
  left_join(winloss, by="runner") %>%
  left_join(bar, by="runner") %>%
  select(1:4, 7:8, 5:6) %>%
  arrange(desc(first_place)) %>%
  slice(1:5)
  
# {gt} table
tab %>% gt() %>%
  gt_theme_nytimes() %>%
  gt_merge_stack(col1 = runner, col2 = nationality) %>%
  gt_fa_repeats(column = first_place, palette = "#013369",name = "trophy",align = "left") %>%
  gt_plt_winloss(column=outcomes,max_wins=23) %>%
  gt_plt_bar_stack(column=bar, position="stack", labels=c("Top 3 Places","Other Ranks"),
                   palette=c("#006466","black","black"), width=55) %>%
  tab_header(title = "Most First Place Ultra Runners",
             subtitle="100 miles races from 2012-01-14 to 2021-09-03") %>%
  tab_source_note(source_note = md("Data from Benjamin Nowak by way of ITRA<br>Inspired by @jakepscott2020 and @BjnNowak")) %>%
  cols_label(first_place = "First places count",
             completed="Race completed",
             outcomes="First place outcomes",
             min_date = "Earliest race",
             max_date = "Latest race") %>%
  cols_align(columns=completed, align=c("center")) %>%
  cols_align(columns=bar, align=c("left")) %>%
  cols_width(first_place ~ px(150)) %>%
  tab_options(source_notes.font.size = 12,
              source_notes.padding = 10)
  