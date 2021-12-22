# Data: Formula 1 Races (TidyTuesday 2021w37)
# Data source: Ergast API, https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-07/readme.md

library(tidyverse)
library(gt)
#remotes::install_github("jthomasmock/gtExtras")
library(gtExtras)

races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv',show_col_types = FALSE)
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv',show_col_types = FALSE)
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv',show_col_types = FALSE)
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv',show_col_types = FALSE)

# dataset of results by race, driver and season
# reference: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-07/readme.md
driver_results_df <- driver_standings %>% 
  left_join(races, by = "raceId") %>% 
  rename(driver_url = url) %>% 
  left_join(drivers, by = "driverId") 
  
# get top 10 drivers with most wins (position==1)
wins10 = drivers %>% 
  left_join(results, by = "driverId") %>% 
  left_join(races, by = "raceId") %>% 
  filter(position == "1") %>%
  mutate(Driver = paste(forename, surname)) %>%
  count(Driver, sort=T) %>% slice(1:10)
wins10

# year with wins/losses and points
d1 = driver_results_df %>% 
  mutate(Driver = paste(forename, surname)) %>%
  select(Driver, year, wins, position, points) %>%
  group_by(Driver,year) %>%
  summarise(wins = sum(wins), points=sum(points)) %>%
  filter(Driver %in% wins10$Driver) %>%
  ungroup() %>%
  mutate(wins_1 = ifelse(wins==0,0,1)) %>%
  group_by(Driver) %>%
  summarise(
    Wins = length(wins_1[wins_1==1]),
    Losses = length(wins_1[wins_1==0]),
    Outcomes = list(wins_1), .groups = "drop",
    Points= sum(points),
    Spark = list(points),
    ) 

d2 = d1 %>% left_join(wins10, by="Driver") %>% #add column with total wins 
	select(Driver,"Wins"=n, "Years with Wins" = Wins, "Years with Losses"=Losses,
    Outcomes, Points, "Sparkline" = Spark) %>% 
    arrange(desc(Wins)
                   
# table using {gtExtras} package (https://jthomasmock.github.io/gtExtras/)
d3 %>%
  gt() %>%
  gt_plt_winloss(Outcomes) %>%
  gt_kable_sparkline(Sparkline, height=45) %>%
  gt_theme_espn() %>%
  tab_header(title="Top 10 Formula 1 Drivers with the most wins") %>%
  tab_source_note(source_note = "Data source: ergast.com/mrd/db") %>%
  cols_align(
  align = "center",
  columns = c(Wins,`Years with Wins`,`Years with Losses`)) %>%
  cols_width(`Years with Wins` ~px(90),
             `Years with Losses` ~px(90),) 


  
