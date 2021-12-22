# Data: NBER papers (TidyTuesday Week 40)
# Data source: National Bureau of Economic Research and nberwp package (https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-28/readme.md)

library(tidyverse)
library(glue)
library(gt)
library(gtExtras)

# import data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# joined df
joined_df <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 

# {gt} Table: Working papers count by program and decade

wp = joined_df %>% group_by(program, program_desc, year) %>%
  summarise(n=n_distinct(paper)) %>%
  arrange(year) %>%
  mutate(decade=case_when(between(year,1980,1989)~"1980s",
                          between(year,1990,1999)~"1990s",
                          between(year,2000,2009)~"2000s",
                          between(year,2010,2019)~"2010s"
                          )) %>%
  drop_na() 

wp2 = wp %>% mutate(program=glue::glue("{program_desc} ({program})")) %>%
  group_by(program) %>% mutate(total=sum(n)) %>%
  arrange(year, program) %>%
  mutate(spark=list(n)) %>%
  select(program, spark, total) %>%
  distinct()

wp3 = wp %>% mutate(program=glue::glue("{program_desc} ({program})")) %>%
  group_by(program, decade) %>% tally(n) %>% 
  ungroup() %>%
  pivot_wider(names_from = decade, values_from=n) %>%
  mutate_if(is.numeric, list(~replace_na(., 0)))
  
t1 = wp3 %>% inner_join(wp2, by="program") %>%
  select(Program=program, Total=total, "1980s","1990s","2000s","2010s",Trend=spark) %>%
  arrange(desc(Total)) %>%
  gt() %>%
  gt_theme_espn() %>%
  cols_align(Program, align="left") %>%
  gt_plt_dot(Total, Program,palette = "rcartocolor::ag_GrnYl", max_value=5246) %>%
  gtExtras::gt_sparkline(Trend) %>%
  tab_options(table.font.size = 12.5,
              heading.subtitle.font.size = 14) %>%
  gt_color_box(`1980s`, domain=2:786) %>%
  gt_color_box(`1990s`, domain=2:797) %>%
  gt_color_box(`2000s`, domain=132:1647) %>%
  gt_color_box(`2010s`, domain=200:2424) %>%
  tab_header(title="Economic Papers", subtitle="Working papers count by program and decade") %>%
  tab_source_note(source_note="TidyTuesday Week 40 | Data source: National Bureau of Economic Research (NBER) by way of the nberwp package by Ben Davies") 
  
t1