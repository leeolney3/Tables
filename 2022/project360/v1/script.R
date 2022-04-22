# 2022-04-07
# 360project

library(tidyverse)
library(ggtext)
library(geomtextpath)
library(ggstream)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Eczar")
f1="Eczar"

df1 = read_csv("20220405-results/scenario_01_acc.csv") %>% mutate(scenario = "Scenario 1")
df2 = read_csv("20220405-results/scenario_02_acc.csv") %>% mutate(scenario = "Scenario 2")
df3 = read_csv("20220405-results/scenario_03_acc.csv") %>% mutate(scenario = "Scenario 3")

df = rbind(df1,df2,df3) %>% janitor::clean_names() %>%
  select(year, accumulated_number_of_employees_that_left, scenario)
max(df$year)

## Stream plot
df %>% ggplot(aes(year, accumulated_number_of_employees_that_left, fill=scenario)) +
  geom_stream(bw=.4) +
  scale_fill_manual(values=c("#4286ED","#00C4A7","#8A80FF")) +
  geom_vline(
      data = tibble(x = c(0, seq(100, 300, by = 100))),
      aes(xintercept = x),
      inherit.aes = F, 
      color = "grey88", 
      size = .4,
      linetype = "dotted"
    ) +
  coord_cartesian(expand=F) +
  scale_y_continuous(position="right") +
  cowplot::theme_minimal_vgrid(10.5) +
  theme(text=element_text(family=f1),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        plot.title=element_text(size=13),
        panel.grid=element_line(size=.4),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(margin=margin(l=7)),
        axis.line.y=element_blank(),
        axis.ticks=element_line(size=.4),
        axis.ticks.length.y=unit(.25, "cm"),
        legend.title=element_blank(),
        legend.position=c(0.085,0.2)) + 
  labs(x="Year",
       title="Accumulated number of employees that left")
       
## Line plot
df %>%
  mutate(lab = glue::glue("**{scenario}**")) %>%
  ggplot(aes(year, accumulated_number_of_employees_that_left, color=scenario)) +
  geom_line(size=1.5) +
  geom_vline(xintercept = max(df$year), color="grey80", linetype="dashed") +
  geom_textpath(aes(label=lab),size=4.3, vjust = -0.5, hjust=.7, rich=TRUE, family=f1) +
  geom_text(data=df %>% group_by(scenario) %>% filter(year==max(year)),
            aes(x=max(df$year)+6, label=scales::comma(accumulated_number_of_employees_that_left)), 
            hjust=0, family=f1, fontface="bold", size=3.8) +
  geom_text(data=df %>% filter(year==max(year), scenario=="Scenario 1"),
            aes(x=max(df$year)+6, y=accumulated_number_of_employees_that_left+600, label="Year 334:"), 
            hjust=0, color="grey50", size=3.5, family=f1, fontface="bold") +
  scale_color_manual(values=c("#4286ED","#00C4A7","#8A80FF")) +
  scale_y_continuous(breaks=seq(0,12500,2500), labels=scales::comma_format(),
                     expand = expansion(mult = c(0, 0.02))) + 
  scale_x_continuous(limits=c(0,370), expand = expansion(mult = c(0.01, 0.02)),
                     breaks=c(1,100,200,300)) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_hgrid(12.5) +
  theme(text=element_text(family=f1),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title=element_text(size=15),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        axis.title.y = element_blank()) +
  labs(x="Year",
       title="Accumulated number of employees that left")
       
## Tile plot
rbind(df1,df2,df3) %>% janitor::clean_names() %>%
  select(year, number_of_employees_that_left, scenario) %>%
  group_by(scenario) %>%
  mutate(grp = cut(year, breaks=c(0,50,100,150,200,250,300,334), ordered=TRUE,
                   labels=c("Year 1-50", "Year 51-100", "Year 101-150", "Year 151-200",
                           "Year 201-250", "Year 251-300", "Year 301-334"))) %>%
  ungroup() %>%
  group_by(scenario, grp) %>%
  mutate(id=row_number()) %>%
  ggplot(aes(id, fct_rev(grp), fill=number_of_employees_that_left)) +
  geom_tile(height=.6) +
  scale_fill_gradient2(low="#4286ED",mid="#00C4A7",high="#8A80FF") +
  facet_wrap(~scenario) +
  cowplot::theme_minimal_grid(10.5) +
  theme(legend.position = "top",
        legend.title=element_blank(),
        text=element_text(family=f1),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=10),
        strip.text=element_text(size=11, face="bold"),
        plot.title.position = "plot",
        legend.margin=margin(l=-62),
        plot.title=element_text(size=13, margin=margin(b=7)),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        panel.grid=element_blank()) +
  guides(fill = guide_colorbar(title.hjust = .6, barwidth = unit(15, "lines"), barheight = unit(.5, "lines"))) +
  labs(title="Number of employees that left, by year")

