# Dietary Choices, UK Adults
# Data from Our World In Data & YouGov (2022)
# Source: https://yougov.co.uk/topics/lifestyle/trackers/dietery-choices-of-brits-eg-vegeterian-flexitarian-meat-eater-etc
# Source: https://ourworldindata.org/vegetarian-vegan

# Libraries
library(tidyverse)
library(scales)

# Data
df = read_csv("data/dietary-choices-uk.csv",show_col_types = FALSE) %>% janitor::clean_names()

# Wrangle
df1 = df %>% #filter(entity=="All adults") %>% 
  pivot_longer(flexitarian:vegetarian) %>%
  mutate(name=str_to_sentence(str_replace_all(name,"_"," ")),
         name=fct_reorder(name, value),
         value=case_when(name=="Meat eater"~-1*value, TRUE~value)) %>%
  rename(date=day) 
  
df2 = df1 %>% filter(name=="Meat eater") %>% group_by(entity,name,date) %>%
  summarise(y=value, yend=value+100)

# Function to reverse scale date
# reference: https://github.com/tidyverse/ggplot2/issues/4014
reverse2_trans <- function() {
  trans_new(
    "reverse2",
    function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
    function(x) -1 * as.numeric(x)
  )
}

# Plot
df1 %>%
  ggplot(aes(x=date, y=value)) +
  geom_area(aes(fill=name)) +
  geom_segment(data=df2, aes(x=date, xend=date, y=y, yend=yend), 
               color="white", linetype="dotted") +
  geom_hline(yintercept=0, color="black") +
  #geom_col(width=50) +
  MetBrewer::scale_fill_met_d("Derain", direction=-1) +
  MetBrewer::scale_color_met_d("Derain", direction=-1) +
  scale_y_continuous(labels=function(x) scales::percent(abs(x), scale=1),
                     breaks=seq(-75,50,25), expand=c(0,0), limits=c(NA,50)) +
  scale_x_continuous(breaks=unique(df$day), trans = c("date", "reverse2"), expand=c(0.02,0.02)) +
  #scale_x_date(breaks=unique(df$day)) +
  facet_wrap(~entity, nrow=1) +
  cowplot::theme_minimal_grid() +
  theme(legend.position = "top",
        legend.justification = "center",
        panel.grid=element_blank(),
        panel.spacing = unit(3, "lines"),
        axis.line=element_blank(),
        strip.placement = "outside",
        axis.title=element_blank(),
        axis.ticks=element_line(color="black"),
        axis.ticks.length=unit(.25, "cm"),
        strip.text=element_text(face="bold"),
        plot.title.position="plot",
        plot.caption.position="plot",
        legend.title=element_blank(),
        plot.caption=element_text(color="grey30",hjust=0, margin=margin(t=15)),
        plot.subtitle = element_text(margin=margin(b=10)),
        plot.margin=margin(.5,.5,.5,.5, unit="cm")
        )  +
  coord_flip() +
  guides(fill=guide_legend(reverse=T,nrow = 1)) +
  labs(title="Dietary preferences among British adults",
       subtitle="Share of Dietary preferences from Jul 11, 2019 to Dec 30, 2021. Based on YouGov's surveys, around 2000 adults aged 18 and above are included per survey.",
       caption="Source: Our World In Data and YouGov (2022)")

