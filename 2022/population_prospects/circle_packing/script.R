# Trying out linear circle packing chart  
# Source: [United Nations World Population Prospects 2022](https://population.un.org/wpp/Download/Standard/CSV/)
# Source: [World Bank classifications by income level 2022-2023](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups)
# Reference: https://community.rstudio.com/t/how-to-reproduce-nytimes-beeswarm-plots-in-ggplot/104803

# Load libraries
library(tidyverse)
library(linearpackcircles) #https://gorkang.github.io/linearpackcircles/index.html
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load fonts
font_add_google("PT Sans")
f1 = "PT Sans"

# Import data
pop = readr::read_csv("data/WPP2022_Demographic_Indicators_Medium.csv")
class=readxl::read_xlsx("data/CLASS.xlsx") %>% janitor::clean_names() 

# Wrangle
pop1 = pop %>% filter(Time==2022, !is.na(ISO3_code)) %>% select(ISO3_code,NatChangeRT,TPopulation1July) 

class1 = class %>% select(economy, ISO3_code=code, region, income_group)

df = pop1 %>% left_join(class1, by="ISO3_code") %>%
  drop_na(income_group) %>%
  mutate(income_group=str_wrap(income_group,9))
  
# Plot
p1 = linearpackcircles(df,
                  ID_var="economy",
                  group_var = "income_group",
                  area_var="TPopulation1July",
                  x_var="NatChangeRT",
                  separation_factor = 25,
                  size_text = 3.3,
                  ) +
  #geom_vline(xintercept=0, size=.3) +
  theme_minimal(13) +
  theme(text=element_text(family=f1),
        plot.title.position = "plot",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size=.3, color="#E9DBD1"),
        axis.text.x=element_text(color="black"),
        axis.text.y=element_text(color="black", lineheight = 1, face="bold", size=12),
        axis.title.x=element_text(size=11, margin=margin(t=8)),
        plot.title=element_text(size=14.5, face="bold"),
        plot.subtitle=element_text(size=11, lineheight=1.1),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust=0, size=9.5, color="grey35", margin=margin(t=13)),
        plot.background=element_rect(fill="#FFF1E5", color=NA),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        ) +
  labs(title="Rate of Natural Change and Total Population in 2022",
       subtitle="Each circle represents a country and the diameter represents total population as of 1 July 2022. Rate of natural change is defined as birth minus\ndeaths per 1,000 population.\n",
       x="Rate of Natural Change (per 1,000 population)",
       caption="Source: United Nations, Department of Economic and Social Affairs, Population Division (2022). World Population Prospects 2022, Online Edition.")
       
ggsave("p1.png",p1,height=8,width=10)        