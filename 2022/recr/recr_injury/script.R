# 2022-06-01
# Recreating FlowingData: Deaths by Firearm, Compared Against Injury-Related Deaths [chart](https://flowingdata.com/2022/05/25/deaths-by-firearm-compared-against-injury-related-deaths/)
# Data source: [Centers for Disease Control and Prevention, CDC](https://wonder.cdc.gov/)

# Libraries
library(tidyverse)
library(ggalluvial)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Overpass Mono")
font_add_google("Roboto")
font_add_google("Roboto Serif")
f1 = "Overpass Mono"
f2 = "Roboto"
f3 = "Roboto Serif"

# Data
df = read_tsv("data/cdc.tsv") %>% janitor::clean_names()

# Labels df
lab = df %>% select(year, cause=injury_mechanism_all_other_leading_causes,deaths) %>%
  group_by(cause) %>%
  tally(deaths, sort=T) %>%
  slice(1:6) %>%
  mutate(x=c(2000,2017.8,2012,1999,2004,2002),
         y=c(12900,10500,4400,5400,3200,2400),
         cause=case_when(cause=="Fire/Flame"~"Fire",TRUE~cause))
         
# wrap function that can be applied to a vector of strings 
# reference: https://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles
wrap_strings  <- function(vector_of_strings,width){as.character(sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")}))}         
         
# Plot
df %>% select(year, cause=injury_mechanism_all_other_leading_causes,deaths) %>%
  drop_na() %>%
  mutate(col=case_when(cause=="Firearm"~"#AD1700",TRUE~"white")) %>%
  ggplot(aes(x=year, y=deaths, alluvium=cause)) +
  geom_alluvium(aes(fill = col), decreasing = FALSE, color="black", alpha=1, size=.2) +
  geom_text(data=lab, aes(x=x, y=y, label=cause, size=cause, color=I(ifelse(cause=="Firearm","white","black"))), 
            hjust=0, family=f1) +
  scale_size_manual(values=c(3.2,3,4,4,3.2,3.2)) +
  scale_fill_identity() +
  scale_y_continuous(expand=c(0.02,0.02), breaks=c(0,16709), labels=c(0,"DEATHS\n16,709\n")) +
  scale_x_continuous(breaks=seq(1999,2020,1), expand=c(0.02,0.02), labels = c("1999","2000","'01","'02","'03","'04","'05","'06","'07","'08","'09","'10","'11","'12","'13","'14","'15","'16","'17","'18","'19","'20")) +
  theme_minimal(11, base_family = f1) +
  theme(panel.grid=element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, family=f1),
        plot.subtitle=element_text(hjust=.5, size=9.5, family=f3, margin=margin(b=10)),
        plot.caption=element_text(size=6.5),
        axis.title=element_blank(),
        axis.text=element_text(size=6.5, color="black", lineheight = 1.2),
        axis.ticks=element_line(color="black", size=.2),
        axis.ticks.length=unit(.2, "cm")) +
  labs(title="RECREATED CHART",
       subtitle="Injury mechanism among 1- to 19-year olds in the United States, from 1999 to 2000",
       caption="\nOriginal plot from FlowingData | Data source: Centers for Disease Control and Prevention, CDC") +
  annotate(geom="text", x=2013, y=13500, family=f3, size=2.7, hjust=0, label="As motor vehicle deaths decreased,\nfirearms death increased and became\nthe leading cause of injury-related\ndeath in 2018, among 1- to 19-year-\nolds.")
  
ggsave("p1.png", bg="white", height=6, width=8)         
