# 30DayChartChallenge 2023
# Day 18. data day: EuroStat (Relationships)
# Data: Labour productivity and unit labour costs
# Source: https://ec.europa.eu/eurostat/databrowser/view/NAMQ_10_LP_ULC__custom_5250297/default/table?lang=en

# load libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# load font
font_add_google("Barlow", bold.wt = 500)
f1 = "Barlow"

# read in data
data = read_tsv("data/namq_10_lp_ulc__custom_5250297_tabular2.tsv",show_col_types = FALSE)

# wrangle
grid1 = geofacet::europe_countries_grid1 |>
  filter(code %in% c("NL","BE","LU","CH","DE","AT","FR","DK","SI")) |>
  mutate(row=row-2, col=col-2)
  
prod = read_tsv("data1/namq_10_lp_ulc__custom_5250297_tabular2.tsv",show_col_types = FALSE)|> 
  rename(des=1) |>
  separate_wider_delim(des,delim=",", names=c("freq","unit","adj","item","geo"))|>
  filter(adj=="NSA", unit=="I10") |>
  pivot_longer(6:29) |>
  select(-(1:3)) |>
  group_by(geo,item) |>
  mutate(id=row_number(),
         value=parse_number(value)) |> 
  ungroup() |>
  filter(geo %in% grid1$code,str_detect(item, "PER")) 

pa = prod |> filter(item=="NULC_PER") |> 
  rename("NULC_PER"=value) |> select(-1)
pb = prod |> filter(item=="RLPR_PER") |> 
  rename("RLPR_PER"=value) |> select(-1)
prod2 =pa |> left_join(pb)

# plot
prod |>
  ggplot(aes(x=id, y=value, color=item)) +
  geom_ribbon(data=prod2, aes(x=id, ymin=RLPR_PER, ymax=NULC_PER), inherit.aes = FALSE, alpha=.3, color="grey") +
  geom_line(linewidth=.6) +
  scale_color_manual(values=c("#f35b04","#5d16a6"), labels=c("<span style='color:#f35b04'>Real Labor productivity<br>per person</span>","<span style='color:#5d16a6'>Nominal unit labor cost<br>based on persons</span>")) +
  scale_x_continuous(breaks=seq(1,24,4), labels=c("'17","'18","'19","'20","'21","'22")) +
  scale_y_continuous(expand=c(0,0)) +
  facet_geo(~geo,grid=grid1, label="name") +
  cowplot::theme_minimal_grid() +
  theme(text=element_text(family=f1),
        legend.margin=margin(l=-20),
        legend.position=c(0,.9),
        legend.title=element_blank(),
        legend.text=element_markdown(size=11, face="bold"),
        legend.key.height = unit(2.2,"lines"),
        panel.spacing.x=unit(1.5,"lines"),
        strip.text = element_text(face = "bold"),
        axis.title=element_blank(),
        axis.text=element_text(size=10),
        plot.title.position="plot",
        plot.caption.position="plot",
        plot.caption=element_text(hjust=0, size=9.5),
        plot.subtitle=element_text(lineheight = 1.1),
        plot.margin=margin(.5,.5,.3,.5,unit="cm")) +
  labs(title="Labor Productivity and Costs", 
       subtitle="Quarterly real labor productivity per person and nominal unit labour costs based on persons,\nfrom 2017 Q1 to 2022 Q4. Unadjusted (seasonally/calendar) data, 2010=100 index.",
       caption="#30DayChartChallenge 18. data day: EuroStat")

ggsave("export/30dcc_18.png",bg="white")  
  