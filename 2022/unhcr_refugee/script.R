# 2022-07-02
# Ukraine refugee situation cartograms

# Data source: [UNHCR](https://data.unhcr.org/en/situations/ukraine/location?secret=unhcrrestricted), data date: 28 June 2022 (retrieved 02 July 2022). 
# Color palette from [BjnNowak]https://github.com/BjnNowak/TidyTuesday/blob/main/SC_ElectricStations.R
# Population from [World Bank](https://data.worldbank.org/indicator/SP.POP.TOTL?end=2021&locations=UA&start=1960)
# ggplot cartogram reference: [Cartograms in ggplot2](https://r-charts.com/spatial/cartogram-ggplot2/)

# load libraries
library(tidyverse)
library(ggtext)
library(countrycode)
library(sf)
library(cartogram)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import fonts
font_add_google("Outfit")
f1 = "Outfit"

# Import data
df1 = readr::read_csv("data/unhcr_1.csv",show_col_types = FALSE) %>% janitor::clean_names() %>% select(1:2,4)
df2 = readr::read_csv("data/unhcr_2.csv",show_col_types = FALSE) %>% janitor::clean_names() %>% select(1:2,4)

# Data preparation
df3 = rbind(df1,df2) %>% rename(value=3) %>%
  filter(country!="Total", country!="Russian Federation***") %>%
  mutate(country=case_when(country=="TUrkiye"~"Turkey", TRUE~country),
    adm0_a3=countrycode(country,origin = "country.name", destination ="iso3c")) %>%
  mutate(value1=parse_number(value),
         pct_pop = value1/43814581*100) %>%
  filter(country!="Malta",country!="Liechtenstein") %>%
  filter(!is.na(pct_pop))
  
s = ne_countries(scale=110, type="countries", returnclass = "sf")
s2 = s %>% right_join(df3)
s3 = st_transform(s2, 3857) #Mercator projection
s4 = st_transform(s2, 3035) #Lambert Azimuthal Equal-Area projection 

ncont <- cartogram_ncont(s4, weight = "value1")
prov <- cartogram_dorling(s4, weight = "pct_pop")

# p1: dorling cartogram
pal1 <- c('#FFD700','#C0B72E','#40778A','#0057B7') # color palette

ggplot(prov) +
  geom_sf(aes(fill=pct_pop), size=.2) +
  ggsflabel::geom_sf_text_repel(aes(label=adm0_a3), seed=123, size=2.8, force=2, family=f1) +
  scale_fill_stepsn("Ukrainians registered<br>for protection as a<br>share of Ukraine's<br>2021 population",colors=pal1, labels=scales::percent_format(accuracy=.1, scale=1), breaks=c(.5,1,1.5,2)) +
  cowplot::theme_map(10) +
  guides(fill = guide_colorbar(
    barheight = unit(30, units = "mm"),
    barwidth = unit(3, units = "mm"))) +
  theme(text=element_text(family=f1),
        legend.position = "left",
        legend.margin=margin(t=-10),
        legend.title=element_textbox(lineheight = 1.4, size=11),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, color="grey30", lineheight = 1.1, size=7.5),
        plot.margin=margin(.3,.5,.3,.7, unit="cm"),
        plot.background=element_rect(fill="#F9F9F9", color=NA)) +
  labs(caption="Note: Most recent year (2021) population (n=43,814,581) from World Bank\nSource: UNHCR Ukraine Refugee Situation •  Data date: 28 June 2022  •  Color palette from BjnNowak")

ggsave("p1.png", height=5, width=7)

# p2: non-contiguous cartogram 

ggplot(ncont) +
  geom_sf(aes(fill=pct_pop), size=.1) +
  ggsflabel::geom_sf_text_repel(aes(label=adm0_a3), seed=1, size=2.5, force=1, color="white", family=f1) +
  scale_fill_stepsn("Ukrainians registered for protection as a share of Ukraine's 2021 population",colors=pal1, labels=scales::percent_format(accuracy=.1, scale=1), breaks=c(.5,1,1.5,2)) +
  coord_sf(clip="off") +
  cowplot::theme_map(9) +
  guides(fill = guide_colorbar(title.position="top",ticks.colour = NA,ticks.linewidth = 2, barheight = unit(2, units = "mm"),barwidth = unit(50, units = "mm"))) +
  theme(text=element_text(color="white", family=f1),
        legend.position=c(0,1.07),
        legend.title=element_textbox(lineheight = 1.3, size=10),
        plot.title=element_text(size=7.5, face="plain", margin=margin(t=165, b=-165), hjust=0.95, lineheight=1.2),
        plot.caption.position = "plot",
        legend.direction = "horizontal",
        plot.caption=element_text(hjust=0, color="grey90", lineheight = 1.1, size=6.5),
        plot.margin=margin(.3,.5,.3,.5, unit="cm"),
        plot.background=element_rect(fill="#212529", color=NA)) +
  labs(caption="Note: Most recent year (2021) population (n=43,814,581) from World Bank\nSource: UNHCR Ukraine Refugee Situation •  Data date: 28 June, 2022  •  Color palette from BjnNowak",
       title="2.7% (n=1,194,642) of\nUkraine's population are\nregistered in Poland\nas of 28 June, 2022")

ggsave("p2.png", height=5, width=5.8)
  
