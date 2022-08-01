# 2022-06-22
# Recreation of Bloomberg's consumer price index component chart, by way of [The Moksha Roundup](https://mokshadata.studio/roundup/20/)
# Corresponding article: [How Close Are We Really to 1970s-Style Inflation? (11 June 2022)](https://www.bloomberg.com/graphics/2022-opinion-inflation-rising-prices-gas-food-comparison/)
# Data source: [US Bureau of Labour Statistics](https://www.bls.gov/cpi/data.htm)

# Load libraries
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import font
font_add_google("Lato")
font_add_google("IBM Plex Serif")
f1 = "Lato"
f2 = "IBM Plex Serif"

# Import data
df = readr::read_csv("data/cpi_year.csv")

# Wrangle
df = df %>% 
  add_row(series ="",.after=1) %>%
  add_row(series ="",.after=10)
  
des = c("**All items**","x1","Transportation","Food, beverages","Housing","Other goods,<br>services",
           "Apparel","Recreation","Medical care","Education,<br>communication","x2","Motor fuel",
           "Airline fares","Hotels","Household energy","Vehicle parts","Health insurance",
           "New, used vehicles","Furniture, bedding","Food at home","Tools","Cleaning products",
           "Pets, pet products")

des_lab = des
des_lab[2] =""
des_lab[11]=""

description = tibble(
  series=df$series,
  long = des
)

# Plot
p1 = df %>% 
  pivot_longer(!series) %>%
  left_join(description, by="series") %>%
  mutate(long=factor(long, levels=rev(des))) %>%
  group_by(long) %>%
  mutate(id=row_number()) %>%
  ggplot(aes(x=id, y=long)) +
  geom_tile(aes(fill=value), size=.4, color="white") +
  scale_fill_stepsn("Change from a year earlier",
                    colors=c("#0572FE","#91C3FF","#FFF106","#FFC304","#FE9301","#FF8A4D","#FF0038"),
                    breaks=c(-5,0,5,10,20,40),limits=c(-15,48), na.value = "transparent",
                    labels=c("-5%","0","+5","+10","+20","+40%")) +
  scale_y_discrete(labels=rev(des_lab), expand=c(0,0)) +
  scale_x_continuous(limits=c(0,42), expand=c(0,0), breaks=c(1,15,25,41),position="top",
                     labels=c("Jan<br>2019","WHO declares<br>pandemic","Jan<br>2021","May<br>2022")) +
  cowplot::theme_minimal_grid(11.5) +
  theme(text=element_text(family=f1),
        panel.grid.major = element_blank(),
        legend.title=element_text(face="bold", size=10.5),
        axis.text.y=element_markdown(lineheight=1),
        axis.text.x.top = element_markdown(lineheight=1,margin=margin(b=3)),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="black", size=.3),
        axis.ticks.length = unit(.3,"lines"),
        axis.title=element_blank(),
        legend.position = "top",
        legend.margin=margin(l=-88),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0),
        plot.margin=margin(.4,.4,.4,.4,unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=10.5, face="plain", family=f2),
        plot.subtitle=element_markdown(size=10, family=f2, margin=margin(b=10))) +
  guides(fill=guide_colorsteps(title.position = "top", label.position="top",
                               barwidth = unit(15, "lines"), barheight = unit(.3, "lines"))) +
  annotate(geom="text",x=0.5, y=22, label="Major components", hjust=0, size=3.5, vjust=.9, fontface="bold") +
  annotate(geom="text",x=0.5, y=13, label="Sub components with highest increase", hjust=0, size=3.5, vjust=.9, fontface="bold") +
  labs(caption="\nData source: US Bureau of Labour Statistics",
       title="Recreation of Bloomberg's Consumer Price Index Components chart",
       subtitle="from *How Close Are We Really to 1970s-Style Inflation?*, 11 June 2022")
       
ggsave("p1.png",p1, height=8, width=6.75, bg="white")
           

