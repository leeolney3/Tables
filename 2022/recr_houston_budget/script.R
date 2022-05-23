# Recreating Houston Budget chart by ACLU TX, Data to Dream and Moksha Data Studio
# Data source and original chart: https://houstonbudget.cool/

# Libraries
library(tidyverse)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Cabin")
f1 = "Cabin"

# Data
df = tribble(
  ~lab, ~lab_long, ~value,
  1,"Houston Health\nDepartment", 15,
  2,"Police\nDepartment",12,
  3,"Houston Public\nWorks",10,
  4,"Soild Waste\nManagement",3,
  5,"Fire Department",15,
  6,"Parks and\nRecreation",14,
  7,"Municipal Courts\nDepartment",11,
  8,"Library",11,
  9,"Finance\nDepartment",11,
  10,"Department of\nNeighbourhoods",10,
  11,"Administration\nand Regulatory\nAffairs", 8,
  12,"Planning &\nDevelopment",7,
  13,"Legal",6,
  14,"Office of\nBusiness\nOpportunity",6,
  15,"General Services",5,
  16,"Human\nResources",4,
  17,"Houston\nEmergency\nCenter",4,
  18,"Fleet\nManagement\nDepartment",3,
  19,"Houston\nInformation\nTechnology\nServices",1
)

# Plot
# waffle plot method adapted from u/maxchills, https://www.reddit.com/r/rstats/comments/ucep53/what_is_this_chart_type_called_id_like_to/i69zznq/?utm_source=share&utm_medium=web2x&context=3

df1 = df %>% group_by(lab) %>% expand_grid(x = 1:6, y = 1:8) %>%
  ungroup() %>%
  group_by(lab) %>%
  mutate(id=row_number(),
         col=case_when(id<=value~"b", TRUE~"a")) 

df1 %>%
  ggplot(aes(x = y , y = x, fill = col, color = col)) + 
  geom_tile(color="white", size=1) +
  geom_text(data=df1 %>% group_by(lab) %>% filter(id==1),aes(x=9, y=6, label=lab_long), hjust=1, vjust=0, size=4.5, color="black", family=f1) +
  scale_fill_manual(values =  c("white","#7645FF")) +
  scale_y_reverse() +
  coord_fixed() +
  facet_wrap(~lab, ncol=4) +
  theme_void() +
  theme(panel.spacing = unit(0, "lines"),
        strip.text = element_blank(),
        panel.background = element_rect(color="#7645FF"),
        legend.position = "none") 
        
ggsave("houston.png", height=6.2, width=7, bg="white")