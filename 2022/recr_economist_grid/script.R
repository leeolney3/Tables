# 2022-05-18
# Recreating The Economist: Senators' votes on bill codifying abortion rights chart (May 11 2022)
# Link to original plot: https://blog.datawrapper.de/wp-content/uploads/2022/05/image5-1.png

# Libraries
library(tidyverse)
library(ggtext)
library(geofacet)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("PT Sans")
f1 = "PT Sans"

# Data 
df = tribble(
  ~code, ~grp,
  "MT","s",
  "WI","s",
  "OH","s",
  "ME","s",
  "AK","Against",
  "WA","In favor",
  "OR","In favor",
  "CA","In favor",
  "HI","In favor",
  "ID","Against",
  "NV","In favor",
  "UT","Against",
  "AZ","In favor",
  "WY","Against",
  "CO","In favor",
  "NM","In favor",
  "ND","Against",
  "SD","Against",
  "NE","Against",
  "KS","Against",
  "OK","Against",
  "TX","Against",
  "MN","In favor",
  "IA","Against",
  "MO","Against",
  "AR","Against",
  "LA","Against",
  "IL","In favor",
  "IN","Against",
  "KY","Against",
  "TN","Against",
  "MS","Against",
  "MI","In favor",
  "WV","Against",
  "NC","Against",
  "AL","Against",
  "VA","In favor",
  "SC","Against",
  "GA","In favor",
  "NY","In favor",
  "NJ","In favor",
  "MD","In favor",
  "DC",NA,
  "FL","Against",
  "VT","In favor",
  "MA","In favor",
  "CT","In favor",
  "DE","In favor",
  "NH","In favor",
  "RI","In favor",
)

# Create custom grid df
customgrid = tribble(
  ~row, ~col, ~code,
  8,1,"HI",
  5,1,"CA",
  4,1,"OR",
  3,1,"WA",
  1,1,"AK",
  6,2,"AZ",
  5,2,"UT",
  4,2,"NV",
  3,2,"ID",
  6,3,"NM",
  5,3,"CO",
  4,3,"WY",
  3,3,"MT",
  3,4,"ND",
  4,4,"SD",
  5,4,"NE",
  6,4,"KS",
  7,4,"OK",
  8,4,"TX",
  3,5,"MN",
  4,5,"IA",
  5,5,"MO",
  6,5,"AR",
  7,5,"LA",
  2,6,"WI",
  3,6,"IL",
  4,6,"IN",
  5,6,"KY",
  6,6,"TN",
  7,6,"MS",
  3,7,"MI",
  4,7,"OH",
  5,7,"WV",
  6,7,"NC",
  7,7,"AL",
  4,8,"PA",
  5,8,"VA",
  6,8,"SC",
  7,8,"GA",
  3,9,"NY",
  4,9,"NJ",
  5,9,"MD",
  6,9,"DC",
  8,9,"FL",
  2,10,"VT",
  3,10,"MA",
  4,10,"CT",
  5,10,"DE",
  1,11,"ME",
  2,11,"NH",
  4,11,"RI"
) %>% 
  mutate(name=state.name[match(code,state.abb)],
         name=case_when(code=="DC"~"DC", TRUE~name)) 
         
# df for diagonal lines
df2 = df %>% filter(grp=="s") %>%
  mutate(x1=0, x2=1) %>%
  pivot_longer(x1:x2, values_to="x") %>%
  select(-name) %>%
  mutate(y=x)
  
# Plot
df %>% ggplot() +
  geom_rect(data = df %>% filter(grp!="s"),
            aes(xmin=0, xmax=1, ymin=0, ymax=1, fill=grp)) +
  geom_rect(data = df %>% filter(is.na(grp)),
            aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="grey") +
  geom_rect(data = df %>% filter(grp=="s"),
            aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="#7A9EC2") +
  geom_area(data= df2, aes(x=x, y=y), fill="#BE0A3E") +
  geom_text(aes(x=0.5, y=0.5, label=code), color="white", family=f1) +
  scale_fill_manual(values=c("#BE0A3E","#7A9EC2")) +
  coord_fixed(expand=F) +
  facet_geo(~code, grid=customgrid) +
  cowplot::theme_nothing() +
  theme(text=element_text(family=f1),
        legend.position = "top",
        legend.justification = "left",
        legend.margin=margin(l=-7.9),
        legend.title=element_blank(),
        legend.key.height =unit(0.1, "line"),
        legend.key.width =unit(.63, "line"),
        legend.text = element_text(size=11),
        panel.spacing = unit(0.1,"lines"),
        plot.margin=margin(.5,.5,.5,.5,unit="cm"),
        plot.title = element_text(size=12.5, face="bold", hjust=0, margin=margin(b=5)),
        plot.subtitle=element_text(size=11, hjust=0, margin=margin(b=15)),
        plot.caption=element_markdown(size=10, color="grey50", hjust=0, margin=margin(t=15))
        ) +
  guides(fill=guide_legend(reverse=T)) +
  labs(title="Senator's votes on bill codifying abortion rights",
       subtitle="May 11th 2022",
       caption="Source: YouGov/*The Economist*")
       
# Save
ggsave("p1.png", height=4.2, width=4.2, bg="white")
