# Recreating FiveThirtyEight's [tile map](https://fivethirtyeight.com/wp-content/uploads/2022/07/potts.BIRTH-CONTROL.0713.png?w=1424) from [How Limiting Access To Abortion Limits Access To Birth Control (Jul 13, 2022)](https://fivethirtyeight.com/features/how-limiting-access-to-abortion-limits-access-to-birth-control/)

# Load libraries
library(tidyverse)
library(shadowtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import fonts
font_add_google("Roboto")
f1 = "Roboto"
font_add_google("IBM Plex Sans")
f2 = "IBM Plex Sans"
font_add_google("Barlow Semi Condensed")
f3 = "Barlow Semi Condensed"
font_add_google("Barlow")
f4 = "Barlow"

# Map grid
# Map grid
grid = tribble(
  ~y,~x,~label,
  1,5,"TX",
  1,10,"FL",
  2,1,"HI",
  2,2,"AK",
  2,4,"OK",
  2,5,"LA",
  2,6,"AR",
  2,7,"MS",
  2,8,"AL",
  2,9,"GA",
  3,3,"AZ",
  3,4,"NM",
  3,5,"KS",
  3,6,"MO",
  3,7,"KY",
  3,8,"TN",
  3,9,"SC",
  3,10,"NC",
  3,11,"DC",
  4,2,"CA",
  4,3,"NV",
  4,4,"CO",
  4,5,"NE",
  4,6,"IL",
  4,7,"IN",
  4,8,"WV",
  4,9,"VA",
  4,10,"MD",
  4,11,"DE",
  5,2,"OR",
  5,3,"UT",
  5,4,"WY",
  5,5,"SD",
  5,6,"IA",
  5,7,"WI",
  5,8,"OH",
  5,9,"PA",
  5,10,"NJ",
  5,11,"CT",
  6,2,"WA",
  6,3,"ID",
  6,4,"MT",
  6,5,"ND",
  6,6,"MN",
  6,8,"MI",
  6,10,"NY",
  6,11,"MA",
  6,12,"RI",
  7,11,"VT",
  7,12,"NH",
  8,12,"ME"
)

# Data
df = grid %>% 
  mutate(grp1 = case_when(label %in% c("WY","SD","WI","KS","TN","SC","NC","MS","AL","GA","TX","FL")~"border"),
         grp2 = case_when(label %in% c("SD","MS","AL","TX")~"NEAR-TOTAL BAN ON ABORTION",
                          label %in% c("NC")~"PARTIAL BAN ON ABORTION",
                          label %in% c("WY","WI","KS","TN","SC","GA","FL")~"ABORTION RESTRICTIONS IN LEGAL LIMBO"))
                          
# Plot
p1= df %>%
  ggplot(aes(x=x, y=y)) +
  geom_point(data = df %>% filter(is.na(grp1)), size=18, shape=15, color="#F0F0F0") +
  shadowtext::geom_shadowtext(data = df %>% filter(is.na(grp1)), aes(label=label), family=f1, color="#D5D5D5",bg.color="white", nudge_y = .1, size=4.2) +
  geom_point(data= df %>% filter(!is.na(grp1)), size=19.5, shape=22, stroke=.3) +
  geom_text(data= df %>% filter(!is.na(grp1)), aes(label=label), family=f1, nudge_y=.1) +
  geom_point(data=df %>% filter(!is.na(grp1)), aes(y=y-.15,color=factor(grp2,levels=c("NEAR-TOTAL BAN ON ABORTION","PARTIAL BAN ON ABORTION","ABORTION RESTRICTIONS IN LEGAL LIMBO"))), size=3) +
  scale_color_manual(values=c("#F26C3B","#FDCD26","#6140D9")) +
  coord_fixed(clip="off", expand=FALSE) +
  cowplot::theme_map() +
  theme(text=element_text(family=f2),
        plot.title=element_text(size=14.3),
        plot.subtitle=element_text(size=13.3, lineheight = 1, margin=margin(b=20), family=f2),
        legend.text = element_text(size=10.5),
        legend.title=element_blank(),
        legend.position=c(.12,.98)) +
  labs(title="Recreation of FiveThirtyEight's Tile Map",
       subtitle="Status of the legality of abortion in states that failed to expand Medicaid under the affordable\nCare Act") +
  # caption 
  annotate(geom="text",x=.5,y=-.15, label="July 14, 2022", hjust=0, size=3.2, family=f4) +
  annotate(geom="text",x=12.5,y=-.15, label="SOURCES: KAISER FAMILY FOUNDATION, GUTTMACHER INSTITUTE", hjust=1, size=3, family=f3) +
  annotate(geom="segment", x=.5, xend=12.5, y=0, yend=0, size=.3) +
  # KS and FL labels
  annotate(geom="text",x=3.5, y=.85, label="Kansas has an\nanti-abortion\nconstitutional\namendment on\nits Aug. 2 ballot", lineheight=1, size=3.2, family=f2) +
annotate(geom="text",x=11.5, y=1.85, label="Florida courts are\ncurrently determing\nthe legality of the\nstate's recent 15-\nweek abortion ban", lineheight=1, size=3.2, family=f2) +
  # arrows
  annotate(geom="curve", x=3.35, y=1.5, xend=4.55, yend=2.7, curvature=-.6, size=.2, arrow=arrow(length=unit(.25,"lines"))) +
  annotate(geom="curve",x=10.6, y=2.07, xend=10, yend=1.45,size=.2, arrow=arrow(length=unit(.25,"lines")))
  
ggsave("p1.png", p1, height=6.6, width = 8, bg="white")
                            