# Monkey pox confirmed cases May 6, 2022 to May 25, 2022.
# Data: Global.health (https://github.com/globaldothealth/monkeypox) via Our World In Data (https://ourworldindata.org/explorers/monkeypox)

# Libraries 
library(tidyverse)
library(lubridate)
library(countrycode)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("IBM Plex Serif")
f1 = "IBM Plex Serif"

# Data
df = read_csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv",show_col_types = FALSE) %>% janitor::clean_names()

# Wrangle
df1 = tibble(date_confirmation= seq(as.Date("2022-05-01"),as.Date("2022-05-31"),1)) %>%
  left_join(df %>% filter(status=="confirmed") %>%count(date_confirmation) %>% drop_na(),by = "date_confirmation") %>%
  mutate(day=day(date_confirmation),
         y=ceiling(day/7),
         x=wday(date_confirmation)
         #x=fct_inorder(wday(date_confirmation,label = TRUE))
         )
         
# Calendar plot
ggplot() +
  geom_tile(data=df1 %>% filter(is.na(n),day<=25), aes(x, y), size=1, fill="grey91", color="white") +
  geom_tile(data=df1 %>% filter(is.na(n),day>25), aes(x, y), size=1, fill="white", color="grey91") +
  geom_tile(data=df1 %>% filter(!is.na(n)), aes(x, y), size=.5,color="white", fill="#212529") +
  geom_text(data=df1, aes(x=x+.45,y-.35,label=day,color=I(ifelse(is.na(n),"black","#ced4da"))), hjust=1, size=3) +
  geom_point(data=df3 %>% filter(region=="Americas"), 
             aes(x-.35,y+.35, shape=region, fill=region), size=3) +
  geom_point(data=df3 %>% filter(region=="Asia"), 
             aes(x-.2,y+.35,shape=region,, fill=region), size=3) + 
  geom_point(data=df3 %>% filter(region=="Europe"), 
             aes(x-.05,y+.35,shape=region,, fill=region), size=3) +
  geom_point(data=df3 %>% filter(region=="Oceania"), 
             aes(x+.2,y+.35,shape=region,fill=region), size=3) +
  scale_shape_manual(values=seq(21,24,1)) +
  MetBrewer::scale_fill_met_d("Egypt") +
  #ggnewscale::new_scale_color() +
  geom_text(data=df1 %>% filter(!is.na(n)) %>%
  mutate(n=as.double(n)), aes(x, y, label=n), size=5, color="white", fontface="bold", show.legend = F) +
  scale_y_reverse() +
  scale_x_continuous(breaks=seq(1,7),position="top", labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday", "Friday", "Saturday"),) +
  coord_cartesian(expand=FALSE,clip="off") +
  cowplot::theme_minimal_grid(13) +
  theme(text=element_text(family=f1),
        plot.margin=margin(.4,.75,.4,.4, unit="cm"),
        plot.caption=element_text(size=10, hjust=0),
        plot.subtitle=element_text(size=10.5, lineheight=1.2, margin=margin(t=4, b=8)),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        panel.grid=element_blank(),
        legend.position = "top",
        legend.text=element_text(size=10.5),
        legend.title = element_text(size=10.5)) +
  labs(shape="Region:", fill="Region:",
       subtitle="Total number of confirmed cases reported in Americas, Asia, Europe and Oceania\nfrom May 6, 2022 to May 25, 2022.",
       caption="\nSource: Global.health by way of Our World In Data",
       title="Monkeypox confirmed cases (May 2022)")
       
ggsave("p1.png", bg="white")         