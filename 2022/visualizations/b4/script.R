library(tidyverse)
library(ggtext)

library(showtext)
font_add_google("Lato")
font_add_google("Outfit")
font_add_google("Libre Franklin")
font_add_google("Roboto")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Euromonitor International
df1 = tribble(
  ~cat, ~val,
  "Tinctures and Sprays",34,
  "Topicals",19,
  "Flower",12,
  "Capsules",9,
  "Edibles",8,
  "Vapour",8,
  "Beverages",4,
  "Pre‐Roll",3,
  "Concentrates",2,
  "Other",1
) 

df1 %>%
  mutate(cat=fct_rev(fct_inorder(cat)),
         val=val/100) %>%
  ggplot(aes(x=val, y=cat)) +
  geom_segment(aes(x=0, xend=val, y=cat, yend=cat,color=cat), show.legend=F, size=1.5) +
  geom_point(aes(color=cat), size=9, show.legend = F) +
  geom_text(aes(label=scales::percent(val, accuracy=1)), 
            color="white", size=3.4, family="Outfit") +
  geom_text(aes(label=cat, x=-0.01, color=cat), hjust=1, show.legend=F,family="Lato", fontface="bold") +
  scale_color_manual(values=c("#EBA796","#588C73","#754044","#DDA247","#3B3F73",
                            "#CC4A50","#C28863","#CF6F39","#586144","#658BA0")) +
  scale_x_continuous(expand = expansion(mult = c(.275, .02))) +
  coord_cartesian(clip="off") +
  cowplot::theme_map(12) +
  theme(axis.title=element_blank(),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        panel.grid=element_blank(),
        text=element_text(family="Lato"),
        plot.caption=element_text(hjust=0, size=8, margin=margin(t=8))) +
  labs(title="Global CBD Sales by Format, 2021",
       caption="Source: Euromonitor International")
       
# Aon
library(gt)
library(gtExtras)

df2 = tribble(
  ~"Medical Trend", ~HMO, ~PPO, ~POS, ~Indemnity, ~HDHP, ~Overall,
  "5th Percentile",3.8,4.2,5.2,5.0,4.1,4.0,
  "25th Percentile",6.5,5.7,6.5,6.2,5.5,5.6,
  "50th Percentile",8.4,8.2,7.8,8.5,8.2,7.6,
  "75th Percentile",10.5,9.7,10.0,11.4,9.7,9.5,
  "95th Percentile",23.3,13.3,11.9,16.5,13.5,12.4
)

df2 %>%
  gt() %>%
  gt_theme_nytimes() %>%
  gt_color_box(columns=HMO, domain=range(df2$HMO), width=70, accuracy=.1, suffix="%") %>%
   gt_color_box(columns=PPO, domain=range(df2$PPO), width=70, accuracy=.1, suffix="%") %>%
   gt_color_box(columns=POS, domain=range(df2$POS), width=70, accuracy=.1, suffix="%") %>%
  gt_color_box(columns=Indemnity, domain=range(df2$Indemnity), width=70, accuracy=.1, suffix="%") %>%
gt_color_box(columns=HDHP, domain=range(df2$HDHP), width=70, accuracy=.1, suffix="%") %>%
gt_color_box(columns=Overall, domain=range(df2$Overall), width=70, accuracy=.1, suffix="%") %>%
  tab_options(table.width = px(650),
              data_row.padding = px(9),
              source_notes.font.size = px(11)
              ) %>%
  cols_align(align="center", columns=HMO:Overall) %>%
  tab_style(style=cell_text(color="black", size=px(15), transform = "capitalize", weight=550), locations=cells_column_labels()) %>%
  tab_header(title= "Medical Trends by Percentiles",
             subtitle="While the average medical increase for renewals in 2021 is 7.5% when weighted by enrollment, there is a significant differential in the reported trend rates by carrier. The following chart illustrates the range of reported trends.") %>%
  tab_source_note("Data source: Aon’s 2021 Carrier Trend Survey")
  
  