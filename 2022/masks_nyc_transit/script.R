# Mask-wearing on NYC transit
# Source: MTA & data.ny.gov, by way of DataIsPlural (updated: April 25, 2022)
# Data: https://data.ny.gov/Transportation/MTA-Subway-and-Bus-Mask-Compliance-Statistics-Begi/ijxr-nffj 

# Libraries
library(tidyverse)
library(lubridate)
library(ggh4x)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Fonts
font_add_google("IBM Plex Serif")
f1 = "IBM Plex Serif"
font_add_google("IBM Plex Sans Condensed")
f2="IBM Plex Sans Condensed"

# Data
df = read_csv("MTA_Subway_and_Bus_Mask_Compliance_Statistics__Beginning_2020.csv") %>% 
	janitor::clean_names()

# Wrangle
df1 = df %>% 
  mutate(survey_period_start_date=case_when(survey_period_start_date=="6/1/2020"~"6/1/2021",
                                            TRUE~survey_period_start_date)) %>%
  mutate(start=mdy(survey_period_start_date),
                    end=mdy(survey_period_end_date)) %>%
  mutate(d1 = end-start,d1 = start+d1) %>%
  group_by(mode) %>%
  mutate(id=rev(row_number()))
  
# Plot
df1 %>% 
  ggplot() +
  geom_rect(aes(xmin=start, xmax=end, ymin=mask_worn_correctly, ymax=total_wearing_a_mask,
                fill="Mask worn incorrectly"), alpha=.4) +
  geom_segment(aes(x=start, xend=end, y=total_wearing_a_mask, yend=total_wearing_a_mask, 
                   color="Total wearning a mask"), size=1) +
  geom_segment(aes(x=start, xend=end, y=mask_worn_correctly, yend=mask_worn_correctly, 
                   color="Mask worn correctly"),, size=1) +
  geom_text(aes(x=d1, y=mask_worn_correctly, 
                label=scales::percent(mask_worn_correctly, accuracy=1)),
            size=3, family=f2, color="#421B96", vjust=1.8, hjust=.6) +
  facet_wrap(~fct_rev(mode), ncol=1, scales="free") +
  scale_color_manual(values=c("#421B96","#F25A38")) +
  scale_fill_manual(values=c("#758d99")) +
  scale_y_continuous(labels=scales::percent, limits=c(0.6,1), expand=c(0,0)) +
  scale_x_date(breaks="3 months",date_minor_breaks = "1 month", guide = "axis_minor",
               limit=range(df2$value), labels=scales::date_format("%d %b\n%Y")) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid = element_line(size=.4),
        text=element_text(family=f1),
        strip.text=element_text(size=12, margin=margin(b=10), face="bold"),
        legend.title=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.ticks.x=element_line(),
        axis.ticks.y=element_blank(),
        ggh4x.axis.ticks.length.minor = rel(.5),
        axis.ticks.length.x=unit(.3, "cm"),
        panel.grid.minor.x=element_blank(),
        axis.line.x=element_line(),
        plot.margin=margin(.5,.9,.3,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size=9, lineheight = 1),
        plot.caption = element_text(size=8.5),
        panel.spacing = unit(1.8, "lines"),
        axis.text=element_text(size=8.5),
        axis.title=element_blank()) +
  guides(color=guide_legend(reverse=T, order=1)) +
  labs(caption="\nSource: Metropolitan Transportation Authority & data.ny.gov",
       title="Mask wearing on NYC transit",
       subtitle="Since June 2020, New York’s Metropolitan Transportation Authority has surveyed mask usage in a sample of subways and at\nits busiest bus stops. The results shows the percentages of people observed (nearly 5 million so far) found to be wearing a mask\ncorrectly, incorrectly, or not at all, during each biweekly survey cycle from Jun 15, 2020 to Apr 15, 2022.")

# Save plot
ggsave("masks_nyc_transit.png",width=8, height=8, bg="#fafafa")
