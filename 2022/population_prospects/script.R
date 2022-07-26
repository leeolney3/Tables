# UN World Population Prospects 2022
# Data source: United Nations [World Population Prospects 2022](https://population.un.org/wpp/Download/Standard/CSV/)

# Load libraries
library(tidyverse)
library(ggtext)
library(geofacet)
library(ggbraid)
library(showtext)
library(ggshadow)
library(ggnewscale)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load fonts
font_add_google("Libre Franklin")
f1 = "Libre Franklin"
font_add_google("Fira Sans")
f2 = "Source Sans 3"
font_add_google("Fira Sans Condensed")
f3 = "Fira Sans Condensed"

# Section 1: EU27 births and deaths
df = popun %>%
  filter(!is.na(ISO3_code),Time<=2100) %>%
  mutate(reg = countrycode::countrycode(ISO3_code, origin="iso3c", destination="eu28")) %>%
  filter(!is.na(reg), Location!="United Kingdom") %>%
  mutate(Location=case_when(Location=="Czechia"~"Czech Republic",TRUE~Location)) %>%
  select(ISO3_code,ISO2_code,Location,Time, `Crude Birth Rate`= CBR, `Crude Death Rate`=CDR) 

df_long = df %>% pivot_longer(5:6)

p1 = df_long %>% 
  ggplot() +
  geom_vline(xintercept=2022, size=.2, color="grey20", linetype="dotted") +
geom_line(aes(x=Time, y=value, linetype=name), size=.4) +
  ggbraid::geom_braid(aes(x=Time, ymin=`Crude Birth Rate`, ymax=`Crude Death Rate`, fill=`Crude Birth Rate`<`Crude Death Rate`), data=df, alpha = 0.85, method = 'line')+
  geom_text(data=df_long %>% filter(Location=="Sweden",Time %in% c(2016, 2028), name=="Crude Birth Rate"), aes(x=Time, y=27.5, label=c("←Estimate","Forecast→")),hjust=c(1,0), family=f3, size=2.4, color="grey20") +
  facet_geo(~Location,grid = grid1) +
  scale_fill_manual(values=c("#52B0AE","#A21314"), labels=c("Crude Birth Rate > Crude Death Rate","Crude Death Rate > Crude Birth Rate")) +
  scale_x_continuous(breaks=c(1950,2022,2100)) +
  scale_y_continuous(limits=c(5,NA), breaks=seq(5,30,5)) +
  coord_cartesian(expand=FALSE, clip="off") +
  cowplot::theme_minimal_grid(9.5) +
  theme(text=element_text(family=f2),
        panel.grid.major= element_blank(),
        legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=8.5),
        legend.box.margin = margin(l=-15),
        axis.ticks=element_line(size=.3, color="grey30"),
        axis.text.y=element_text(size=7,color="grey30"),
        axis.text.x=element_text(size=7,color="grey30", hjust=c(0.1,.5,.9)),
        axis.title=element_blank(),
        panel.spacing.x = unit(.9, "lines"),
        strip.text=element_text(size=8.5),
        plot.title.position="plot",
        plot.subtitle=element_text(lineheight=1.1,size=9, margin=margin(b=9)),
        plot.title=element_text(size=11.5, family=f1),
        plot.caption = element_text(color="grey40", margin=margin(t=10)),
        plot.margin=margin(.4,.4,.2,.4,unit="cm")
        ) +
  guides(linetype=guide_legend(order=1)) +
  labs(caption="Source: population.un.org  •  Plot inspired by Georgios Karamanis @geokaramanis",
       title="Births and Deaths, 1950 - 2100",
       subtitle="EU27 countries, Crude Birth Rate (births per 1 000 population) and Crude Death Rate (deaths per 1 000 population) according\nto UN World Population Prospects 2022 medium projection scenario")

ggsave("unpop22_p1.png", bg="white")


# Section 2: Comparing Ireland's demographic indicators to other EU27 countries, using John Burn-Murdoch's Financial Times graphic style, https://twitter.com/jburnmurdoch/status/1539248155446980608

des = c(`CBR`="Births per 1,000 population",`MAC`="Mean age childbearing",`SRB`="Males per 100 female births",`NNR`="Surviving daughters\nper woman")

df2 = pop %>%
  filter(!is.na(ISO3_code),Time<=2100) %>%
  mutate(reg = countrycode::countrycode(ISO3_code, origin="iso3c", destination="eu28")) %>%
  filter(!is.na(reg), ISO2_code!="GB") %>%
  select(ISO3_code,ISO2_code,Location,Time,CBR,NNR,SRB,MAC) %>%
  pivot_longer(CBR:MAC) 

df2a = df2 %>% filter(Location!="Ireland") %>%
  group_by(name,Time) %>%
  summarise(min=min(value),
            max=max(value))

## Load fonts
font_add_google("Inter")
font_add_google("PT Sans")
f1 = "Inter"
f2 = "PT Sans"

## Plot
p2 = df2 %>%
  ggplot(aes(x=Time, y=value)) +
  geom_ribbon(data=df2a, aes(x=Time, ymin=min, ymax=max), inherit.aes = FALSE, fill="grey92") +
  geom_line(data= df2 %>% filter(Location!="Ireland"), aes(group=Location),size=.2, color="grey", alpha=.9) +
  geom_line(data= df2 %>% filter(Location=="Ireland"), aes(group=Location),size=.5, color="#C4373E") +
  geom_text(data=tibble(name=c("CBR","NNR","SRB","MAC"), Time=rep(2100,4), value=c(10.5,.87,100.9,31.2), Location=rep("Ireland",4)), aes(label=Location, y=1.05*value), size=3, color="#C4373E", hjust=1, fontface="bold", family=f2) +
  shadowtext::geom_shadowtext(data=tibble(name="MAC", Time=1982, value=27.5, label="EU countries"), aes(label=label),size=2.8, color="grey20",bg.color="white", family=f2) +
  facet_wrap(~name, nrow=1, scales="free",labeller = as_labeller(des)) +
  scale_x_continuous(expand = expansion(mult = c(0, .03))) +
  scale_size_identity() +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_hgrid(9.5, line_size=.35) +
  theme(text=element_text(family=f1),
        axis.line.x = element_blank(),
        plot.margin=margin(1,1,1,.5,unit="cm"),
        axis.title=element_blank(),
        axis.text.x=element_text(color="grey30", size=7.7, hjust=0.1),
        axis.text.y=element_text(color="grey30", size=7.7),
        axis.ticks.length=unit(.17, "cm"),
        axis.ticks.x=element_line(color="grey40"),
        axis.ticks.y=element_line(color=c("grey40",rep("grey85",6))),
        panel.grid.major.y = element_line(color=c("grey40",rep("grey85",6))),
        strip.text = element_text(hjust=0, size=9.2, family=f2, color="black"),
        panel.spacing.x = unit(1.4, "lines"),
        plot.title.position = "plot",
        plot.title=element_text(size=11,face="plain",margin=margin(b=14)),
        plot.caption.position = "plot",
        plot.caption=element_text(size=6.7, lineheight=1.1, color="grey45", hjust=0, margin=margin(t=15))) +
  ggh4x::facetted_pos_scales(
    y = list(
      name == "CBR" ~ scale_y_continuous(breaks=seq(5,32,9), limits=c(5,32), expand=c(0,0)),
      name == "MAC" ~ scale_y_continuous(breaks=seq(23,35,4),limits=c(23,35), expand=c(0,0)),
      name == "SRB" ~ scale_y_continuous(limits=c(103,109),breaks=seq(103,109,2),expand=c(0,0)),
      name == "NNR" ~ scale_y_continuous(breaks=seq(.5,2,.5), limits=c(.5,2), expand=c(0,0))
    )
  ) +
  labs(title="Ireland's demographic indicators compared to EU27 countries, 1950-2100",
       caption="Note: Medium scenario projection from UN World Population Prospects 2022. The medium scenario\nprojection corresponds to the median of several thousand distinct trajectories of each demographic\ncomponent derived using the probabilistic model of the variability in changes over time.\nSource: population.un.org")

ggsave("p2.png", p2, height=4, width=9, bg="white")

# Section 3: Recreating The Economist's chart on annual change in population in The pecking order of the world’s population is soon to change (14 July 2022), https://blog.datawrapper.de/wp-content/uploads/2022/07/image6-2.png

## p1 data
df3 = pop %>% filter(!is.na(ISO3_code)) %>%
  mutate(grp=case_when(Location %in% c("India","Nigeria","Ethiopia","Pakistan","Democratic Republic of the Congo")~1,
                       Location %in% c("China","United States of America","Brazil","Japan")~2,TRUE~3)) %>%
  mutate(PopChange=PopChange/1000)

lab1 = tibble(x=c(1960,1980,1994,2013,2020.5,2024),
       y=c(-1.8,11,-3.8,-3,8,-6.5),
       label=c("Great Leap<br>Forward","One-child<br>policy begins","Rwandan<br>genocide","Syrian<br>civil war","Covid-19<br>pandemic","War in<br>Ukraine")
)

lab2 = tibble(
  x=c(1957.5,2043,2070,2083),
  y=c(17.5,-1.4,1.4,-2),
  label=c("China","Japan","US","Brazil")
)

lab3 = tibble(
  x=c(2000,2031,2038,2059,2100),
  y=c(17.5,5.9,3.2,3.8,4.2),
  label=c("India","Nigeria","Ethiopia","Pakistan","Congo")
)

## Fonts
font_add_google("Fira Sans")
font_add_google("Fira Sans Condensed")
f1 = "Fira Sans"
f2 = "Fira Sans Condensed"

## Plot
p3 = df3 %>% filter(grp==3) %>%
ggplot(aes(Time,PopChange,group=Location)) +
  #y-axis
  geom_segment(data=tibble(x=1942, xend=2111, y=seq(-15,25,5)),aes(x=x, xend=xend, y=y, yend=y),inherit.aes = FALSE, size=.35, color=c("#514D49",rep("#cdcccc",2),"#514D49",rep("#cdcccc",5))) +
  geom_text(data=tibble(x=2111, y=seq(-15,25,5)), aes(x=x, y=y+.7, label=y), inherit.aes = FALSE, size=3, hjust=1, color="#464645", family=f1) +
  # estimate forecast line
  annotate(geom="segment", x=2022, xend=2022, y=14, yend=25, linetype="dotted", color="#464645") +
  annotate(geom="segment", x=2022, xend=2022, y=-8, yend=-15, linetype="dotted", color="#464645") +
  annotate(geom="text",x=c(2023.5,2020.5), y=c(24,24), label=c("Forecast →","← Estimate"), hjust=c(0,1), color="#464645", size=3.5, family=f2) +
  #lines
  geom_line(color="#DCDBD7") +
  ggshadow::geom_shadowline(data=df3 %>% filter(grp==2), aes(color="Other countries"), size=.65, shadowsize=1.5) +
  scale_color_manual(values="#999789", labels="<span style='color:#777567'>Other countries</span>") +
  ggnewscale::new_scale_color() + #to create space between legend key/text
  ggshadow::geom_shadowline(data=df3 %>% filter(grp==1), aes(color="Top five countries contributing to population growth"), size=.65, shadowsize=1.5) +
  scale_color_manual(values="#EF563A", labels="<span style='color:#EF563A'>Top five countries<br>contributing to<br>population growth</span>") +
  guides(color=guide_legend(reverse=TRUE)) +
  scale_x_continuous(breaks=seq(1950,2100,10),labels=c("1950","60","70","80","90","2000","10","20","30","40","50","60","70","80","90","2100")) +
  coord_cartesian(ylim=c(-15,25), expand=FALSE,clip="off") +
  cowplot::theme_minimal_grid(10) +
  theme(text=element_text(family=f1),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="#464645"),
        axis.ticks.length = unit(.3,"lines"),
        axis.ticks.x=element_line(color="#464645", size=.35),
        axis.ticks.y=element_blank(),
        legend.position = c(.65,.75),
        legend.title=element_blank(),
        legend.text=element_markdown(lineheight = 1.2, size=9, face="bold"),
        legend.spacing.y = unit(.2,"lines"),
        legend.box.background = element_rect(fill="white",color=NA),
        panel.grid=element_blank(),
        plot.margin=margin(.7,.8,.7,.6,"cm"),
        plot.title=element_text(color="#322D28",size=10.7),
        plot.subtitle = element_markdown(lineheight=1.3, color="#322D28", margin=margin(b=10)),
        #plot.caption = element_text(color="grey30",margin=margin(t=8))
        ) +
  geom_richtext(data=lab1, aes(x=x, y=y, label=label), inherit.aes = FALSE, family=f2, size=3, lineheight=1,hjust=c(rep(.5,4),rep(0,2)), color="#464645", label.padding = grid::unit(rep(0, 4), "pt"),label.color = NA,fill="white") +
  geom_text(data=lab2, aes(x=x, y=y, label=label), inherit.aes = FALSE, family=f1, size=3, color="#7F7E6F") +
  geom_richtext(data=lab3, aes(x=x, y=y, label=label), inherit.aes = FALSE, family=f1, size=2.9, color="#EF563A",label.padding = grid::unit(rep(0, 4), "pt"),label.color = NA,fill="white", hjust=c(rep(.5,4),1)) +
  labs(title="Annual change in population, m",
       subtitle="Recreation of The Economist's chart in *The pecking order of the world’s population is soon to change*, July 14<br>Data source: United Nations World Population Prospects 2022")
       
## Add line and save plot
png("p3.png", width=7.5, height=6.5,unit='in',res=300)

#function reference: https://stackoverflow.com/questions/64656234/how-does-the-economist-make-these-lines-near-the-title-using-using-ggplot
annotate_npc <- function(x, y, height, width, ...) {
  grid::grid.draw(grid::rectGrob(
    x = unit(x, "npc"), y = unit(y, "npc"), height = unit(height, "npc"), width = unit(width, "npc"),
    gp = grid::gpar(...)
  ))
}

p3
annotate_npc(x = 0.055, y = 0.967, height = 0.002, width = 0.03, fill = "black", col = NA)
dev.off()