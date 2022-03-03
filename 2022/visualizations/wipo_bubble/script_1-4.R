library(tidyverse)
df = read_csv("bubble_fpd/bubble_fpd.csv")

# fonts
library(showtext)
font_add_google("Lato")
font_add_google("Roboto")
font_add_google("Roboto Condensed")
font_add_google("Libre Franklin")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Rank chart: Top 10 economies by rank from 2017 to 2021
library(ggbump)
library(ggflags)
library(countrycode)
library(ggtext)

top10 = df %>% filter(RANK<=10) %>% pull(NAME)

dfb = df %>% filter(NAME %in% top10) %>% 
  mutate(c2 = countrycode(NAME, origin="country.name",destination="genc2c"),
         c2 = str_to_lower(c2))

dfb %>% 
  ggplot(aes(GIIYR, RANK*-1)) +
  geom_bump(aes(color=REG_UN, group=NAME), alpha=.85, size=4) +
  geom_vline(xintercept = seq(2017, 2021,1), color="#F2F2F2", size=.4) +
  geom_text(data= dfb %>% filter(GIIYR==2021),
            aes(x=2021.2, label=NAME), hjust=0, size=3.2, family="Roboto Condensed") +
  geom_flag(data=dfb %>% filter(GIIYR==2017, RANK<=10), aes(country=c2), size=8) +
  geom_flag(data=dfb %>% filter(GIIYR==2021), aes(country=c2), size=8) +
  geom_flag(data=dfb %>% filter(GIIYR==2019, RANK==10), aes(country=c2), size=8) +
  coord_cartesian(ylim=c(-10,-1)) +
  scale_y_reverse(name="Rank",breaks=seq(-10,-1,1), labels=abs) +
  scale_x_continuous(name="",limits=c(2017,2022.3), breaks=seq(2017,2021,1)) +
  scale_color_manual(values=c("#15607a","#7678ed","#00dca6","#fa8c00")) +
  theme_minimal(base_family ="Roboto Condensed") +
  theme(legend.position="top",
        panel.grid=element_blank(),
        plot.background = element_rect(fill="#F2F2F2", color=NA),
        legend.text=element_text(size=9.2),
        plot.title.position="plot",
        plot.caption.position="plot",
        plot.title=element_text(family="Roboto", face="bold"),
        plot.caption=element_text(family="Roboto", hjust=0, size=8, color="grey40"),
        legend.margin=margin(l=-47, t=2),
        plot.margin=margin(.6,.5,.5,.5, unit="cm"),
        axis.title=element_text(size=9, family="Roboto", color="grey10"),
        axis.text=element_text(size=9, family="Roboto", color="grey10")) +
 # guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  labs(color="",
       title="Global Innovation Index Ranking, 2017 - 2021",
       caption="Data source: WIPO")
       
ggsave("wipo_s2.png", width=7, height=5.4, unit="in")

# Tile map: GII score and GDP of all economies, 2021
library(biscale)
library(cowplot)

tile = read.csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv")
dft = tile %>% select(alpha.2,alpha.3,x,y) %>% rename(ISO3=alpha.3)

df21 = df %>% filter(GIIYR==2021)
dfj = df21 %>% left_join(dft) %>%
  mutate(alpha.2=case_when(NAME=="Namibia"~"NA", 
                           ISO3=="HKG"~"HK",
                           TRUE~alpha.2)) %>%
  mutate(x=ifelse(ISO3=="HKG",24,x),
         y=ifelse(ISO3=="HKG",7,y)) %>%
 bi_class(x=GDP, y=SCORE, style="quantile", dim=3)
 
p21a = dfj %>%
  ggplot(aes(x, y, fill=bi_class)) +
  geom_point(size=6.5, shape=21, color="white") +
  geom_text(data=dfj %>% filter(bi_class=="1-1"),
            aes(label=alpha.2), color="black", family="Roboto Condensed", size=3) +
  geom_text(data=dfj %>% filter(bi_class!="1-1"),
            aes(label=alpha.2), color="white", family="Roboto Condensed", size=3)+
  bi_scale_fill(pal="DkViolet", dim=3, guide="none") +
  cowplot::theme_map() +
  scale_y_reverse() +
  coord_equal()
  
p21b = bi_legend(pal = "DkViolet", 
            dim = 3,
            xlab = "GDP",
            ylab = "GII Score",
            size = 2.5) + 
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 8,
                                    color = "black", margin=margin(t=-5)),
        axis.title.y = element_text(size = 8,
                                    color = "black", margin=margin(r=-5)),
        legend.text = element_text(size = 5),
        legend.text.align = 0)

ggdraw() +
  draw_plot(p21a, 0, 0, 1, 1) +
  draw_plot(p21b, 0, 0.1, 0.25, 0.25) +
  draw_label("2021", fontface= "bold",
             color = "#22222b", size = 15, angle = 0, x =0.1, y = 0.34, hjust=0) 
             
ggsave("wipo_s1_2021.png",width=7, height=5, unit="in", bg="white")

# Tile map: GII score and GDP of all economies, 2020
df20 = df %>% filter(GIIYR==2020)
dfj20 = df20 %>% left_join(dft) %>%
  mutate(alpha.2=case_when(NAME=="Namibia"~"NA", 
                           ISO3=="HKG"~"HK",
                           TRUE~alpha.2)) %>%
  mutate(x=ifelse(ISO3=="HKG",24,x),
         y=ifelse(ISO3=="HKG",7,y)) %>%
 bi_class(x=GDP, y=SCORE, style="quantile", dim=3)
 
p20a = dfj20 %>%
  ggplot(aes(x, y, fill=bi_class)) +
  geom_point(size=6.5, shape=21, color="white") +
  geom_text(data=dfj %>% filter(bi_class=="1-1"),
            aes(label=alpha.2), color="black", family="Roboto Condensed", size=3) +
  geom_text(data=dfj %>% filter(bi_class!="1-1"),
            aes(label=alpha.2), color="white", family="Roboto Condensed", size=3)+
  bi_scale_fill(pal="DkViolet", dim=3, guide="none") +
  cowplot::theme_map() +
  scale_y_reverse() +
  coord_equal()
  
p20b = bi_legend(pal = "DkViolet", 
            dim = 3,
            xlab = "GDP",
            ylab = "GII Score",
            size = 2.5) + 
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 8,
                                    color = "black", margin=margin(t=-5)),
        axis.title.y = element_text(size = 8,
                                    color = "black", margin=margin(r=-5)),
        legend.text = element_text(size = 5),
        legend.text.align = 0)
        
ggdraw() +
  draw_plot(p20a, 0, 0, 1, 1) +
  draw_plot(p20b, 0, 0.1, 0.25, 0.25) +
  draw_label("2020", fontface= "bold",
             color = "#22222b", size = 15, angle = 0, x =0.1, y = 0.34, hjust=0) 
             
ggsave("wipo_s1_2020.png",width=7, height=5, unit="in", bg="white")

# Dot plot:  GDP (log) and GII Score, 2021
# method by Christopher Nicault https://twitter.com/cnicault/status/1496923075379699721/photo/1)
dfa = df21 %>% select(ISO3,ln_GDP,SCORE) %>%
  mutate(g = cut(ln_GDP, breaks=c(6,7,8,9,10,11,12)),
         s = cut(SCORE, breaks=c(10,20,30,40,50,60,70)))
        
  
dfa %>% count(g,s) %>%
  add_column(g2 = c(6,7,7,8,8,8,9,9,9,9,10,10,10,10,10,11,11,11,11,11)) %>%
  add_column(s2 = c(20,10,20,10,20,30,10,20,30,50,20,30,40,50,60,20,30,40,50,60)) %>%
  mutate(p = sqrt(g2)*sqrt(s2)) %>%
  ggplot(aes(x=g, y=s)) +
  geom_point(aes(size=n, color=p), show.legend=F) +
  geom_text(aes(label=n), size=4.1, color="white") +
  scale_size(range=c(8,17)) +
  scale_color_gradient2(low = "#FA7F08", high = "#348888", mid = "black", midpoint = 16) +
  scale_y_discrete(expand=c(0.05,0.05), position="right") +
  scale_x_discrete(expand=c(0.03,0.03)) +
  cowplot::theme_minimal_grid(11) +
  theme(panel.grid=element_blank(),
        plot.margin=margin(.5,.7,.5,.7, unit="cm"),
        axis.ticks=element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_text(margin=margin(t=9)),
        axis.title.y.right=element_text(margin=margin(l=11)),
        plot.title.position = "plot",
        plot.title=element_text(size=20),
        plot.subtitle=element_text(size=12)
        ) +
  labs(title="2021",
       subtitle="Count of Economies by GII Score and logarithm of GDP",
       x="Logarithm of GDP", y="GII Score")
       
ggsave("wipo_s3.png", height=6, width=7, unit="in", bg="white")

# Rank chart, Economies in Europe 2020 to 2021, GII Rank and Score
# method by Cedric Scherer https://github.com/z3tt/TidyTuesday/tree/master/plots/2021_02)

dfc = df %>% filter(GIIYR>=2020, REG_UN=="Europe") %>%
  mutate(c2 = countrycode(NAME, origin="country.name",destination="genc2c"),
         c2 = str_to_lower(c2)) %>%
  mutate(INCOME=factor(INCOME, levels=c("High income","Upper-middle","Lower middle")))
  
range(dfc$RANK)

dfc %>%
  ggplot(aes(GIIYR, RANK*-1)) +
  geom_bump(aes(color=INCOME, group=NAME), alpha=.85) +
  geom_flag(aes(country=c2), size=5) +
  geom_segment(data = dfc %>% filter(GIIYR==2021),
               aes(x=2021.2, xend=2021.2+SCORE/50,
                   y=RANK*-1, yend=RANK*-1, color=INCOME),
               size=4, lineend = "round") +
  geom_segment(data = dfc %>% filter(GIIYR==2020),
               aes(x=2019.8, xend=2019.8-SCORE/50,
                   y=RANK*-1, yend=RANK*-1,color=INCOME),
               size=4, lineend = "round") +
  geom_text(data = dfc %>% filter(GIIYR==2021),
            aes(x=2021.2+SCORE/50, label=round(SCORE,2)), size=3, hjust=1, color="white") +
  geom_text(data = dfc %>% filter(GIIYR==2020),
            aes(x=2019.8-SCORE/50, label=round(SCORE,2)), size=3, hjust=0, color="white") +
  geom_text(data = dfc %>% filter(GIIYR==2021),
            aes(x=2021.09, label=RANK), size=3, hjust=.5) +
  geom_text(data = dfc %>% filter(GIIYR==2020),
            aes(x=2019.91, label=RANK), size=3, hjust=.5) +
  scale_x_continuous(expand=c(0.02,0.02), breaks=c(2020,2021), position="top") +
  scale_y_continuous(limits=c(-85,.25), expand=c(0,0)) +
  scale_color_manual(values=c("#8360B0","#2a9d8f","#FF9E1A")) +
  theme_minimal() +
  theme(legend.position="top",
        plot.margin=margin(.5,.5,.5,.5,unit="cm"),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="black", size=10, face="bold"),
        panel.grid=element_blank(),
        plot.title.position = "plot",
        plot.title=element_text(face="bold", hjust=.5)
        ) +
  annotate(geom="text", x=2019, y=0, label="Score", size=3.2) +
   annotate(geom="text", x=2022, y=0, label="Score", size=3.2) +
  labs(color="Income group",
       title="Europe (2020-2021)")



