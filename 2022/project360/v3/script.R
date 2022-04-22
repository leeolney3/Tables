# 2022-04-22
# 360 project

library(tidyverse)
library(ggtext)

library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Eczar")
f1="Eczar"

df1 = read_csv("20220420-results/scenario_01.csv") %>% janitor::clean_names()
df2 = read_csv("20220420-results/scenario_02.csv") %>% janitor::clean_names()
df3 = read_csv("20220420-results/scenario_03.csv") %>% janitor::clean_names()


df1a = df1 %>% group_by(month) %>% summarise(avg=mean(number_of_employees_that_left)) %>%
  mutate(s="Pixel Systems - Outdated work practices and no efforts to improve work culture",s2 ="Scenario 1")

df2a = df2 %>% group_by(month) %>% summarise(avg=mean(number_of_employees_that_left)) %>% mutate(s="Woodcoms - Outdated work practices with low efforts to improve work culture", s2 ="Scenario 2")

df3a = df3 %>% group_by(month) %>% summarise(avg=mean(number_of_employees_that_left)) %>% mutate(s="Sun Co. - Outdated work practices with moderate efforts to improve work culture",s2 ="Scenario 3")

df = rbind(df1a, df2a, df3a) %>%
  group_by(s) %>%
  mutate(cs = cumsum(avg)) %>%
  mutate(s = factor(s, levels=c("Pixel Systems - Outdated work practices and no efforts to improve work culture","Woodcoms - Outdated work practices with low efforts to improve work culture","Sun Co. - Outdated work practices with moderate efforts to improve work culture")))

## Section 1: Number of employees that left
### 1a: Stream plot
df %>%
  ggplot(aes(x=month, y=cs,fill=s)) +
  ggstream::geom_stream(bw=.4) +
   geom_vline(
      data = tibble(x = c(0, seq(30, 90, by = 30))),
      aes(xintercept = x),
      inherit.aes = F, 
      color = "grey88", 
      size = .4,
      linetype = "dotted"
    ) +
  theme(legend.position="top",
        legend.direction = "vertical",
        legend.title=element_blank()) +
  scale_y_continuous(position="right", expand=c(0,0)) +
  scale_x_continuous(breaks=seq(0,120,30), expand=c(0.001,0.001)) +
  scale_fill_manual(values=c("#4286ED","#00C4A7","#8A80FF")) +
  cowplot::theme_minimal_vgrid(12) +
  theme(text=element_text(family=f1),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        panel.grid=element_line(size=.4),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(margin=margin(l=7)),
        axis.line.y=element_blank(),
        axis.ticks=element_line(size=.4),
        axis.ticks.length.y=unit(.25, "cm"),
        legend.direction = "vertical",
        legend.position = "top",
        legend.title=element_blank(),
        ) +
  labs(x="Month",
       title="Accumulated number of employees that left")
       
ggsave("b1.png", bg="white", height=5, width=8)

### 1b: Line plot
df1b = df1 %>% mutate(s="Pixel Systems - Outdated work practices and no efforts to improve work culture", s2 ="Scenario 1")

df2b = df2 %>% mutate(s="Woodcoms - Outdated work practices with low efforts to improve work culture", s2 ="Scenario 2")

df3b = df3 %>% mutate(s="Sun Co. - Outdated work practices with moderate efforts to improve work culture", s2 ="Scenario 3")

dfb = rbind(df1b, df2b, df3b) %>% 
  group_by(s, run) %>%
  mutate(cs= cumsum(number_of_employees_that_left)) %>%
  mutate(s = factor(s, levels=c("Pixel Systems - Outdated work practices and no efforts to improve work culture","Woodcoms - Outdated work practices with low efforts to improve work culture", "Sun Co. - Outdated work practices with moderate efforts to improve work culture")))
  
df %>%
  ggplot(aes(month, cs, color=s)) +
  geom_line(size=1.5) +
  geom_point(data=dfb, aes(x=month, y=cs, color=s), alpha=.1, size=.2) +
  #geom_textpath(aes(label=s2),size=4.3, vjust = -0.5, hjust=.7, rich=TRUE, family=f1) +
  geom_vline(xintercept = max(df$month), color="grey80") +
  geom_text(data=df %>% group_by(s2) %>% filter(month==max(month)),
            aes(x=max(df$month)+2, label=scales::comma(cs)), 
            hjust=0, family=f1, fontface="bold", size=3.8, show.legend = F) +
  geom_text(data=df %>% filter(month==max(month), s2=="Scenario 1"),
            aes(x=max(df$month)+2, y=cs+100, label="120th\nmonth:"), 
            hjust=0, color="grey50", size=3.5, family=f1, fontface="bold", lineheight=1) +
  scale_color_manual(values=c("#4286ED","#00C4A7","#8A80FF")) +
  scale_x_continuous(breaks=seq(0,120,30), expand = expansion(mult = c(0.01, 0.1)),) +
  scale_y_continuous(expand=c(0,0)) +
  cowplot::theme_minimal_hgrid(12) +
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.title=element_blank(),
        text=element_text(family=f1),
        plot.title=element_text(size=15),
        plot.title.position = "plot",
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        axis.title.y = element_blank()
        ) +
  labs(x="Month",
       title="Accumulated number of employees that left")
       
ggsave("b2.png", height=5, width=8, bg="white")

### 1c: Tile plot
df1c = df1 %>% group_by(month) %>% summarise(avg=mean(number_of_employees_that_left)) %>%
  mutate(s="**Pixel Systems**<br><span style = 'font-size:9pt'>Outdated work practices and no<br>efforts to improve work culture</span>",s2 ="Scenario 1")

df2c = df2 %>% group_by(month) %>% summarise(avg=mean(number_of_employees_that_left)) %>% mutate(s="**Woodcoms**<br><span style = 'font-size:9pt'>Outdated work practices with low<br>efforts to improve work culture</span>", s2 ="Scenario 2")

df3c = df3 %>% group_by(month) %>% summarise(avg=mean(number_of_employees_that_left)) %>% mutate(s="**Sun Co.**<br><span style = 'font-size:9pt'>Outdated work practices with moderate<br>efforts to improve work culture</span>",s2 ="Scenario 2")

dfc = rbind(df1c, df2c, df3c) %>%
  mutate(s=factor(s, levels=c("**Pixel Systems**<br><span style = 'font-size:9pt'>Outdated work practices and no<br>efforts to improve work culture</span>","**Woodcoms**<br><span style = 'font-size:9pt'>Outdated work practices with low<br>efforts to improve work culture</span>","**Sun Co.**<br><span style = 'font-size:9pt'>Outdated work practices with moderate<br>efforts to improve work culture</span>")))
  
dfc %>% select(month, avg, s) %>%
  mutate(avg=round(avg)) %>%
  filter(month!=0) %>%
  group_by(s) %>%
  mutate(grp = cut(month, breaks=c(0,20,40,60,80,100,120), ordered=TRUE,
                   labels=c("Month 1-20", "Month 21-40", "Month 41-60", 
                           "Month 61-80", "Month 101-100", "Month 101-120"))) %>%
  ungroup() %>%
  group_by(s, grp) %>%
  mutate(id=row_number()) %>%
  ggplot(aes(id, fct_rev(grp), fill=avg)) +
  geom_tile(height=.6, width=.9) +
  scale_fill_gradient(high="#8A80FF",low="#00C4A7") +
  #scale_fill_gradient2(low="#4286ED",mid="#00C4A7",high="#8A80FF") +
  facet_wrap(~s) +
  cowplot::theme_minimal_grid(12) +
  theme(legend.position = "top",
        legend.title=element_blank(),
        legend.margin=margin(l=-75),
        plot.title.position = "plot",
        text=element_text(family=f1),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(),
        strip.text=element_markdown(size=11, lineheight = 1),
        plot.title=element_text(size=15, margin=margin(b=7)),
        plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        panel.grid=element_blank()) +
  guides(fill = guide_colorbar(title.hjust = .6, barwidth = unit(15, "lines"), barheight = unit(.5, "lines"))) +
  labs(title="Number of employees that left, by month")
  
ggsave("b3.png", height=5, width=8, bg="white")  

## Section 2: Turnover
t1 = read_csv("20220420-results/scenario_01_turnover.csv") %>% janitor::clean_names() %>% mutate(s="Pixel Systems - Outdated work practices and no efforts to improve work culture")

t2 = read_csv("20220420-results/scenario_02_turnover.csv") %>% janitor::clean_names() %>% mutate(s="Woodcoms - Outdated work practices with low efforts to improve work culture")

t3 = read_csv("20220420-results/scenario_03_turnover.csv") %>% janitor::clean_names() %>% mutate(s="Sun Co. - Outdated work practices with moderate efforts to improve work culture")

t1a = t1 %>% group_by(year, s) %>% summarise(turnover=mean(turnover)) 
t2a = t2 %>% group_by(year, s) %>% summarise(turnover=mean(turnover)) 
t3a = t3 %>% group_by(year, s) %>% summarise(turnover=mean(turnover)) 
dft = rbind(t1a, t2a, t3a) %>%
  mutate(s = factor(s, levels=c("Pixel Systems - Outdated work practices and no efforts to improve work culture","Woodcoms - Outdated work practices with low efforts to improve work culture","Sun Co. - Outdated work practices with moderate efforts to improve work culture")))

dfu = rbind(t1,t2,t3) %>%
  mutate(s = factor(s, levels=c("Pixel Systems - Outdated work practices and no efforts to improve work culture","Woodcoms - Outdated work practices with low efforts to improve work culture","Sun Co. - Outdated work practices with moderate efforts to improve work culture")))
  
### 2a: Beeswarm with average
dft %>%
  ggplot(aes(x=factor(year), y=turnover, color=s)) +
  ggbeeswarm::geom_beeswarm(data=dfu, size=.2, cex=.6, alpha=.8) +
  geom_line(aes(group=s), size=1, alpha=.9) +
  geom_point(aes(fill=s), shape=21, color="black", size=3, show.legend = F) +
  scale_color_manual(values=c("#4286ED","#00C4A7","#8A80FF")) +
  scale_fill_manual(values=c("#4286ED","#00C4A7","#8A80FF")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,40)) +
  cowplot::theme_minimal_hgrid(12) +
  theme(text=element_text(family=f1),
        legend.position="top",
        legend.direction = "vertical",
        legend.title=element_blank(),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title=element_text(size=15),
        plot.title.position = "plot",
        axis.title.y = element_blank()
        )  +
  labs(x="Year", 
       title="Average Employee Turnover Rate")

ggsave("b4.png", bg="white", height=6, width=8)

### 2b: Sina
dfu %>% 
  ggplot(aes(x=factor(year), y=turnover, color=s, fill=s)) +
  ggforce::geom_sina(scale="count", maxwidth=.6, seed=1, alpha=.5) +
  ggforce::geom_sina(maxwidth = .6, scale = "count", seed = 1, shape = 1, color = "black", stroke = .1
  ) +
  scale_color_manual(values=c("#4286ED","#00C4A7","#8A80FF")) +
  cowplot::theme_minimal_hgrid(12) +
  scale_y_continuous(expand=c(0,0), limits=c(0,40)) +
  theme(text=element_text(family=f1),
        legend.position="top",
        legend.direction = "vertical",
        legend.title=element_blank(),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title=element_text(size=15),
        plot.title.position = "plot",
        axis.title.y = element_blank()) +
  labs(x="Year",
       title="Employee Turnover Rate",
       subtitle="Each point represents one run, 100 runs per scenario for each year")

ggsave("b5.png", bg="white", height=6, width=8)  