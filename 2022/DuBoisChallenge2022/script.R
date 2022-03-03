# DuBoisChallenge2022 <https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022>
# Challenge 02, 07, 05, 03, 10


library(tidyverse)
library(tidytext)

# challenge02 
data = read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge02/data.csv") %>% janitor::clean_names()
data = data %>% distinct()

d1 = data %>% mutate(r1=5393885, r2=5764293, r3= 8153390, r4=12322003, r5=12941230, r6=13447423)

data %>%
  ggplot(aes(x=year, y=valuation_dollars, fill=factor(year))) +
  # circles
  geom_col(data=d1, aes(x=year, y=r6), fill="#C51334", width=7) +
  geom_col(data=d1, aes(x=year, y=r5), fill="#E0CAB0", width=7) +
  geom_col(data=d1, aes(x=year, y=r4), fill="#EFB700", width=7) +
  geom_col(data=d1, aes(x=year, y=r3), fill="#344D87", width=7) +
  geom_col(data=d1, aes(x=year, y=r2), fill="#A38067", width=7) +
  geom_col(data=d1, aes(x=year, y=r1), fill="#181818", width=7) +
  # columns
  geom_col(data=data %>% filter(year!=1875), aes(fill=factor(year)), width=1.3, show.legend=F) +
  scale_fill_manual(values=c("black","#A38067","#344D87","#EFB700","#D5C0A9","#C51334")) +
  # value labels
  geom_text(data = data %>% filter(year!=1875), aes(label=scales::dollar(valuation_dollars)), color="black", size=3.5, nudge_y = -1800000, angle=c(65,-60,0,60,-75)) +
  geom_text(data = data %>% filter(year==1885), aes(label=scales::dollar(valuation_dollars)), color="white", size=3.5, nudge_y = -1800000, angle=-60) +
  geom_text(data = data %>% filter(year==1875), aes(label=scales::dollar(valuation_dollars)), color="white", size=3.5, nudge_y = -2200000) +
  scale_x_reverse() +
  coord_polar(start=13447423) +
  theme_void(base_family = "mono") +
  theme(plot.background = element_rect(fill="#E9D8C5", color=NA),
        plot.title=element_text(hjust=.5, size=17),
        plot.margin=margin(.75,.5,.5,.5, unit="cm"),
        plot.caption=element_text(hjust=.5, color="grey30", size=9.5)
  ) +
  labs(title=str_to_upper("Assessed Valuation of all Taxable Property\nOwned by Georgia Negroes"),
       caption="#TidyTuesday #DuboisChallenge2022 | Data source: Anthony Starks") +
  # year labels
  geom_text(data= data %>% filter(year!=1875 & year!=1885),aes(x=1882, y=valuation_dollars-240000, label=year), color="black", size=2.9) +
  geom_text(data= data %>% filter(year==1875 | year==1885),aes(x=1882, y=valuation_dollars-500000, label=year), color="white", size=3.2) 

ggsave("challenge02.png", height=9, width=8, unit="in")

# challenge 07
data = read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv",show_col_types = FALSE)

library(showtext)
font_add_google("Teko","teko")
font_add_google("EB Garamond")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1 ="teko"
f2 ="EB Garamond"

df = data %>% 
  mutate(Group=case_when(Group=="Over 65"~"Ages.<br>Over 65", TRUE~Group),
         Group=fct_inorder(Group)) %>%
  pivot_longer(!Gender:Group) %>%
  mutate(value= case_when(Gender=="Male"~-1*value, TRUE~value)) 

dflabs1 = levels(df$Group)
dflabs2 = levels(df$Group)

c07 = df %>%
  ggplot(aes(y=value,x=as.numeric(Group))) +
  geom_col(aes(fill=factor(name, levels=c("Widowed","Married","Single"))), show.legend=F,width=1, alpha=.95) +
  geom_hline(yintercept=seq(-100,100,2), color="black", alpha=.1, size=.3) +
  geom_hline(yintercept=seq(-100,100,10), color="black", alpha=.2, size=.4) +
  geom_hline(yintercept=0, color="black", alpha=.5, size=.4) +
  geom_vline(xintercept=seq(0.5,9.5,1), color="black", alpha=.2, size=.4) +
  scale_x_continuous(breaks = 1:length(dflabs1),
                     labels = str_to_upper(dflabs1),
                     sec.axis = dup_axis()) +
  scale_fill_manual(values=c("#3D8861","#CF1A42","#0058AC")) +
  scale_y_continuous("PER CENTS.",breaks=seq(-100,100,10), labels=abs(seq(-100,100,10))) +
  coord_flip(expand=F, clip="off") +
  theme_minimal() +
  theme(plot.background=element_rect(fill="#E0D5C8", color=NA),
        plot.title=element_markdown(hjust=.5, lineheight = 2.2, color="#7E6A52", family=f2, size=12),
        plot.margin=margin(.8,.4,1.2,.4, unit="cm"),
        axis.text.y.left =element_markdown(family="mono", size=10.5, color="black", 
                                           face="bold", vjust=-0.57),
        axis.text.y.right =element_markdown(family="mono", size=10.5, color="black",
                                            face="bold", vjust=-0.57),
        axis.text.x=element_text(family=f1, color="black", size=12, margin=margin(t=0)),
        axis.title.y=element_blank(),
        axis.title.x=element_text(family="mono", face="bold", margin=margin(t=4.5)),
        panel.grid = element_blank(),
        plot.caption=element_text(color="#7E6A52", family=f2, hjust=.5, size=10)
  ) +
  labs(title="<span style = 'font-size:17pt'>Conjugal condition of American Negroes according to age periods.</span><br>Condition conjugale des Negres Americains au point de vue de l'age.<br><span style = 'font-size:10pt'>Done by Atlanta University.</span><br>",
       caption="\n#TidyTuesday #DuboisChallenge2022 | Data source: Anthony Starks") +
  annotate(geom="rect", ymin=-100.2, ymax=100.2, xmin=9.5, xmax=9.7, color=NA, fill="#E0D5C8") +
  annotate(geom="text", y=c(-50,50), x=c(9.62,9.62), label=c("MALES.","FEMALES."), 
           family="mono", fontface="bold", size=4.4) +
  annotate(geom="text", y=c(-38,-55,-93), x=c(1.8,5.5,8.5), label=c("SINGLE","MARRIED","WIDOWED"),
           angle=c(52,52,65), family=f1, size=c(7.2,7.2,5.3)) +
  annotate(geom="text", y=c(38,55,89), x=c(1.8,5.5,7.5), label=c("SINGLE","MARRIED","WIDOWED"),
           angle=c(-52,-52,-63), family=f1, size=c(7.2,7.2,5.5)) 

library(cowplot)
cowplot::set_null_device("png")
cowplot::plot_grid(c07) +
  # add lines in title
  draw_line(x=c(0.42,0.58),y=c(0.905,0.905), size=.3, color="#7E6A52", alpha=.3) +
  draw_line(x=c(0.42,0.58),y=c(0.9405,0.9405), size=.3, color="#7E6A52", alpha=.3)

ggsave("challenge07.png",height=10, width=8.5)

# challenge 05
data5 = read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge05/data.csv",show_col_types = FALSE)

data5 = data5 %>%
  add_row(Year=1863, Slave=97, Free=3)

lab5 = data5 %>% 
  mutate(Free=ifelse(Free==100.0,"100%",Free),
                        Free=ifelse(Free==1.3,"1.3%",Free)) %>%
  filter(Year!=1863)

c05 = data5 %>% pivot_longer(Slave:Free) %>%
  ggplot(aes(x=Year, y=value)) +
  geom_area(aes(fill=name), show.legend=F, color="#E3D4C3") +
  geom_vline(xintercept=lab5$Year, color="#E3D4C3") +
  scale_x_reverse(breaks=lab5$Year, expand=c(0,0), 
                  sec.axis = sec_axis(~ . * 1, breaks = lab5$Year, labels = lab5$Free)
  ) +
  scale_y_continuous(expand=c(0,0), position="right",
                     breaks=c(97,98,99,100), labels=c("3%","2%","1%","")) +
  scale_fill_manual(values=c("#CD2642","#1E1B19")) +
  coord_flip(ylim=c(97,100)) +
  theme_minimal(base_family = "mono") +
  theme(plot.margin=margin(.8,3.4,.5,3.4,"cm"),
        axis.title=element_blank(),
        axis.text.y.left =element_text(margin=margin(r=17), size=8.3,color="grey40"),
        axis.text.y.right =element_text(margin=margin(l=17), size=8.3,color="grey40"),
        plot.background = element_rect(fill="#E3D4C3", color=NA),
        plot.title=element_text(hjust=.5, margin=margin(b=30)),
        plot.caption.position="plot",
        plot.caption=element_text(hjust=.5, margin=margin(t=40), size=7),
        axis.ticks.x.top = element_line(size=.3, color="grey50"),
        axis.text.x.top = element_text(size=7, color="grey40")
  ) +
  labs(title=str_to_upper("#DuBoisChallenge2022 Challenge 05"),
       caption="#TidyTuesday #DuboisChallenge2022 | Data source: Anthony Starks")

plot_grid(c05) + draw_text("PERCENT", x=.76, y=.89, size=7, color="grey40", family="mono")

ggsave("challenge05.png",width=6, height=7.5, unit="in")


# challenge 03
#Annual mean wage for Veterinarians (May 2020)
#Data source: https://data.bls.gov/oes/#/occGeo/One%20occupation%20for%20multiple%20geographical%20areas

# Data: copy tribble from https://pastebin.com/49finRvU
summary(data3$`Annual.mean.wage(2)`)

df = data3 %>% janitor::clean_names() %>%
  mutate(state_code = str_extract(area_name, '(?<=\\()[0-9-]+(?=\\))'),
         state=gsub('[0-9]+', '', area_name),
         state=gsub("\\(|\\)", "", state)) %>%
  mutate(annual_mean_wage_2 = annual_mean_wage_2/1000,
         wage = cut(annual_mean_wage_2, breaks=seq(70,130,10),
                    labels=c("70,000 - 80,000","80,000 - 90,000","90,000 - 100,000",
                             "100,000 - 110,000","110,000 - 120,000","120,000 - 130,000"))) 

df_map = df %>% select(state,wage)

library(usmap)
base_map = plot_usmap(data=df, regions="states", values="wage", size=.1)
c03= base_map +
  scale_fill_manual(values=c("#DB3450","#2C2049","#C8B7A6","#E2B0A6","#F2AB00","#734C34")) +
  theme(plot.title=element_text(hjust=.5, margin=margin(b=80), face="bold", size=13),
        plot.margin=margin(0,1.5,.5,1.5, unit="cm"),
        legend.position ="bottom",
        legend.justification = "center",
        legend.key.size = unit(.5,"cm"),
        legend.spacing.y = unit(1,"lines"),
        plot.background=element_rect(fill="#E2D1C1", color=NA),
        legend.background = element_blank(),
        text=element_text(family="mono", lineheight = 1),
        legend.text=element_text(color="grey30")
  ) +
  labs(title=str_to_upper("Annual Mean Wage of Veterinarians\nby State, as of May 2020"),
       fill=NULL) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) 

ggsave(plot=c03,"challenge03.png", height=7, width=5.8)


# challenge 10

d10 = read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge10/data.csv",show_col_types = FALSE) %>% janitor::clean_names()

d10a= d10 %>% 
  mutate(percent_enrolled=ifelse(year==1896,77.29,percent_enrolled)) %>%
  add_column(not_enrolled = c(50,50,59)) %>%
  rename("OF CHILDREN NOT ENROLLED"=not_enrolled,
         "OF CHILDREN ENROLLED"=percent_enrolled) %>%
  pivot_longer(2:3)

d10a %>%
  ggplot(aes(factor(year), y=value, fill=fct_rev(name))) +
  geom_col(width=.51, alpha=.95) +
  geom_text(data = d10a %>% filter(name=="OF CHILDREN ENROLLED"),
            aes(label=scales::number(d10$percent_enrolled, accuracy=0.01, suffix="%")),
            position = position_stack(vjust = .5),family="mono", size=5, fontface="bold") +
  scale_x_discrete(position="top") +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_manual(values=c("#030101","#dc143c"), 
                    labels=c("OF CHILDREN NOT ENROLLED","OF CHILDREN ENROLLED")) +
  theme_minimal(base_family="mono") +
  theme(plot.margin=margin(1,1,2,1, unit="cm"),
        axis.text.y=element_blank(),
        axis.title=element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(family=f1, size=12.5, hjust=.5, lineheight = 1.75, 
                                  margin=margin(b=20)),
        axis.text.x.top = element_text(family="mono", face="bold", size=12.5, color="black"),
        plot.background=element_rect(fill="#DFD4C8", color=NA),
        panel.grid=element_blank(),
        legend.position = c(.2,.12),
        legend.text=element_blank(),
        legend.title=element_blank(),
        legend.key.size = unit(.8, 'cm'),
        legend.spacing.y = unit(.8, 'cm'),
        plot.subtitle = element_text(size=8, hjust=.5, margin=margin(b=30))
  ) +
  labs(title=str_to_upper("Proportion  Of  Total  Negro  Children  Of  School  Age  Who  Are  Enrolled  In  The  Public  School\n#DuboisChallenge2022  Challenge 10"),
       subtitle="#TIDYTUESDAY WEEK 7") +
  guides(fill=guide_legend(byrow=T)) +
  annotate(geom="text",family="mono", size=2.8, hjust=1,color="grey30",
           x=c(.9,.9), y=c(116,133), label=c("PROPORTION","PROPORTION")) +
  annotate(geom="text",family="mono", size=2.8, hjust=0,color="grey30",
           x=c(1.13,1.13), y=c(116,133), label=c("OF CHILDREN ENROLLED","OF CHILDREN NOT ENROLLED")) 

ggsave("challenge10.png", height=7.6, width=6)





