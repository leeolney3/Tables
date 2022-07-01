# 2022-07-02
# Recreating FiveThirtyEight's waffle [chart](https://blog.datawrapper.de/wp-content/uploads/2022/06/image24-2.png) from the article [What It’s Like To Open An Abortion Clinic Right Now, June 16](https://fivethirtyeight.com/features/what-its-like-to-open-an-abortion-clinic-right-now/)
# Data source: Caitlin Myers

# Load libaries
library(tidyverse)
library(ggwaffle)
library(ggbump)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import fonts
font_add_google("Barlow")
f2 = "Barlow"

# Data
df = tibble(
  year=c(rep(2013,4), rep(2014,4), rep(2015,4), rep(2016,4),rep(2017,4),rep(2018,4),rep(2019,4), rep(2020,4),rep(2021,4), rep(2022,4)),
  n = c(17L,10L,22L,46L,19L,22L,17L,37L,29L,
        15L,14L,32L,18L,29L,8L,28L,14L,25L,21L,25L,14L,
        10L,13L,20L,27L,12L,7L,20L,32L,13L,19L,19L,27L,14L,
        17L,20L,5L,5L,13L,7L)) %>%
  group_by(year) %>%
  mutate(grp1=row_number()) %>%
  ungroup() %>%
  mutate(grp2=case_when(grp1>=3~2,TRUE~1),
         yearid= year-min(year)) 

## opened/reopened df 
w1 = waffle_iron(df %>% filter(grp2==1, year==2013) %>% uncount(n), aes_d(group = grp1), rows=3) %>% mutate(year=2013) 
w2 = waffle_iron(df %>% filter(grp2==1, year==2014) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2014)
w3 = waffle_iron(df %>% filter(grp2==1, year==2015) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2015)
w4 = waffle_iron(df %>% filter(grp2==1, year==2016) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2016)
w5 = waffle_iron(df %>% filter(grp2==1, year==2017) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2017)
w6 = waffle_iron(df %>% filter(grp2==1, year==2018) %>% uncount(n), aes_d(group = grp1), rows=3) %>% mutate(year=2018) 
w7 = waffle_iron(df %>% filter(grp2==1, year==2019) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2019)
w8 = waffle_iron(df %>% filter(grp2==1, year==2020) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2020)
w9 = waffle_iron(df %>% filter(grp2==1, year==2021) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2021)
w10 = waffle_iron(df %>% filter(grp2==1, year==2022) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2022)

w = rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10) %>%
  mutate(yid= year-min(year),
         y2 = case_when(yid>0 ~ yid*3+1*yid+y, TRUE~as.double(y)))
         
## closed/reclosed df
x1 = waffle_iron(df %>% filter(grp2==2, year==2013) %>% uncount(n), aes_d(group = grp1), rows=3) %>% mutate(year=2013, x=-1*x) 
x2 = waffle_iron(df %>% filter(grp2==2, year==2014) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2014, x=-1*x)
x3 = waffle_iron(df %>% filter(grp2==2, year==2015) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2015, x=-1*x)
x4 = waffle_iron(df %>% filter(grp2==2, year==2016) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2016, x=-1*x)
x5 = waffle_iron(df %>% filter(grp2==2, year==2017) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2017, x=-1*x)
x6 = waffle_iron(df %>% filter(grp2==2, year==2018) %>% uncount(n), aes_d(group = grp1), rows=3) %>% mutate(year=2018, x=-1*x) 
x7 = waffle_iron(df %>% filter(grp2==2, year==2019) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2019, x=-1*x)
x8 = waffle_iron(df %>% filter(grp2==2, year==2020) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2020, x=-1*x)
x9 = waffle_iron(df %>% filter(grp2==2, year==2021) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2021, x=-1*x)
x10 = waffle_iron(df %>% filter(grp2==2, year==2022) %>% uncount(n), aes_d(group = grp1), rows=3)  %>% mutate(year=2022, x=-1*x)

x = rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) %>%
  mutate(yid= year-min(year),
         y2 = case_when(yid>0 ~ yid*3+1*yid+y, TRUE~as.double(y)))  
         
## labels df
labdf = tibble(
  x=seq(2,40,4), y=seq(2013,2022,1), lab = c("[<span style='color:#B7390D'>-41</span> <span style='font-size:7pt';>net change</span>]","[<span style='color:#B7390D'>-13</span>]","[<span style='color:#B7390D'>-2</span>]","[<span style='color:#6140D9'>+11</span>]","[<span style='color:#B7390D'>-7</span>]","[<span style='color:#B7390D'>-9</span>]","[<span style='color:#6140D9'>+12</span>]","[<span style='color:#6140D9'>+6</span>]","[<span style='color:#6140D9'>+4</span>]","[<span style='color:#B7390D'>-10</span>]")
)

# Plot
ggplot() +
  # annotation 
  ggbump::geom_sigmoid(aes(x = 26.5, xend = 40.4, y = -16.3, yend =-.65), smooth=4, direction="y", size=.3) +
  annotate(geom="segment", x=40.12, y=-.7, xend=40.12, yend=-.65, arrow=arrow(length=unit(.2,"cm")), size=.3)+
  annotate(geom="richtext", x=30.5,y=-16.7, label="Between Jan. 1, 2013, and<br>June 1, 2022, the U.S. had a total<br>net loss of <span style='color:#B94216;font-family: \"Barlow\"'><b>49</br></span> abortion clinics", vjust=1, size=3.5,label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  # circles
  ggforce::geom_circle(data=w, aes(x0=y2+.5, y0=x+.25, r=.4, fill=factor(group),size= factor(group)), 
                       show.legend = F) +
  ggforce::geom_circle(data=x, aes(x0=y2+.5, y0=x-1, r=.4, fill=factor(group),size= factor(group)), 
                       show.legend = F) +
  # legend
  ggforce::geom_circle(data= tibble(y=rep(18.5,4),x=c(0.4,12.5,26,26.5), group=c(2,4,1,3)), 
                       aes(x0=x, y0=y, r=.4, fill=factor(group),size= factor(group)), 
                       show.legend = F) +
  geom_text(data=tibble(y=rep(18.5,3),x=c(0.4,12.5,26.5), labels=c("clinics opened/reopened","clinics closed/reopened","Planned Parenthood Clinics")),aes(x+.8, y=y, label=str_to_upper(labels)), size=2.8, hjust=0) +
  # x-axis
  geom_hline(yintercept = 0) +
  ggtext::geom_richtext(data=labdf, aes(x=x+.5, y=0, label=y), label.color = NA, size=3.8) +
  ggtext::geom_richtext(data=labdf, aes(x=x+.5, y=-.9, label=lab),size=3.3, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), family=f2) +
  # notes
  geom_hline(yintercept = -26.8, size=.3, color="#656565") +
  geom_text(aes(x=0,y=-26, label="Some clinics have opened and closed multiple times since 2013, which means they are counted more than once."), size=2.8, color="#656565", hjust=0) +
  # scales
  scale_size_manual(values=c(.35,0,.35,0)) +
  scale_fill_manual(values=c("#8D6CB2","#AA91C6","#EC713A","#F2956B")) +
  scale_x_continuous(limits=c(0,41), expand=c(0,0)) +
  scale_y_continuous(limits=c(-27,19.5), expand=c(0,0)) +
  coord_equal(clip = "off") +
  theme_void(10) +
  theme(plot.caption=element_text(family=f1, color="#656565"),
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(lineheight=1.2)) +
  labs(caption=str_to_upper("Source: Caitlyn Myers"),
       title="Recreation of FiveThirtyEight's waffle chart",
       subtitle="Number of abortion clinics that have opened or closed in the U.S. since 2013, including\nPlanned Parenthood Clinics, as of June 1,2022") 

ggsave("p1.png",height=8, width=8, bg="white")  
            