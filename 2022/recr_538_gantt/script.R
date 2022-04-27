# 2022-04-25
# Recreating FiveThirtyEight gantt chart, https://fivethirtyeight.com/features/who-the-census-misses/

# Libraries
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Saira Semi Condensed")
f1="Saira Semi Condensed"
font_add_google("Saira Condensed")
f2="Saira Condensed"
font_add_google("Roboto")
f3="Roboto"

# Data
x1= tribble(
  ~id,~CATEGORIES,~x,~xend,~left,~right,
  0.8, "white",1790,1840,"Free white\nfemales and\nmales",NA,
  2, "black or african american",1790,1840,"Slaves",NA,
  3, "other",1790,1840,"All other\nfree persons",NA,
  4, "black or african american",1815, 1840,"Free colored\npersons",NA
) %>%
  mutate(CATEGORIES=str_to_upper(CATEGORIES))
  
x2= tribble(
  ~id,~CATEGORIES,~x,~xend,~left,~right,
  0.8,"white",1790,2020,NA,"White",
  5,"black or african american",1850,2020,"Black","Black or\nAfrican American",
  6,"black or african american",1850,1890,"Mulatto",NA,
  6,"black or african american",1910,1920,NA,NA,
  7,"american indian or alaskan native",1860,2020,"Indian","American indian\nor alaskan native",
  8,"asian",1860,2020,"Chinese","Chinese",
  9,"asian",1890,2020,"Japanese","Japanese",
  10,"black or african american",1890,1890,"Quadroon",NA,
  11,"black or african american",1890,1890,"Octoroon",NA,
  12,"other",1910,2020,"Other","Some other race",
  13,"asian",1920,2020,"Filipino","Filipino",
  14,"asian",1920,1940,"Korean",NA,
  14,"asian",1970,2020,NA,"Korean",
  15,"asian",1920,1940,"Hindu",NA,
  16,"hispanic/latino",1930,1930,"Mexican",NA,
  17,"native hawaiian or pacific islander",1960,2020,"Hawaiian","Native Hawaiian",
  18,"native hawaiian or pacific islander",1960,1960,"Part-Hawaiian",NA,
  19,"american indian or alaskan native",1960,1960,"Eskimo",NA,
  19,"american indian or alaskan native",1980,1990,NA,NA,
  20,"american indian or alaskan native",1960,1960,"Aleut",NA,
  20,"american indian or alaskan native",1980,1990,NA,NA,
  21,"native hawaiian or pacific islander",1980,2020,"Guamanian","Chamorro",
  22,"native hawaiian or pacific islander",1980,2020,"Samoan","Samoan",
  23,"asian",1980,2020,"Vietnamese","Vietnamese",
  24,"asian",1980,2020,"Asian Indian","Asian Indian",
  25,"asian",1990,1990,"Other API",NA,
  26,"asian",1990,1990,"Asian or Pacific Islander",NA,
  27,"native hawaiian or pacific islander",2000,2020,"Other Pacific Islander","Other Pacific Islander",
  28,"asian",2000,2020,"Other Asian","Other Asian",
) %>%
  mutate(CATEGORIES=str_to_upper(CATEGORIES))
  
x3 = tribble(
  ~id,~CATEGORIES,~x,~xend,~left,~right,
  25,"asian",1990,1990,"Other API",NA,
  26,"asian",1990,1990,"Asian or Pacific Islander",NA,
)

# Plot
p1 = ggplot() +
  geom_segment(data=x2,aes(x=x, xend=xend, y=id, yend=id, color=CATEGORIES), 
               size=3, lineend = "round", key_glyph = "point") +
  #LHS text
  geom_text(data=x2,aes(x=x-4, y=id, label=left,color=CATEGORIES),
            size=3.3, hjust=1, lineheight=.8, family=f1) +
  #RHS text
  geom_text(data=x2,aes(x=xend+4, y=id, label=right,color=CATEGORIES),
            size=3.3, hjust=0, lineheight=.8, family=f1, fontface="bold") +
  
  geom_segment(data=x1,aes(x=x, xend=xend, y=id, yend=id), 
               size=3.2, lineend = "round", color="black") +
  geom_segment(data=x1,aes(x=x, xend=xend, y=id, yend=id,color=CATEGORIES), 
               size=2.8, lineend = "round") +
  #LHS text
  geom_text(data=x1,aes(x=x-4, y=id, label=left, color=CATEGORIES),
            size=3.3, hjust=1, lineheight=.8, family=f1) +
  
  scale_y_reverse(expand=c(0.02,0.02), limits=c(NA,0.7)) +
  scale_x_continuous(position="top", limits=c(1770,2060), 
                     breaks=c(1800,1850,1900,1950,2000)) +
  coord_cartesian(clip="off") +
  cowplot::theme_minimal_vgrid(13, line_size = 0.3) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(family=f2, color="grey30", size=9),
        plot.title=element_markdown(hjust=.5, face="plain", lineheight = 1.3, margin=margin(b=15)),
        plot.subtitle=element_markdown(size=10, lineheight = 1.3, margin=margin(b=15)),
        axis.line = element_blank(),
        plot.caption=element_text(family=f2, size=8, margin=margin(t=10)),
        legend.position = "none",
        legend.title=element_text(family=f3, size=9.5, face="bold"),
        legend.text=element_text(family=f3, size=9.5),
        axis.title.x=element_text(face="bold", hjust=0, size=9)
        ) +
  labs(x="Language when\nfirst introduced",title="**FiveThirtyEight Census Gantt chart**<br><span style = 'font-size:11pt; font-family=f3'>Timeline of all categories used to denote race on the United States decennial census, 1970-2020</span>",
       subtitle="**CATEGORIES**<br><span style='color:#F26D3B'>● WHITE</span> <span style='color:#41C1C9'>● BLACK OR AFRICAN AMERICAN</span> <span style='color:#D934A1'>● AMERICAN INDIAN OR ALASKAN NATIVE</span> <span style='color:#6141D9'>● ASIAN</span><br><span style='color:#2AAD53'>● NATIVE HAWAIIAN OR PACIFIC ISLANDER</span> <span style='color:#999999'>● OTHER</span> <span style='color:#5a5a5a'>◑ TWO CATEGORIES</span> <span style='color:#FC2C1D'>● HISPANIC/LATINO</span>",
       caption="SOURCE: U.S. CENSUS BUREAU, GOVERNMENT INFORMATIVE QUARTERLY, PEW RESEARCH CENTER") +
  scale_color_manual(values=c("#D934A1","#6141D9","#41C1C9","#FC2C1D",
                              "#2AAD53","#999999","#F26D3B")) +
  annotate(geom="text", family=f3, hjust=0, vjust=1.3, x=1845, y=.7, size=3,
           label="From 1790 to 1840, the census noted only the name\nof the household head. But Beginning in 1950, all individuals\nwere enumerated and a color category was added, shaping\nAmerican's perceptions of race.") +
  annotate(geom="text", family=f3, hjust=0, vjust=1.3, x=1945, y=14.5, size=3,
           label="The separate Hispanic/Latino ethnicity question\nwas added to the census in 1970.") +
  gggibbous::geom_moon(data=x3, aes(x=x, y=id, ratio=.5), size=3.3, 
                       fill="#2AAD53", color="white") 
                       
# Save
png("p1.png", width=8, height=8, unit='in', res=300)
print(p1)
dev.off()                      