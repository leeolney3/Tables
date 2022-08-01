# Venture studios and their startups
# Data source: Jim Moran’s [Venture Studio Index](https://www.venturestudioindex.com/p/the-venture-studio-index) by way of [Data is Plural](https://www.data-is-plural.com/archive/2022-07-27-edition/)
# Credits to Georgios Karamanis for network plot method

# Load libraries
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

# Import data
startups= readr::read_csv("data/Startups.csv") %>% janitor::clean_names()

# Wrangle 
df1 = startups %>% 
  filter(end_customer!="unknown", studio!="Founder's Factory") %>%
  distinct(studio,name, end_customer) %>%
  group_by(studio) %>%
  mutate(n=n()) %>% filter(n>2)

df1c= df1 %>% filter(end_customer=="Consumer")
df1b= df1 %>% filter(end_customer=="Business")

df1s1 = df1 %>% filter(!studio %in% c("Saas Builders/BTwinz Ventures","btwinz"))
df1s2 = df1 %>% filter(studio %in% c("Saas Builders/BTwinz Ventures","btwinz"))

graph = df1 %>% select("from"="studio","to"="name") %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(grp=case_when(name %in% df1$studio~"Venture studio",
                       name %in% df1c$name~"End customer: Consumer",
                       name %in% df1b$name~"End customer: Business"),
         is_studio = ifelse(name %in% df1s1$studio,TRUE,FALSE),
         is_studio_repel = ifelse(name %in% df1s2$studio,TRUE,FALSE))
         
# Plot
p1= ggraph(graph, layout = 'stress') +
  geom_edge_link(color="grey") +
  geom_node_point(aes(size=grp, color=grp)) +
  geom_text(aes(x,y,label=if_else(is_studio,str_wrap(name,11),NULL)), size=3, lineheight=.7) +
  ggrepel::geom_text_repel(aes(x,y,label=if_else(is_studio_repel,str_wrap(name,11),NULL)), size=3, lineheight=.7, direction="y") +
  scale_size_manual("Startup factories",values=c(1,1,3.5)) +
  scale_color_manual("Startup factories",values=c("#23CBC7","#F25F1B","#FFBD43")) +
  theme_void() +
  theme(legend.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=12),
        legend.position = c(.75,.9),
        plot.caption=element_text(hjust=.5,size=10, color="grey30", lineheight=1.1, margin=margin(t=-10)),
        plot.margin=margin(0,0,.5,0,unit="cm")) +
  guides(color=guide_legend(reverse=TRUE, title.position = "top"), 
         size=guide_legend(reverse=TRUE, title.position = "top")) +
  labs(caption="Note: Chart includes venture studios with more than 2 startups\nSource: Jim Moran’s Venture Studio Index")   
  
ggsave("p3.png", p3, height=11, width=14, bg="white")        