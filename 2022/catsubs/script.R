# Subreddits of cats (8 categories)
# Data from https://www.reddit.com/r/Catsubs/wiki/index/
# Plot inspired by Nicola Rennie @nrennie35 (https://twitter.com/nrennie35/status/1523987463634624513/photo/1)

# Libraries
library(tidyverse)
library(showtext)

# Font
font_add_google(name = "Ubuntu", family = "ubuntu")
font_add_google("IBM Plex Serif")
f1 = "IBM Plex Serif"

# Data
df = read_csv("data/catsubs.csv")

selected = c("General Cats","Cat Media","Cats Being Cats","Cats in Shapes","Cat Body Parts","Cats Making Noises","Cats with Things","Cat breeds and colorations") 

df1 = df %>% filter(category %in% selected) %>%
  mutate(category=fct_infreq(category)) %>%
  arrange(category, desc(sub)) %>%
  group_by(category) %>%
  mutate(c = row_number()) 
  
dim(df1)

# Plot
df1 %>%
  ggplot() +
  geom_text(aes(x=category, y=c, label=sub), size=2, family="ubuntu", color="white") +
  geom_text(data = df1 %>% group_by(category) %>% filter(c==max(c)),
            aes(x=category, y=c+2.5, label=c), 
            family=f1, color="white", fontface="bold") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 17)) +
  scale_y_continuous(expand=c(.03,.03)) +
  theme_minimal() +
  theme(text=element_text(family=f1, color="white"),
        panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="white", family=f1, face="bold", size=8),
        plot.background = element_rect(fill="black", color=NA),
        plot.margin = unit(c(-.75, .7, .75, .5), "cm"),
        plot.title=element_text(hjust = 1, vjust = -8, size=22),
        plot.subtitle = element_text(hjust = 1, vjust = -30, 
                                     size = 8, lineheight = 0.6,
                                     colour = "white", family = "ubuntu")
        ) +
  labs(title="Subreddits of cats",
       subtitle = "310 subreddits of cats from 8 selected categories listed on r/Catsubs, as of Jan 06, 2022.\n\nSubreddits in each category are arranged in alphabetical order.\n\nSource: r/Catsubs | Plot inspired by Nicola Rennie @nrennie35")
       
ggsave("p1.png", height=6, width=9)


