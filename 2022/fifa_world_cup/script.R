# Data: [The Fjelstul World Cup Database](The Fjelstul World Cup Database) via [@joshfjelstul](https://twitter.com/joshfjelstul/status/1546471071842443264) introductory Twitter thread
# Citation: Joshua C. Fjelstul (2022). worldcup: The Fjelstul World Cup Database. R package version 0.1.0.

# Load libraries
library(tidyverse)
library(tidygraph)
library(ggraph)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Import font
font_add_google("Outfit")
f1 = "Outfit"

# Import data
team_appearances = readr::read_csv("https://raw.githubusercontent.com/jfjelstul/worldcup/master/data-csv/team_appearances.csv")
team = readr::read_csv("https://raw.githubusercontent.com/jfjelstul/worldcup/master/data-csv/teams.csv")

# Count of tournaments, matches and teams
team_appearances %>% select(tournament_id, match_id,team_id, opponent_id) %>%
  pivot_longer(!c(tournament_id,match_id)) %>%
  summarise(n_tournaments=n_distinct(tournament_id),
            n_matches=n_distinct(match_id),
            n_teams =n_distinct(value))
            
# Prepare networks
# reference: https://bjnnowak.netlify.app/2021/09/30/r-network-analysis-with-tidygraph/
edges_list = team_appearances %>%
  select(team_id, opponent_id) %>%
  mutate(team_id= parse_number(team_id)*-1, opponent_id=parse_number(opponent_id)*-1) %>%
  count(team_id, opponent_id) %>%
  mutate(max=pmax(team_id, opponent_id),min=pmin(team_id, opponent_id)) %>% 
  unite(check, c(min, max), remove = FALSE) %>%
  distinct(check,.keep_all = TRUE) %>%
  select(from=team_id, to=opponent_id,n)

edges_list = edges_list %>% left_join(team1, by=c("from"="team_id")) %>%
  select(-from) %>% rename(from=team_code) %>%
  left_join(team1, by=c("to"="team_id")) %>%
  select(-to) %>% rename(to=team_code) %>%
  select(from, to, n)

network <- as_tbl_graph(edges_list, directed = FALSE)

# Add confederation to network
c1 = team %>% filter(confederation_id=="CF-1")
c2 = team %>% filter(confederation_id=="CF-2")
c3 = team %>% filter(confederation_id=="CF-3")
c4 = team %>% filter(confederation_id=="CF-4")
c5 = team %>% filter(confederation_id=="CF-5")
c6 = team %>% filter(confederation_id=="CF-6")

graph = network %>%
  activate(nodes) %>%
  mutate(confederation = case_when((name%in%c1$team_code)~"Asian Football Confederation",
                          (name%in%c2$team_code)~"Confederation of African Football",
                          (name%in%c3$team_code)~"Confederation of North, Central American\nand Caribbean Association Football",
                          (name%in%c4$team_code)~"South American Football Confederation",
                          (name%in%c5$team_code)~"Oceania Football Confederation",
                          (name%in%c6$team_code)~"Union of European Football Associations",
                          ))
                          
# Color palette
palette= c("#9D0819","#D86F47","#FFD2C6","#013335","#6BB3B1","#C1DFEF")

# Plot
ggraph(
  graph= graph, 
  layout = "stress") +
  geom_edge_diagonal(aes(edge_width=n),color = "#ffcb69", alpha = 0.7, show.legend = FALSE) +
  scale_edge_width(range=c(.2,1.5)) +
  geom_node_point(aes(color=confederation, fill=confederation), size=7, key_glyph = draw_key_dotplot) +
  scale_color_manual("Confederation",values=palette) +
  scale_fill_manual("Confederation",values=palette) +
  guides(color=guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_text(aes(x,y,label=name, color=confederation), size=2.5, family=f1) +
  scale_color_manual(guide="none", values=c("white","white","black","white","black","black")) +
  cowplot::theme_map(10) +
  theme(text=element_text(family=f1),
        legend.title=element_blank(),
        legend.position = "top",
        legend.justification = "center",
        legend.margin=margin(t=3),
        plot.margin=margin(.5,.5,.4,.5, unit="cm"),
        plot.title=element_text(hjust=.5, size=13),
        plot.subtitle=element_text(hjust=.5, size=10, color="grey10")
        ) +
  labs(caption="Source: Joshua C. Fjelstul (2022). worldcup: The Fjelstul World Cup Database. R package version 0.1.0.",
       title="FIFA World Cup Team Matchups",
       subtitle="900 matches played by 84 teams in 21 tournaments, from 1930 to 2018")

ggsave("fifa.png", height=8, width=8, bg="white")                                      


