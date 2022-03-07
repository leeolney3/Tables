library(tidyverse)
library(ggtext)
library(showtext)
font_add_google("Lato")
font_add_google("Roboto Condensed")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

#ubs
df1 = tribble(
  ~fac, ~"2020",~"2026E",
  "Wide-area IoT",2.2,6.5,
  "Cellular  IoT",2.2,7.7,
  "Short-range IoT",11.4,20.3,
  "Total IoT",12.6,27.8,
) %>%
  mutate(fac = fct_rev(fct_inorder(fac))) %>%
  pivot_longer(2:3)
  
df1 %>%
  ggplot(aes(x=value, y=fac)) +
  geom_line(aes(group=fac), color="grey85", size=3) +
  geom_point(aes(color=name), size=9.8, fill="white") +
  geom_text(aes(label=scales::number(value, accuracy=.1)), size=3.2, 
            color="white", fontface="bold") +
  scale_color_manual(values=c("#B9252A","#3B4992")) +
  cowplot::theme_minimal_hgrid(12.2,line_size = 0.4) +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text.y=element_markdown(lineheight = 1.1, face="bold"),
        plot.title.position = "plot",
        plot.title=element_markdown(size=13, margin=margin(b=10)),
        plot.subtitle = element_markdown(margin=margin(t=7, b=7), size=12, lineheight = 1.2),
        text=element_text(family="Lato"),
        axis.line.x.bottom = element_blank(),
        plot.margin=margin(.5,.7,.5,.5, unit="cm"),
        plot.caption.position = "plot",
        plot.caption = element_markdown(hjust=0, size=9, lineheight = 1.2, color="grey20"),
        axis.text.x=element_blank()) +
  labs(title="Technological disruption is set to continue",
       subtitle="IoT connections, 2020 – 2026, in billions<br><br><span style='color:#B9252A'><b>2020</b></span> <span style='color:black'>and</span> <span style='color:#3B4992'><b>2026E</b></span>",
       caption="*Note: Chart includes randomly generated data"
       )
       
#bofa df
df_bofA = tribble(
~"Sector", ~"Policy",~"Policy-Women",~"Policy-POC",~"Policy-Veterans",~"Policy-LGTBQ",
"Communication services",88,69,65,69,69,
"Consumer Discretionary",98,57,49,43,51,
"Consumer Staples",88,55,55,45,58,
"Energy",100,70,43,43,35,
"Financials",82,63,63,55,63,
"Healthcare",98,66,55,55,58,
"Industrials",97,68,62,59,52,
"Information Technology",93,75,59,59,61,
"Materials",36,18,14,14,7,
"Real Estate",97,23,12,10,13,
"Utilities",100,68,68,64,71,
"Total",91,61,53,50,52
)

write_csv(df_bofA,"df_bofA.csv")

#bearing point 
df3 = tribble(
  ~cat, ~c1, ~c2, ~c3, ~c4,
  "Decision quality has improved",30,60,3,7,
  "We are quicker to react on external/internal changes",25,62,3,10,  
  "It is difficult to find the appropriate skills within our organization to implement data-driven techniques",20,48,7,25,
  "It is difficult to get senior management buy-in for the adoption of data-driven approaches",9,21, 20,50,
) 

df3a = df3 %>% 
  mutate(cat=(fct_inorder(cat))) %>%
  pivot_longer(c1:c4) %>%
  mutate(name=fct_relevel(name, "c1","c2","c3","c4")) %>%
  mutate(v1 = if_else(name %in% c("c1", "c2"), value, -value)) 
  
df3b = df3a %>% filter(cat=="It is difficult to get senior management buy-in for the adoption of data-driven approaches") %>%
  filter(name=="c2"|name=="c3") %>%
  mutate(lab1 = case_when(name=="c2"~"(strongly) agree", 
                          name=="c3"~"(strongly) disagree"))
                          

df3a %>%
  ggplot(aes(x=v1, y=cat, fill=name)) +
  geom_col(width=.5, show.legend = F) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  scale_fill_manual(values=c("#58794F","#65C346","#6863AA","#9991F6")) +
  geom_text(data = df3b,
            aes(label=lab1), size=3.5, vjust=-3.5) +
  geom_text(data=df3a %>% filter(abs(v1)>3),
            aes(label=scales::number(abs(v1), accuracy=1, suffix="%")), 
            position = position_stack(vjust = .5), size=3.5, color="white", family="Roboto Condensed") +
  cowplot::theme_minimal_grid(11) +
  theme(plot.margin=margin(.5,.5,.5,.5, unit="cm"),
        plot.title.position = "plot",
        axis.title=element_blank(),
        axis.text.x=element_blank(),
        plot.title=element_text(face="plain", size=10, margin=margin(b=8)),
        plot.subtitle=element_text(face="bold", size=13),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_blank()) +
  labs(title="Figure 11",
       subtitle="How would you rate the experience of becoming a data-driven company?")
