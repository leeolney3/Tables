# Performance comparision of hybrid models, across 5 ML algorithms

library(tidyverse)
library(gt)
library(gtExtras)

library(ggtext)
library(showtext)
font_add_google("Source Sans Pro")
font_add_google("Libre Franklin")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# data
df = tibble::tribble(
~Model,~Measure,~`Naïve Bayes`,~`Logistic regression`,~`Decision Tree`,~`Random Forest`,~`Generalized linear model`,
"Model 1 (net promoter method, scale of 0 to 10). ","AUC", 0.97, 0.99, 0.99, 0.99, 0.99,
"Model 1 (net promoter method, scale of 0 to 10). ","Precision", 0.78, 0.72, 0.73, 0.73, 0.93,
"Model 1 (net promoter method, scale of 0 to 10). ","Recall", 0.89, 0.99, 0.99, 0.99, 0.99,
"Model 1 (net promoter method, scale of 0 to 10). ","F Measure", 0.83, 0.84, 0.84, 0.84, 0.96,
"Model 2 (fuzzy logic approach, numeric range from 0 to 1)","AUC", 0.76, 0.82,  0.8, 0.86, 0.84,
"Model 2 (fuzzy logic approach, numeric range from 0 to 1)","Precision", 0.61, 0.65, 0.72, 0.87, 0.68,
"Model 2 (fuzzy logic approach, numeric range from 0 to 1)","Recall", 0.63, 0.52, 0.58, 0.38, 0.59,
"Model 2 (fuzzy logic approach, numeric range from 0 to 1)","F Measure", 0.61, 0.58, 0.63, 0.53, 0.62,
"Model 3 (engagement based star rating, Likert scale 1 to 5)","AUC", 0.84, 0.88,  0.8, 0.88, 0.88,
"Model 3 (engagement based star rating, Likert scale 1 to 5)","Precision", 0.69, 0.77, 0.71, 0.73, 0.72,
"Model 3 (engagement based star rating, Likert scale 1 to 5)","Recall", 0.73, 0.66, 0.74, 0.72, 0.77,
"Model 3 (engagement based star rating, Likert scale 1 to 5)","F Measure",  0.7, 0.71, 0.72, 0.72, 0.74,
"Model 4 (engagement based like/dislike, binary variable with values 1 and 0)","AUC", 0.94, 0.88, 0.86, 0.86, 0.98,
"Model 4 (engagement based like/dislike, binary variable with values 1 and 0)","Precision", 0.95, 0.75, 0.72, 0.83, 0.96,
"Model 4 (engagement based like/dislike, binary variable with values 1 and 0)","Recall", 0.62, 0.73, 0.75, 0.69, 0.78,
"Model 4 (engagement based like/dislike, binary variable with values 1 and 0)","F Measure", 0.75, 0.74, 0.73, 0.75, 0.86
  )

# table  
df %>% 
  gt(groupname_col = "Model") %>% 
  gt_theme_538() %>% 
  tab_options(table.font.size = px(16),
              column_labels.font.size = px(13)) %>%
  cols_width(3:7 ~px(95)) %>%
  cols_label(Measure="") %>%
  tab_header(title="Comparison of hybrid models across five machine learning algorithms.") %>%
  opt_table_font(font=list(google_font("Source Sans Pro"))) %>%
  tab_style(style = cell_text(color = "black",font = google_font("Source Sans Pro"),transform = "capitalize"),
            locations = cells_column_labels(everything())) %>%
  tab_style(style = list(cell_text(size=px(14), weight =  400)),
            locations = cells_body())  %>%
  tab_style(style = list(cell_text(weight=380, size=px(13.5))),
    locations = list(cells_column_labels())) %>%
  tab_style(style = list(cell_text(size=px(13.5), weight =  380)),
            locations = cells_body(Measure)) %>%
  tab_style(style = list(cell_text(weight =  400,transform = "capitalize", size=px(13.5))),
            locations = cells_row_groups()) %>%
  gt_color_box(columns=3, domain=range(df$`Naïve Bayes`), accuracy=0.01, width=60) %>%
  gt_color_box(columns=4, domain=range(df$`Logistic regression`), accuracy=0.01, width=60) %>%
  gt_color_box(columns=5, domain=range(df$`Decision Tree`), accuracy=0.01, width=60) %>%
  gt_color_box(columns=6, domain=range(df$`Random Forest`), accuracy=0.01, width=60) %>%
  gt_color_box(columns=7, domain=range(df$`Generalized linear model`), accuracy=0.01, width=60) %>%
  cols_align(columns=3:7, align="left")
  
# cross tab 
cm = tribble(
  ~has_company_logo, ~company_profile_specified,~value,~id,~grp,
  "0","0",0.12,1,"Not fraudulent",
  "1","0",0.04,3,"Not fraudulent",
  "0","1",0.06,2,"Not fraudulent",
  "1","1",0.78,4,"Not fraudulent",
  "0","0",0.66,1,"Fraudulent",
  "1","0",0.02,3,"Fraudulent",
  "0","1",0.01,2,"Fraudulent",
  "1","1",0.31,4,"Fraudulent"
) 

cm2 = cm %>%
  mutate(has_company_logo=case_when(has_company_logo==0~"No",
                                    has_company_logo==1~"Yes"),
         company_profile_specified=case_when(company_profile_specified==0~"No",
                                             company_profile_specified==1~"Yes"),
         has_company_logo=fct_inorder(has_company_logo),
         company_profile_specified=fct_inorder(company_profile_specified),
         grp=fct_inorder(grp)) 

cm2 %>%
  ggplot(aes(company_profile_specified,has_company_logo)) +
  geom_tile(aes(fill=value), color="white", size=3.5) +
  geom_text(aes(label=scales::percent(value, accuracy=1), color=I(ifelse(value<0.5,"black","white"))), 
            family="Source Sans Pro", size=5) +
  geom_text(data=cm2 %>% filter(value>.5), aes(label=c("Has company\nlogo and profile","No company logo\nor profile")), nudge_y = .2, color="white", lineheight=1, size=3.4, fontface="italic") +
  scale_color_identity() +
  ggsci::scale_fill_material("blue-grey") +
  #rcartocolor::scale_fill_carto_c(palette = "ag_GrnYl", limits=c(0,1), direction=-1) +
  coord_cartesian(expand=F) +
  facet_wrap(~grp) +
  cowplot::theme_minimal_grid(12) +
  theme(legend.position = "none",
        strip.text=element_text(size=12, face="bold", margin=margin(b=7), family="Libre Franklin"),
        axis.title.x=element_text(face="italic", margin=margin(t=7), size=10.5),
        axis.title.y=element_text(face="italic", size=10.5),
        plot.margin=margin(.5,.5,.5,.5,unit="cm"),
        panel.spacing = unit(2, "lines")) +
  labs(x="Company profile specified?", y="Has company logo?") 
  
# cross tab alt
p1 = cm %>% 
  filter(grp=="Not fraudulent") %>%
  mutate(xmin = 0, ymin=0,
         xmax=case_when(id==1|id==3 ~ -1*(sqrt(value)), TRUE~sqrt(value)),
         ymax=case_when(id==1|id==2 ~ -1*(sqrt(value)), TRUE~sqrt(value))
         ) %>%
  mutate(x=case_when(id==1|id==3~-0.005,TRUE~0.005),
         y=case_when(id==1|id==2~-0.01, TRUE~0.01)) %>%
  ggplot() + 
  geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax/4, ymax=ymax/4, fill=factor(id)), show.legend = F) +
  geom_text(aes(x=x,y=y, label=scales::percent(value, accuracy=1), hjust=ifelse(id==1|id==3,1,0)), 
            size=3.5, family="Source Sans Pro") +
  scale_fill_manual(values=c("#CCE9C0","#F7D8F3","#CC8AC0","#86B26F")) +
  coord_equal() +
  theme_void(base_family = "Source Sans Pro")

p2 = cm %>% 
  filter(grp=="Fraudulent") %>%
  mutate(xmin = 0, ymin=0,
         xmax=case_when(id==1|id==3 ~ -1*(sqrt(value)), TRUE~sqrt(value)),
         ymax=case_when(id==1|id==2 ~ -1*(sqrt(value)), TRUE~sqrt(value))
         ) %>%
  mutate(x=case_when(id==1|id==3~-0.005,TRUE~0.005),
         y=case_when(id==1|id==2~-0.01, TRUE~0.01)) %>%
  ggplot() + 
  geom_rect(aes(xmin=xmin, ymin=ymin, xmax=xmax/4, ymax=ymax/4, fill=factor(id)), show.legend = F) +
  geom_text(aes(x=x,y=y, label=scales::percent(value, accuracy=1), hjust=ifelse(id==1|id==3,1,0)), 
            size=3.5, family="Source Sans Pro") +
  scale_fill_manual(values=c("#CCE9C0","#F7D8F3","#CC8AC0","#86B26F")) +
  coord_equal() +
  theme_void(base_family = "Source Sans Pro")
  
library(patchwork)
p1|p2