# IPCC scenarios
# dataset available at: https://ourworldindata.org/explorers/ipcc-scenarios

library(tidyverse)
library(gt)
library(gtExtras)

raw = read_csv("data/ipcc-scenarios.csv")

data = raw %>% 
  filter(str_detect(Scenario,'Baseline')) %>%
  select(`Scenario`,`Year`,`Temperature`,`Economic consumption per capita`,`Final energy per capita`,`Primary energy per capita`,`Electricity capacity`) %>%
  pivot_longer(`Temperature`:`Electricity capacity`) 
  
summary(data$Year)

# function to draw plot
fun_plot <- function(data){
  trend <- ggplot(data=data, aes(Year,value, color=Scenario)) +
  geom_line(size=1.5, alpha=.9) +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(2005,2025,2050,2075,2100)) +
  ggthemes::scale_color_gdocs() +
  cowplot::theme_minimal_grid(20) +
  theme(legend.position = c(0.1, 0.8),
        legend.title = element_blank(),
        axis.title=element_blank())
  return(trend)
}

data_plot<-data %>%
  group_by(name)%>%
  nest()%>%
  mutate(
    gg = purrr::map(data,fun_plot) 
  )%>%
  select(name=name,gg)
  
# add description
tabdf = data %>% select(name) %>% distinct() %>%
  mutate(description=c("Global average temperature increase relative to the pre-industrial era (in °C).\nThe global average temperature increase relative to the pre-industrial era, which is taken to be the year 1750.",
  "Global private economic consumption per capita ($).\nEconomic consumption measures the expenditure of private households. It is gross domestic product minus government expenditures, investments, and exports. It is measured in 2005 international-dollars. This means it is adjusted for inflation and cross-country price differences.",
  "Global final energy consumption per person (kWh).\nFinal energy is the energy delivered to consumers for end consumption e.g. household electricity, or petrol sold at the pump. It is primary energy energy minus losses which occur in the transformation of fuels into usable energy products (giving us 'secondary energy') minus
transmission and distribution losses of getting electricity or heat to the end user.",
  "Global primary energy consumption per person (kWh).\nPrimary energy is the energy embodied in natural resources prior to transformation into useful end products. For example, the energy stored in coal or oil before combustion in a power plant. The transformation of fossil fuels into their secondary form (such as electricity) results in large inefficiencies due to heat loss. This means primary energy is higher than the energy used by a population.",
  "Global electricity capacity (GW).\nElectricity capacity is the maximum output an energy source if it runs continuously. To convert this to actual electricity output it has to be
multiplied by the time that this source is running, and its capacity factor.")) 

# table
table1 = tabdf %>% 
  mutate(trend=NA) %>%
  select(name, description, trend) %>%
  gt() %>%
  gt::text_transform(
    locations = cells_body(columns=trend),
    fn = function(x){
      purrr::map(
        data_plot$gg, gt::ggplot_image, 
        height = px(200), aspect_ratio = 1.8
  )}) %>%
  cols_label(name="Metric",
             description="Description",
             trend="IPCC Scenarios") %>%
  tab_source_note(source_note = "Source: Our World in Data & Riahi et al. (2017). The Shared Socioeconomic Pathways and their energy, land use, and greenhouse gas emissions implications: An overview, Global Environmental Change") %>%
  tab_footnote(footnote = md("SSP1: Sustainability – Taking the Green Road (Low challenges to mitigation and adaptation);<br>SSP2: Middle of the Road (Medium challenges to mitigation and adaptation);<br>SSP3: Regional Rivalry – A Rocky Road (High challenges to mitigation and adaptation);<br>SSP4 Inequality – A Road Divided (Low challenges to mitigation, high challenges to adaptation);<br>SSP5 Fossil-fueled Development – Taking the Highway (High challenges to mitigation, low challenges to adaptation)"), locations=cells_column_labels(columns=trend)) %>%
  tab_header(title=md("**IPCC Scenarios**"), 
             subtitle ="Assumptions of global shared socioeconomic pathways used in IPCC scenarios. Table of temperature, economic consumption per capita, final energy per capita, primary energy per capita, and electricity capacity metrics.") %>%
  cols_width(description~px(400)) %>%
  gt_theme_espn() %>%
  tab_options(heading.subtitle.font.size = px(18))
  
# save table
gtsave(table1, "IPCC_scenarios.png")