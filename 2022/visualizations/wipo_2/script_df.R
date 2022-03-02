library(tidyverse)

# dataframe for Datawrapper
library(tidyverse)
df = read_csv("bubble_fpd/bubble_fpd.csv")
write.csv(df21,"df21.csv")

df21b = df21 %>% select(`GDP per capita [PPP$ Logarithmic scale]`,SPLINE,`GII score`)
write.csv(df21b,"df21b.csv", row.names = F)


# dataframe for Flourish (2021)
df21f = df %>% filter(GIIYR==2021) %>%
  mutate(POP=round(POP*1000,2)) %>%
  rename(`GDP per capita [PPP$ Logarithmic scale]`=ln_GDP,
         `GII score`=SCORE, 
         `Population (million)`=POP, 
         `GII rank`=RANK,
         `UN region`=REG_UN,
         `Income category`=INCOME)
write.csv(df21f,"df21f.csv")

# dataframe for Flourish (2017 to 2021)
dff = df %>%
  mutate(POP=round(POP*1000,2)) %>%
  rename(`GDP per capita [PPP$ Logarithmic scale]`=ln_GDP,
         `GII score`=SCORE, 
         `Population (million)`=POP, 
         `GII rank`=RANK,
         `UN region`=REG_UN,
         `Income category`=INCOME,
         Year=GIIYR)
write.csv(dff,"dff.csv")




