# CASSE Steady State Herald
# Consumption Vs. Production Blog
# Taylor Lange
# Date

#packages
library(tidyverse)
library(readxl)
#load fonts for figure
extrafont::loadfonts(device = "win", quiet = TRUE)

#load data from Global Carbon Project
territorial <- read_excel("data_raw/National_Carbon_Emissions_2020v1.0.xlsx", 
                          sheet = "Territorial Emissions")
consumption <- read_excel("data_raw/National_Carbon_Emissions_2020v1.0.xlsx", 
                          sheet = "Consumption Emissions")

select(territorial,year,USA)%>%#select year and US data from territorial data
  rename(Territorial = USA)%>%#rename US column as territorial emissions
  right_join(select(consumption,year,USA))%>% #join with consumption data by year
  filter(USA != "NaN")%>%#remove years where consumption based estimates are not calculated
  mutate(Consumption = as.numeric(USA))%>%#duplicate and rename consumption estimates
  select(year,Territorial,Consumption) -> CO2#select just year, territorial column, and consumption column

#figure 1
ggplot(CO2,aes(x=year,y=Territorial))+#create plot
  scale_y_continuous(limits = c(1000,2000))+
  geom_line()+
  geom_point()+
  labs(x="Year",
       y="Estimate (MMT of Carbon)",
       title = "Territorial Carbon Emissions in the United States",
       subtitle = "1990 - 2018")+
  scale_linetype_manual(values = c("dashed","solid"))+
  theme_classic()+
  theme(text = element_text(family="Calibri"),
        plot.title = element_text(hjust = 0.5, family = "Calibri"),
        plot.subtitle = element_text(hjust = .5,color = "grey25",family = "Calibri"),
        panel.grid = element_blank(),
        axis.line = element_blank())

#figure 2
mutate(CO2, diff = (Consumption-Territorial)/Territorial*100)%>%#create % difference between territorial and consumption variable
mutate(greater = diff>0)%>%#create column indicating positive or negative difference for plot color purposes
  ggplot(aes(x=year,y=diff,fill=greater))+#create plot
  geom_bar(stat = "identity",color = "grey50")+
  scale_y_continuous(limits = c(-5,10))+
  labs(x = "Year",
       y = "Percent Difference",
       title = "Percent Difference Between\nTerritorial & Consumption Estimates",
       subtitle = "1990-2018")+
  theme_classic()+
  theme(text = element_text(family="Calibri"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = .5,color = "grey25"))

#figure 3

ggplot(CO2,aes(x=year,y=1))+#create plot
  scale_y_continuous(limits = c(1000,2000))+
  geom_line(aes(y=Consumption,linetype = "Consumption"))+
  geom_point(aes(y=Consumption),shape = 1)+
  geom_line(aes(y=Territorial, linetype = "Territorial"))+
  geom_point(aes(y=Territorial))+labs(x="Year",
                                      y="Estimate (MMT of Carbon)",
                                      title = "Carbon Emissions in the United States\nTerritorial vs Consumption",
                                      subtitle = "1990 - 2018",
                                      linetype = "Method of Accounting ")+
  scale_linetype_manual(values = c("dashed","solid"))+
  theme_classic()+
  theme(text = element_text(family="Calibri"),
        plot.title = element_text(hjust = 0.5, family = "Calibri"),
        plot.subtitle = element_text(hjust = .5,color = "grey25", family = "Calibri"),
        legend.position = "bottom")


