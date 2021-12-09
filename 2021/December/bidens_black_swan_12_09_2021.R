# Biden's Black Swan
# Taylor Lange
# Started 12/09/2021

#load packages
library(tidyverse)
library(readxl)

#load oil production data from Energy Information Administration - retrieved December 2nd 2021
WCRFPUS2w <- read_excel("WCRFPUS2w.xls", 
                        sheet = "Data 1")
#load oil accident data from National Oceanic and Atmospheric Administration - retrieved December 2nd 2021
incidents <- read_csv("incidents.csv")

#calculate yearly total production
mutate(WCRFPUS2w, year = lubridate::year(Date))%>%#extract year for yearly total
  group_by(year)%>%#group by year
  summarize(total = sum(`Crude Oil`*7))->oil #calculate total as row-wise average weekly total times 7 days

#calculate yearly spills
filter(incidents, threat=="Oil")%>%#filter to only oil related accidents
  separate(open_date,c("month","day","year"),sep="/")%>%#separate character dates into day, month, year
  replace_na(list(max_ptl_release_gallons = 1))%>%#replace NAs of minor spills with a single gallon
  mutate(bbl = max_ptl_release_gallons/42)%>%#convert gallons to barrels
  mutate(size = "catastrophic")%>%#create size catagory
  mutate(size = ifelse(bbl<1000,"minor",#recatagorize minor spills
                       ifelse(bbl<100000,"moderate",#recatagorize moderate spills
                              ifelse(bbl<1000000,"substantial",size))))->accidents#recatagorize magor spills

#total oil production 2000-20
filter(oil,year<2021,year>2000)->oil#filter oil to between 2001 and 2020
total.production <- sum(oil$total)*1000/1000000000#translate total production to billions of barrels

group_by(accidents,year)%>%#group by year 
  count(size)%>%#count number of spills of each size
  mutate(year = as.numeric(year))%>%#convert year to numeric
  group_by(size)%>%#group by size to gain whole totals
  filter(!is.na(n),year<2021,year>2000)%>%#filter to just 2001-2020
  summarize(spills = sum(n))%>%#count total number  
  mutate(rate= spills/total.production)->rates #calculate the rate of spills of each type
  

spill.probs <- tibble(Bbbl = 1:200)%>% #create tibble of billions of barrels
  mutate(catastrophic = 1-dpois(0,0.0190*Bbbl),#probability of at least 1 catastrophic per billion barrels; lambda from catastrophic spills
         substantial = 1-dpois(0,0.203*Bbbl),#probability of at least 1 substantial per billion barrels; lambda from substantial spills
         moderate = 1-dpois(0,1.13*Bbbl), #probability of at least 1 moderate per billion barrels; lambda from moderate spills
         minor = 1-dpois(0,12.7*Bbbl))#probability of at least 1 minor per billion barrels; lambda from minor spills

gather(spill.probs, "Severity","Probability",2:5)%>% #gather probabilities for figure
  ggplot(aes(x=Bbbl,y=Probability,color=Severity))+#create line graph, colored by probs
  geom_line(size = 1)+
  scale_x_log10()+
  labs(x="Billions of Barrels of Oil Produced",
       y="Probability of a Spill")+
  cowplot::theme_cowplot()+
  theme(legend.position = "none")#remove legend

