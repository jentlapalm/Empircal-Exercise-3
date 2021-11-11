library(readxl)
library(igraph)
library(data.table)
library(randtests)
library(stringr)
library(Matrix)
library(expm)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library("PearsonDS")
library(plm)

install.packages('pglm')
library(pglm)

# home wd
setwd("C:\\Users\\Jent\\Documents\\College\\Emory\\Social Network Analytics\\EE#3")
# school wd
#setwd("C:\\Users\\jentl\\Documents\\Emory\\Fall 2021\\Social Network Analytics\\EE#3")

"Loading Data"

border <- fread("border_information.csv")
election <- fread("election_results.csv")
rain <- fread("monthly_rainfall.csv")

# checking column names
head(border) #edge list of districts, 794 unique districts, 413 focal districts
head(election) #election results
head(rain) #rain by district

"1(A)"
"Calculating SPI"

# cleaning data
rain$time <- as.integer(rain$time)
rain <- rain[,list(rain=sum(rainfall)), by=.(district,time)]

# average rain

# calculating scale: yearly variance across districts/yearly mean across districts by year
rain[,scale:=var(rain)/mean(rain),by=time]

# calculating shape: mean(x)^2/yearly variance by year
rain[,shape:=mean(rain)^2/var(rain),by=time]

# calculating qpearsonIII quantile function
rain[,qpear3:=qpearsonIII(p=pnorm(rain, mean=mean(rain), sd=sd(rain)), shape=shape, scale=scale, location=0)]

# calculating SPI
rain[,avg_pear3:=mean(qpear3),by=district]
rain[,sd_pear3:=sd(qpear3),by=district]
rain[,SPI:=(qpear3-avg_pear3)/sd_pear3,by=.(district,time)]

# create election year flag

# joining tables by year, creating election year flag
rain[election, on=c(time='year'),elect_year:=year]

# replace elect_year NA with 0
rain[is.na(elect_year),elect_year:=0]

# creating elect_year_group
unique_years <- unique(rain$elect_year)

for (i in 1:(length(unique_years)-1)){
  rain[unique_years[i]<time & time<= unique_years[i+1],elect_group:=i]
}

rain[is.na(elect_group),elect_group:=0]

# calculate number of parties founded each election-interval

# get list of parties in district in each election, all previous elections

# call setdiff on these lists to find, count the number of different parties

new_party <- data.table(district=character(), year=numeric(), new_parties=numeric())

# need a year zero b/c parties are 'founded' in the first election
elect_years <- c(0,unique(election$year))

for (i in 1:(length(elect_years-1))){
  # list of new parties 
  temp1 <- setdiff(election[year==elect_years[i+1],party_name,by=district],
                   election[year<=elect_years[i],party_name,by=district])
  # number of new parties by district
  temp1[,new_parties:=length(unique(party_name)),by=district]
  # creating new party table
  temp2 <- election[year==unique(election$year)[i]][temp1, on='district', new_parties:=new_parties]
  new_party <- rbind(new_party,temp2[,.(district,year,new_parties)])
}

# joining new_party to rain by district and year
rain[new_party, new_parties:=i.new_parties, on=c(time='year',district='district')]

# fixing na's
rain[is.na(new_parties),new_parties:=0]

"1(A).
Create a scatter plot, showing relationship between 
average SPI in a district across each year from the prior election or first election up to the current election, and 
the number of political parties that are founded in a district. 
Consider the party to be 'founded' if it has not competed in an election in this district prior to this election. X axis is avg SPI for a district in an election-interval, y axis is num parties founded"

# calculate average SPI by district by election interval
rain[,avg_SPI:=mean(SPI),by=.(district,elect_group)]

# table for SPI 
rain_plot <- unique(rain[,.(avg_SPI,new_parties),by=.(district,elect_group)])

# using ggplot, adding geom_smooth trend line
ggplot(rain_plot, aes(avg_SPI,new_parties))+geom_point()+xlab("Average SPI")+ylab("Number of New Parties")+ggtitle("Number of New Indian Political Parties vs. SPI by District each Year ")+theme_classic()

"1(B).
Using the election-period level rainfall measures created above, show that the SPI is
not independent from on election period to the next within a district, as well as from
neighboring districts from one election period to the next.

It is possible to show this relationship by regressing a district's current level of the rainfall
variable on
(1) its lagged value and
(2) the lagged value of its neighbor's rainfall variable.
For computing the neighbors' value, you can use an average of each of the surrounding district's values.

Include a control in the regression for the number of years in the election period, and
use a fixed effects specification to control for the time-invariant features of a district
as well as a control for each election period.
This can be accomplished using the plm package, using a model specified in the form of
plm(outcome variable∼predictor variables, data, effect='twoways', model='within', index='district'), where 'twoways' and 'within' provide both sets of fixed effects."

# setting up data table
rain_short <- rain[time<=1999,.(district,time,elect_group,avg_SPI)]
rain_short <- unique(rain_short)

# creating neighbor SPI table to join to rain_reg afterwards
neighbors_SPI <- data.table(district=character(), V1=numeric(), elect_group=numeric())

for (d in unique(rain$district)){
  # getting district neighbors
  neighbors <- border[focal_district==d,district]
  # calculating mean SPI for each election cycle
  temp <- rain[district %in% neighbors, mean(SPI), by=.(elect_group)]
  temp$district <- d
  neighbors_SPI <- rbind(neighbors_SPI,temp)
}

# joining neighbors SPI with rain short table
rain_short[neighbors_SPI,on=c('district','elect_group'),n_SPI:=i.V1]

# getting lag values
rain_short[,lag_SPI:=avg_SPI-shift(avg_SPI),by=district]
rain_short[,lag_n_SPI:=n_SPI-shift(n_SPI),by=district]

# next task: get election-period number of years
years <- data.table(elect_years,elect_years-shift(elect_years))
years <- years[2:.N,]
setnames(years,'V2','year_gap')
years <- years[-1,]
years$elect_group <- 1:13

# joining with rain_short table
rain_short[years,on='elect_group',year_gap:=i.year_gap]
rain_short[is.na(year_gap),year_gap:=0]

# cleaning up non-election entries
rain_short <- rain_short[elect_group!=0]

# running the regression
plm(avg_SPI~lag_SPI+lag_n_SPI+year_gap, data=rain_short, effect='twoways', model='within', index='district')

"
1(C).
moderate droughts occur if SPI falls below -1,
moderate floods to occur if it rises above 1. 

Create a measure that sums the number of years a district experiences either moderate droughts
or floods during the interval starting from the year following the previous election up
until the year of the current election. 

Perform the same test as in (B), using this new transformed measure. 

Since this is a count outcome that is reported as a discrete number of years, use a regression adopted for data of this form—this can be accomplished with the pglm package, using a model specified
in the form of pglm(outcome variable ∼predictor variables, data, effect =
'twoways', model = 'within', index = 'district', family = 'poisson'). 
What differences do you see between the estimates?
"

# creating drought-flood flag
rain[SPI>1 | SPI<1,dr_fl:=1]
rain[is.na(dr_fl),dr_fl:=0]

# counting num years drought-flood per election cycle
rain[,sum_dr_fl:=sum(dr_fl), by=.(district,elect_group)]

# joining sum_dr_fl with rain_short
rain_short[rain,on=c(time='time',district='district'),sum_dr_fl:=i.sum_dr_fl]

# running into lots of errors, replacing NA's with 0
rain_zero <- rain_short
for (i in seq_along(rain_zero)) set(rain_zero, i=which(is.na(rain_zero[[i]])), j=i, value=0)

# running regression
pglm <- pglm(avg_SPI~lag_SPI+lag_n_SPI+year_gap+sum_dr_fl, data=rain_zero, effect='twoways', model='within', index='district', family='poisson')

# interesting results!
pglm$estimate

"2(A). 
Run a regression predicting the number of new political parties that are formed as a function of the number of years a district experiences droughts or flooding
in the interval starting from the year following the previous election up until the year of the current
election. 
The number of new political parties that enter a district is a discrete count outcome, 
so we should use a regression format adopted for counts, as in (1C). 
Include a control in the regression for the number of years in the election period, 
and a control for the time-invariant features of a district, as in Question 1. 

Are certain kinds of political parties, based on the issues they cater to, more likely to be formed when a district experiences extreme weather?"