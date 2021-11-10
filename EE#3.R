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
library(cowplot)

install.packages("cowplot")

install.packages("PearsonDS")
library("PearsonDS")

# home wd
# setwd("C:\\Users\\Jent\\Documents\\College\\Emory\\Social Network Analytics\\EE#3")
# school wd
setwd("C:\\Users\\jentl\\Documents\\Emory\\Fall 2021\\Social Network Analytics\\EE#3")

"Loading Data"

border <- fread("border_information.csv")
election <- fread("election_results.csv")
rain <- fread("monthly_rainfall.csv")

# checking column names
head(border) #edge list of districts, 794 unique districts, 413 focal districts
head(election) #election results
head(rain) #rain by district

# check column types
sapply(border,class)
sapply(election,class)
sapply(rain,class)

# check for na values
apply(sapply(border,is.na),2,sum) 
apply(sapply(election,is.na),2,sum) 
apply(sapply(rain,is.na),2,sum) 

# for reference, dplyer version:
border %>% is.na() %>% colSums()

"1(A)"
"Calculating SPI"

# creating average rain by district by year
rain[,avg_rain:=mean(rainfall),by=.(district,trunc(time))]

# calculating scale: yearly variance across districts/yearly mean across districts by year
rain[,scale:=var(rainfall)/mean(rainfall),by=trunc(time)]

# calculating shape: mean(x)^2/yearly variance by year
rain[,shape:=mean(rainfall)^2/var(rainfall),by=trunc(time)]

# calculating qpearsonIII quantile function
rain[,qpear3:=qpearsonIII(p=pnorm(rainfall, mean=avg_rain, sd=sd(rainfall)), shape=shape, scale=scale, location=0), by=.(district,trunc(time))]

# calculating SPI
rain[,SPI:=(qpear3-mean(qpear3))/sd(qpear3),by=.(district,trunc(time))]

# dropping rows with NA SPI values
rain <- rain[!is.na(SPI),]

# create election year flag

# year column for joining
rain[,year:=trunc(time)]

# joining tables by year, creating election year flag
rain[election, on='year',elect_year:=year]

# replace elect_year NA with 0
set(rain,which(is.na(rain[[6]])),6,0)

# creating elect_year_group

unique_years <- unique(rain$elect_year)

for (i in 1:(length(unique_years)-1)){
  rain[unique_years[i]<rain$year & rain$year <= unique_years[i+1],elect_group:=i]
}

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
rain[new_party, new_parties:=i.new_parties, on=c(year='year',district='district')]

"1(A).
Create a scatter plot, showing relationship between 
average SPI in a district across each year from the prior election or first election up to the current election, and 
the number of political parties that are founded in a district. 
Consider the party to be 'founded' if it has not competed in an election in this district prior to this election. X axis is avg SPI for a district in an election-interval, y axis is num parties founded"

"clarify: am I graphing per district or aggregate?"
# calculate average SPI by district by year
rain[,avg_SPI:=mean(SPI),by=.(district,year)]

# setting NA new parties to zero
set(rain,which(is.na(rain[[12]])),12,0)

# table for SPI 
rain_plot <- unique(rain[,.(avg_SPI,new_parties),by=.(district,year)])

# using ggplot, adding geom_smooth trend line
ggplot(rain_plot, aes(avg_SPI,new_parties))+geom_point()+xlab("Average SPI")+ylab("Number of New Parties")+ggtitle("Number of New Indian Political Parties vs. SPI by District each Year ")+theme_classic()

