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

install.packages("PearsonDS")
library("PearsonDS")

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

"1(A).
Create a scatter plot, showing relationship between 
average SPI in a district across each year from the prior election or first election up to the current election, and 
the number of political parties that are founded in a district. 
Consider the party to be "founded" if it has not competed in an election in this district prior to this election."

# nail down election year intervals in election dt

# calculate number of parties founded by election-interval

# join with average SPI by district for each election-year interval

# create a scatter plot of SPI by number of parties for a district

# create for loop to apply to all districts