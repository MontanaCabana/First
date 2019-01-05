#add positive and neg feedback to PAGE!!

rm(list=ls()) 
par(mfrow=c(1,1))

#load packages
install.packages("ggplot2") 
install.packages("timeSeries") 
install.packages("lubridate") 
install.packages("openxlsx") 
install.packages("dplyr") 
install.packages("plyr") 
install.packages("data.table") 
install.packages("readr") 
install.packages("tidyr") 
install.packages("leaps") 
install.packages("ggpubr")

library(timeSeries) 
library(ggplot2) 
library(lubridate) 
library(openxlsx) 
library(dplyr) 
library(plyr) 
library(data.table) 
library(readr) 
library(tidyr)  
library(leaps)  
library(ggpubr)

#4 facebook data sources:  (1)PAGE   (2)POST   (3)VIDEO   (4)FOLLOWERS

#Explore first data source - PAGE files
#--------------------------------------

#load qtrly page files (source data has to be extracted qtrly and columns are not aligned in each file so consolidation needs to be done this way)
Q1_2016_page=read.csv("H:/Project Dissertation/Source data/2016 Q1 page M.csv", header=T)
Q2_2016_page=read.csv("H:/Project Dissertation/Source data/2016 Q2 page M.csv", header=T)
Q3_2016_page=read.csv("H:/Project Dissertation/Source data/2016 Q3 page M.csv", header=T)
Q4_2016_page=read.csv("H:/Project Dissertation/Source data/2016 Q4 page M.csv", header=T)
Q1_2017_page=read.csv("H:/Project Dissertation/Source data/2017 Q1 page M.csv", header=T)
Q2_2017_page=read.csv("H:/Project Dissertation/Source data/2017 Q2 page M.csv", header=T)
Q3_2017_page=read.csv("H:/Project Dissertation/Source data/2017 Q3 page M.csv", header=T)
Q4_2017_page=read.csv("H:/Project Dissertation/Source data/2017 Q4 page M.csv", header=T)
Q1_2018_page=read.csv("H:/Project Dissertation/Source data/2018 Q1 page M.csv", header=T)
Q2_2018_page=read.csv("H:/Project Dissertation/Source data/2018 Q2 page M.csv", header=T)

#==============================================================================================================================================
#For PAGE do separate analysis can be done on #like sources;  #people talking about this; #Logged in page views #negative feedback #page stories by type # page consumptions by type
#=================================================================================================================================================

#first look at the page data
str(Q1_2016_page);str(Q2_2016_page);str(Q3_2016_page);str(Q4_2016_page);str(Q1_2017_page);str(Q2_2017_page);str(Q3_2017_page);str(Q4_2017_page);str(Q1_2018_page);str(Q2_2018_page);
dim(Q1_2016_page);dim(Q2_2016_page);dim(Q3_2016_page);dim(Q4_2016_page);dim(Q1_2017_page);dim(Q2_2017_page);dim(Q3_2017_page);dim(Q4_2017_page);dim(Q1_2018_page);dim(Q2_2018_page);
names(Q1_2016_page);names(Q2_2016_page);names(Q3_2016_page);names(Q4_2016_page);names(Q1_2017_page);names(Q2_2017_page);names(Q3_2017_page);names(Q4_2017_page);names(Q1_2018_page);names(Q2_2018_page);
#CHECK NCOLS

class(Q1_2016_page$Lifetime.Total.Likes)
class(Q4_2016_page$Lifetime.Total.Likes)


#View(Q1_2016_page)
names(Q1_2016_page);str(Q1_2016_page);ncol(Q1_2016_page);nrow(Q1_2016_page);dim(Q1_2016_page)

class(Q1_2016_page$Date)
#---> fix individualy by quarter rather than after quarters consolidated in df 'page' , so can also analyse quarterly data if choose

dim(Q1_2016_page);dim(Q2_2016_page);dim(Q3_2016_page);dim(Q4_2016_page);dim(Q1_2017_page);dim(Q2_2017_page);dim(Q3_2017_page);dim(Q4_2017_page);dim(Q1_2018_page);dim(Q2_2018_page);

any(is.na(Q1_2018_page)); any(is.null(Q1_2018_page))
is.data.frame(Q1_2016_page) && nrow(Q1_2016_page)==0

#---> fix individualy by quarter rather than after quarters consolidated in df 'page' , so can also analyse quarterly data if choose to

#update na with zero
Q1_2016_page[is.na(Q1_2016_page)]<-0;Q2_2016_page[is.na(Q2_2016_page)]<-0;Q3_2016_page[is.na(Q3_2016_page)]<-0;Q4_2016_page[is.na(Q4_2016_page)]<-0
Q1_2017_page[is.na(Q1_2017_page)]<-0;Q2_2017_page[is.na(Q2_2017_page)]<-0;Q3_2017_page[is.na(Q3_2017_page)]<-0;Q4_2017_page[is.na(Q4_2017_page)]<-0
Q1_2018_page[is.na(Q1_2018_page)]<-0;Q2_2018_page[is.na(Q2_2018_page)]<-0

#convert date from factor to date format
class(Q1_2016_page$Date);head(Q1_2016_page$Date);Q1_2016_page$Date=as.Date(Q1_2016_page$Date, format = "%d/%m/%Y");class(Q1_2016_page$Date);Q1_2016_page$Date
class(Q2_2016_page$Date);head(Q2_2016_page$Date);Q2_2016_page$Date=as.Date(Q2_2016_page$Date, format = "%d/%m/%Y");class(Q2_2016_page$Date);Q2_2016_page$Date
class(Q3_2016_page$Date);head(Q3_2016_page$Date);Q3_2016_page$Date=as.Date(Q3_2016_page$Date, format = "%d/%m/%Y");class(Q3_2016_page$Date);Q3_2016_page$Date
class(Q4_2016_page$Date);head(Q4_2016_page$Date);Q4_2016_page$Date=as.Date(Q4_2016_page$Date, format = "%d/%m/%Y");class(Q4_2016_page$Date);Q4_2016_page$Date
class(Q1_2017_page$Date);head(Q1_2017_page$Date);Q1_2017_page$Date=as.Date(Q1_2017_page$Date, format = "%d/%m/%Y");class(Q1_2017_page$Date);Q1_2017_page$Date
class(Q2_2017_page$Date);head(Q2_2017_page$Date);Q2_2017_page$Date=as.Date(Q2_2017_page$Date, format = "%d/%m/%Y");class(Q2_2017_page$Date);Q2_2017_page$Date
class(Q3_2017_page$Date);head(Q3_2017_page$Date);Q3_2017_page$Date=as.Date(Q3_2017_page$Date, format = "%d/%m/%Y");class(Q3_2017_page$Date);Q3_2017_page$Date
class(Q4_2017_page$Date);head(Q4_2017_page$Date);Q4_2017_page$Date=as.Date(Q4_2017_page$Date, format = "%d/%m/%Y");class(Q4_2017_page$Date);Q4_2017_page$Date
class(Q1_2018_page$Date);head(Q1_2018_page$Date);Q1_2018_page$Date=as.Date(Q1_2018_page$Date, format = "%d/%m/%Y");class(Q1_2018_page$Date);Q1_2018_page$Date
class(Q2_2018_page$Date);head(Q2_2018_page$Date);Q2_2018_page$Date=as.Date(Q2_2018_page$Date, format = "%d/%m/%Y");class(Q2_2018_page$Date);Q2_2018_page$Date

class(Q1_2016_page$Date)
#View(names(Q1_2016_page))

#shape review
dim(Q2_2018_page);dim(Q1_2018_page);dim(Q4_2017_page);dim(Q3_2017_page);dim(Q2_2017_page);dim(Q1_2017_page);dim(Q4_2016_page);dim(Q3_2016_page);dim(Q2_2016_page);dim(Q1_2016_page);

#seclect columns - total col no. prohibitive so selection of relevant columns (based on simple analysis / sentiment - first analysis is on interested in Daily data, choose intersting metrics - mainly to analyse and calc. key social media metrics defined in word document)
Q2_2018_page=subset(Q2_2018_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q1_2018_page=subset(Q1_2018_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q4_2017_page=subset(Q4_2017_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q3_2017_page=subset(Q3_2017_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q2_2017_page=subset(Q2_2017_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q1_2017_page=subset(Q1_2017_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q4_2016_page=subset(Q4_2016_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q3_2016_page=subset(Q3_2016_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q2_2016_page=subset(Q2_2016_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
Q1_2016_page=subset(Q1_2016_page,select=c(Date,Lifetime.Total.Likes,Daily.New.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))

str(Q1_2016_page);str(Q2_2016_page);str(Q3_2016_page);str(Q4_2016_page);str(Q1_2017_page);str(Q2_2017_page);str(Q3_2017_page);str(Q4_2017_page);str(Q1_2018_page);str(Q2_2018_page);

#CHECK NCOLS
dim(Q1_2016_page);dim(Q2_2016_page);dim(Q3_2016_page);dim(Q4_2016_page);dim(Q1_2017_page);dim(Q2_2017_page);dim(Q3_2017_page);dim(Q4_2017_page);dim(Q1_2018_page);dim(Q2_2018_page);

names(Q4_2016_page)
#consolidate quarterly page files into 1
page<-rbind(Q1_2016_page,Q2_2016_page,Q3_2016_page,Q4_2016_page,Q1_2017_page,Q2_2017_page,Q3_2017_page,Q4_2017_page,Q1_2018_page,Q2_2018_page)

#check consolidated file
#-----------------------
#View(page)
dim(page); dim(Q1_2016_page);dim(Q2_2016_page);dim(Q3_2016_page);dim(Q4_2016_page);dim(Q1_2017_page);dim(Q2_2017_page);dim(Q3_2017_page);dim(Q4_2017_page);dim(Q1_2018_page);dim(Q2_2018_page);
names(page)
class(page)
any(is.na(page));any(is.null(page))

#add days of week
install.packages("lubridate")
library(lubridate)
install.packages("stringi")
library(stringi)
page$Date <- as.Date(page$Date)
page$Day=wday(page$Date, label=TRUE)
page$Day
names(page)

#add 'Daily.Net.Likes' column to data
#-------------------------------------
page$Daily.Net.Likes=page$Daily.New.Likes-page$Daily.Unlikes

#seclect columns page
page=subset(page,select=c(Date,Day,Lifetime.Total.Likes,Daily.New.Likes,Daily.Net.Likes,Daily.Auto.played.30.second.Views,Daily.Logged.in.Page.Views,Daily.Negative.Feedback,Daily.Organic.Impressions,Daily.Organic.impressions.of.your.posts,Daily.Organic.Reach,Daily.Organic.reach.of.Page.posts,Daily.Page.Consumptions,Daily.Page.Engaged.Users,Daily.Paid.30.second.Views,Daily.Paid.Impressions,Daily.Paid.impressions.of.your.posts,Daily.Paid.Reach,Daily.Paid.reach.of.Page.posts,Daily.Reach.of.Page.posts,Daily.Total.30.Second.Repeats,Daily.Total.30.second.Views,Daily.Total.Auto.Played.Views,Daily.Total.Check.ins,Daily.Total.check.ins.using.mobile.devices,Daily.Total.Clicked.30.Second.Views,Daily.Total.Clicked.Views,Daily.Total.Consumers,Daily.Total.Impressions,Daily.Total.Impressions.of.your.posts,Daily.Total.Organic.30.Second.Views,Daily.Total.Organic.Views,Daily.Total.phone.calls.click.count.per.Page,Daily.Total.Promoted.Views,Daily.Total.Reach,Daily.Total.Unique.30.second.Views,Daily.Total.Unique.Video.Views,Daily.Total.Video.Views,Daily.Total.website.click.count.per.Page,Daily.total..Total.action.count.per.Page,Daily.Unlikes,Daily.Video.Repeats,Daily.Viral.Impressions,Daily.Viral.impressions.of.your.posts,Daily.Viral.Reach,Daily.Viral.reach.of.Page.posts,Daily.Total.get.direction.click.count.per.Page))
names(page)
head(page)

#bit more investigation needed on page dist.  Clean up (maybe take log?), remove outliers (maybe - we 
#want to identify the big numbers for likes etc though but exclude for regression model).  See if can regress big likes
#as separate exercide on page file data only, more useful to do this on POSTS I think....)

#IGNORE FOR NOW: circle back and exclude dates up to 'likes' load (7 july I think) as clean up, mostly blank rows (will need to shorten other files also)
nrow(page);ncol(page);names(page);page[is.na(page)] <- 0
#normally distributed?

par(mfrow=c(3,3))
hist(page$Lifetime.Total.Likes,col = 'red', breaks = 50, xlim=range(page$Lifetime.Total.Likes), xlab= "Total Likes", 
     ylab="Count", main="Histogram Total Likes", ps=50, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, cex.sub=1.5)
hist(page$Daily.New.Likes,col = 'blue', breaks = 50, xlim=range(page$Daily.New.Likes), xlab= "New Likes", 
     ylab="Count", main="Histogram New Likes", ps=50, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, cex.sub=1.5)
hist(page$Daily.Unlikes,col = 'green', breaks = 50, xlim=range(page$Daily.Unlikes), xlab= "Unlikes", 
     ylab="Count", main="Histogram Unlikes", ps=50, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, cex.sub=1.5)
plot(page$Lifetime.Total.Likes,col = 'red')
plot(page$Daily.New.Likes,col = 'blue')
plot(page$Daily.Unlikes,col = 'green')
qqnorm(page$Lifetime.Total.Likes,col = 'red')
qqnorm(page$Daily.New.Likes,col = 'blue')
qqnorm(page$Daily.Unlikes,col = 'green')

#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)

ggdensity(page$Daily.New.Likes, 
          main = "Page",
          xlab = "New Likes")

ggqqplot(page$Daily.New.Likes)

ggdensity(page$Lifetime.Total.Likes, 
          main = "Page",
          xlab = "Lifetime Total Likes")

ggqqplot(page$Lifetime.Total.Likes)


Lifetime.Total.Likes=page$Lifetime.Total.Likes;summary(Lifetime.Total.Likes)
var(Lifetime.Total.Likes, na.rm = T)
sd(Lifetime.Total.Likes, na.rm = T)

Daily.New.Likes=page$Daily.New.Likes;summary(Daily.New.Likes)
var(Daily.New.Likes, na.rm = T)
sd(Daily.New.Likes, na.rm = T)

Daily.Unlikes=page$Daily.Unlikes;summary(Daily.Unlikes)
var(Daily.Unlikes, na.rm = T)
sd(Daily.Unlikes, na.rm = T)

#p value less than 0.5 NOT normally dist.
#The Prob < W value listed in the output is the p-value. If the chosen alpha level is 0.05 and the p-value is less than 0.05, then the null hypothesis that the data are normally distributed is rejected. If the p-value is greater than 0.05, then the null hypothesis is not rejected.
shapiro.test(page$Lifetime.Total.Likes)
shapiro.test(page$Daily.New.Likes)
shapiro.test(page$Daily.Unlikes)

names(Q2_2018_page)

#plot_prep
page_1=page$Lifetime.Total.Likes
page_2=page$Daily.New.Likes
page_3=page$Daily.Net.Likes
page_4=page$Daily.Auto.played.30.second.Views
page_5=page$Daily.Logged.in.Page.Views
page_6=page$Daily.Negative.Feedback
page_7=page$Daily.Organic.Impressions
page_8=page$Daily.Organic.impressions.of.your.posts
page_9=page$Daily.Organic.Reach
page_10=page$Daily.Organic.reach.of.Page.posts
page_11=page$Daily.Page.Consumptions
page_12=page$Daily.Page.Engaged.Users
page_13=page$Daily.Paid.30.second.Views
page_14=page$Daily.Paid.Impressions
page_15=page$Daily.Paid.impressions.of.your.posts
page_16=page$Daily.Paid.Reach
page_17=page$Daily.Paid.reach.of.Page.posts
page_18=page$Daily.Reach.of.Page.posts
page_19=page$Daily.Total.30.Second.Repeats
page_20=page$Daily.Total.30.second.Views
page_21=page$Daily.Total.Auto.Played.Views
page_22=page$Daily.Total.Check.ins
page_23=page$Daily.Total.check.ins.using.mobile.devices
page_24=page$Daily.Total.Clicked.30.Second.Views
page_25=page$Daily.Total.Clicked.Views
page_26=page$Daily.Total.Consumers
page_27=page$Daily.Total.Impressions
page_28=page$Daily.Total.Impressions.of.your.posts
page_29=page$Daily.Total.Organic.30.Second.Views
page_30=page$Daily.Total.Organic.Views
page_31=page$Daily.Total.phone.calls.click.count.per.Page
page_32=page$Daily.Total.Promoted.Views
page_33=page$Daily.Total.Reach
page_34=page$Daily.Total.Unique.30.second.Views
page_35=page$Daily.Total.Unique.Video.Views
page_36=page$Daily.Total.Video.Views
page_37=page$Daily.Total.website.click.count.per.Page
page_38=page$Daily.total..Total.action.count.per.Page
page_39=page$Daily.Unlikes
page_40=page$Daily.Video.Repeats
page_41=page$Daily.Viral.Impressions
page_42=page$Daily.Viral.impressions.of.your.posts
page_43=page$Daily.Viral.Reach
page_44=page$Daily.Viral.reach.of.Page.posts
page_45=page$Daily.Total.get.direction.click.count.per.Page

#dirty plots
plot(page_1)

#install.packages("timeSeries")
library(timeSeries)

par(mfrow=c(1,3))
#plot timerseries 
plot(as.timeSeries(page_1), at = "chic", minor.ticks="daily",col="blue",type="l",lwd=3,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Page data - 12.6.2016 - 30.6.2018")
legend("top",legend=c("Lifetime Total Likes"),fill=c("blue"),horiz=FALSE,cex=0.9,xpd=TRUE)

plot(as.timeSeries(page_2), at = "chic", minor.ticks="daily",col="green",type="l",lwd=3,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Page data - 12.6.2016 - 30.6.2018")
legend("top",legend=c("Daily New Likes"),fill=c("green"),horiz=FALSE,cex=0.9,xpd=TRUE)

plot(as.timeSeries(page_39), at = "chic", minor.ticks="daily",col="red",type="l",lwd=3,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Page data - 12.6.2016 - 30.6.2018")
legend("right",legend=c("Daily Unikes"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

plot(as.timeSeries(page_3), at = "chic", minor.ticks="daily",col="black",type="l",lwd=3,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Page data - 12.6.2016 - 30.6.2018")
legend("left",legend=c("Daily Net Likes"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

#install.packages("openxlsx")
library(openxlsx)

write.xlsx(page, 'page_first_look.xlsx')  #exports to H:\Documents

#drop first 164 rows as data blank
original_page=page
page=page[164:911,] # exclude zero rows
dim(page)
head(page$Date)
tail(page); head(page)
names(page)

#plot_prep re run with new range
page_1=page$Lifetime.Total.Likes
page_2=page$Daily.New.Likes
page_3=page$Daily.Net.Likes
page_4=page$Daily.Auto.played.30.second.Views
page_5=page$Daily.Logged.in.Page.Views
page_6=page$Daily.Negative.Feedback
page_7=page$Daily.Organic.Impressions
page_8=page$Daily.Organic.impressions.of.your.posts
page_9=page$Daily.Organic.Reach
page_10=page$Daily.Organic.reach.of.Page.posts
page_11=page$Daily.Page.Consumptions
page_12=page$Daily.Page.Engaged.Users
page_13=page$Daily.Paid.30.second.Views
page_14=page$Daily.Paid.Impressions
page_15=page$Daily.Paid.impressions.of.your.posts
page_16=page$Daily.Paid.Reach
page_17=page$Daily.Paid.reach.of.Page.posts
page_18=page$Daily.Reach.of.Page.posts
page_19=page$Daily.Total.30.Second.Repeats
page_20=page$Daily.Total.30.second.Views
page_21=page$Daily.Total.Auto.Played.Views
page_22=page$Daily.Total.Check.ins
page_23=page$Daily.Total.check.ins.using.mobile.devices
page_24=page$Daily.Total.Clicked.30.Second.Views
page_25=page$Daily.Total.Clicked.Views
page_26=page$Daily.Total.Consumers
page_27=page$Daily.Total.Impressions
page_28=page$Daily.Total.Impressions.of.your.posts
page_29=page$Daily.Total.Organic.30.Second.Views
page_30=page$Daily.Total.Organic.Views
page_31=page$Daily.Total.phone.calls.click.count.per.Page
page_32=page$Daily.Total.Promoted.Views
page_33=page$Daily.Total.Reach
page_34=page$Daily.Total.Unique.30.second.Views
page_35=page$Daily.Total.Unique.Video.Views
page_36=page$Daily.Total.Video.Views
page_37=page$Daily.Total.website.click.count.per.Page
page_38=page$Daily.total..Total.action.count.per.Page
page_39=page$Daily.Unlikes
page_40=page$Daily.Video.Repeats
page_41=page$Daily.Viral.Impressions
page_42=page$Daily.Viral.impressions.of.your.posts
page_43=page$Daily.Viral.Reach
page_44=page$Daily.Viral.reach.of.Page.posts
page_45=page$Daily.Total.get.direction.click.count.per.Page

#RERUN PLOTS NORMAL TEST ETC ABOVE WITH NEW RANGE
write.xlsx(page, 'page_second_look.xlsx')  #exports to H:\Documents

#-------------------------------------------------plot timeseries using ggplot------------------ 
theme_set(theme_minimal())
attach(page)
ts_page_1=ggplot(data = page, aes(x = Date, y = Lifetime.Total.Likes)) + 
  geom_line(color = "blue", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("PAGE LIFETIME TOTAL LIKES")

ts_page_1=ggplot(data = page, aes(x = Date, y = Daily.New.Likes)) + 
  geom_line(color = "green", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("PAGE DAILY NEW LIKES")

ts_page_1=ggplot(data = page, aes(x = Date, y = Daily.Unlikes)) + 
  geom_line(color = "red", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("PAGE DAILY NEW UNLIKES")

ts_page_1=ggplot(data = page, aes(x = Date, y = Daily.Net.Likes)) + 
  geom_line(color = "grey", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("PAGE DAILY NET LIKES")
detach(page)

#correlation loop for Page data, just to explore (look for multicolinearity later in final df for regression model----------------------------------------------------------------
names(page)
length(names(page))
page_loop_length=length(names(page))-4 
cor.coeffs.page<-vector("numeric",length=page_loop_length)
# run a loop and populate corr.coeffs with correlation coefficients
for(i in 1:page_loop_length){
  cor.coeffs.page[i]<-cor(page$Daily.New.Likes, page[, i+4])
};cor.coeffs.page[is.na(cor.coeffs.page)] <- 0

#test to see if loop in correct order
cor(page$Daily.New.Likes,page$Daily.Net.Likes) # ok

#add corr with +0.7 and visualise
Main_cor.coeffs.page=cor.coeffs.page[cor.coeffs.page>=0.6]

#add names
names(cor.coeffs.page)<-names(page)[5:ncol(page)]

#add new likes as baseline = 1 +-1 for graph
cor.coeffs.page=round(cor.coeffs.page,digits=2)
cor.coeffs.page=append(cor.coeffs.page, 1.00,0)
#cor.coeffs.page=append(cor.coeffs.page, -1.00,0)
names(cor.coeffs.page)[1]<-"Daily.New.Likes"
#names(cor.coeffs.page)[2]<-"Daily.New.Likes"
cor.coeffs.page

par(mfrow=c(1,1))
#plot corrs
ylim <- c(-0.2, 1.2)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(cor.coeffs.page, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim,
              main = "Correlation -  Page Metrics v 'Daily New Likes' 12 Jun 2016 to 30 Jun 2018",x.main=0.9, 
              ylab = "Correlation Strength & Direction",col=c("yellow",rep("dodgerblue",46)),border="black")
## Add text at top of bars
text(x = xx, y = cor.coeffs.page, label = cor.coeffs.page,pos = 3, cex = 0.6, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=names(cor.coeffs.page), tick=FALSE, las=2, line=-0.5, cex.axis=0.6)
abline(h = 0.7, col = "darkorange",lwd=2,lty=3);abline(h = -0.7, col = "darkorange",lwd=2,lty=3);abline(h = 0.0, col = "black",lwd=2,lty=1)

library(dplyr)
#page coeffs rank
cor.coeffs.page=cor.coeffs.page[2:44]
names(cor.coeffs.page)
cor.coeffs.page=abs(cor.coeffs.page)
ranktable_names<-names(cor.coeffs.page)
#cor.coeffs.page=lapply(cor.coeffs.page,abs);cor.coeffs.page=as.numeric(cor.coeffs.page)
ranktable_coeffs=cor.coeffs.page
ranktable <- data.frame(ranktable_names, ranktable_coeffs)
ranktable <- arrange(ranktable, desc(ranktable_coeffs)) %>%
  mutate(rank = 1:nrow(ranktable))
ranktable
write.xlsx(ranktable, 'page_ranktable.xlsx')  #exports to H:\Documents

ggdensity(page$Daily.New.Likes, 
          main = "New Likes",
          xlab = "Time")
ggqqplot(page$Daily.New.Likes)

ggdensity(page$Daily.Unlikes, 
          main = "New Likes",
          xlab = "Time")
ggqqplot(page$Daily.Unlikes)

#days
#box plot 
boxplotchart <- ggplot(page, aes(x=Day, y = Daily.New.Likes)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Daily.New.Likes") + 
  xlab("Day of the Week") 

boxplotchart <- ggplot(page, aes(x=Day, y = Daily.Unlikes)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Daily.Unlikes") + 
  xlab("Day of the Week") 

#bar plot total likes
par(mfrow=c(2,2))
Monday_1=subset(page,Day=="Mon", select=c(Daily.New.Likes))
Monday_1=sum(Monday_1$Daily.New.Likes)
Tuesday_1=subset(page,Day=="Tue", select=c(Daily.New.Likes))
Tuesday_1=sum(Tuesday_1$Daily.New.Likes)
Wednesday_1=subset(page,Day=="Wed", select=c(Daily.New.Likes))
Wednesday_1=sum(Wednesday_1$Daily.New.Likes)
Thursday_1=subset(page,Day=="Thu", select=c(Daily.New.Likes))
Thursday_1=sum(Thursday_1$Daily.New.Likes)
Friday_1=subset(page,Day=="Fri", select=c(Daily.New.Likes))
Friday_1=sum(Friday_1$Daily.New.Likes)
Saturday_1=subset(page,Day=="Sat", select=c(Daily.New.Likes))
Saturday_1=sum(Saturday_1$Daily.New.Likes)
Sunday_1=subset(page,Day=="Sun", select=c(Daily.New.Likes))
Sunday_1=sum(Sunday_1$Daily.New.Likes)

Monday_1; Tuesday_1; Wednesday_1; Thursday_1; Friday_1; Saturday_1; Sunday_1
Total.Likes.by.day <- c(Monday_1,Tuesday_1,Wednesday_1,Thursday_1,Friday_1,Saturday_1,Sunday_1)
Total.Likes.by.day=round(Total.Likes.by.day,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Total.Likes.by.day, main="Total Number of Daily New Likes", ylab=NA, ylim=c(0,20000), las=1, col=colors, names=names)

#bar plot total unlikes
Monday_3=subset(page,Day=="Mon", select=c(Daily.Unlikes))
Monday_3=sum(Monday_3$Daily.Unlikes)
Tuesday_3=subset(page,Day=="Tue", select=c(Daily.Unlikes))
Tuesday_3=sum(Tuesday_3$Daily.Unlikes)
Wednesday_3=subset(page,Day=="Wed", select=c(Daily.Unlikes))
Wednesday_3=sum(Wednesday_3$Daily.Unlikes)
Thursday_3=subset(page,Day=="Thu", select=c(Daily.Unlikes))
Thursday_3=sum(Thursday_3$Daily.Unlikes)
Friday_3=subset(page,Day=="Fri", select=c(Daily.Unlikes))
Friday_3=sum(Friday_3$Daily.Unlikes)
Saturday_3=subset(page,Day=="Sat", select=c(Daily.Unlikes))
Saturday_3=sum(Saturday_3$Daily.Unlikes)
Sunday_3=subset(page,Day=="Sun", select=c(Daily.Unlikes))
Sunday_3=sum(Sunday_3$Daily.Unlikes)

Monday_3; Tuesday_3; Wednesday_3; Thursday_3; Friday_3; Saturday_3; Sunday_3
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Total.Unlikes.by.day <- c(Monday_3, Tuesday_3, Wednesday_3, Thursday_3, Friday_3, Saturday_3, Sunday_3)
Total.Unlikes.by.day=round(Total.Unlikes.by.day,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Total.Unlikes.by.day, main="Total Number of Daily Unlikes", ylab=NA, ylim=c(0,3500), las=1, col=colors, names=names)

#bar plot average likes
Monday=subset(page,Day=="Mon", select=c(Daily.New.Likes))
Monday=mean(Monday$Daily.New.Likes)
Tuesday=subset(page,Day=="Tue", select=c(Daily.New.Likes))
Tuesday=mean(Tuesday$Daily.New.Likes)
Wednesday=subset(page,Day=="Wed", select=c(Daily.New.Likes))
Wednesday=mean(Wednesday$Daily.New.Likes)
Thursday=subset(page,Day=="Thu", select=c(Daily.New.Likes))
Thursday=mean(Thursday$Daily.New.Likes)
Friday=subset(page,Day=="Fri", select=c(Daily.New.Likes))
Friday=mean(Friday$Daily.New.Likes)
Saturday=subset(page,Day=="Sat", select=c(Daily.New.Likes))
Saturday=mean(Saturday$Daily.New.Likes)
Sunday=subset(page,Day=="Sun", select=c(Daily.New.Likes))
Sunday=mean(Sunday$Daily.New.Likes)

Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Average.Likes.by.day <- c(Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday)
Average.Likes.by.day=round(Average.Likes.by.day,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Average.Likes.by.day, main="Average Number of Daily New Likes", ylab=NA, ylim=c(0,200), las=1, col=colors, names=names)

#bar plot average unlikes
Monday_2=subset(page,Day=="Mon", select=c(Daily.Unlikes))
Monday_2=mean(Monday_2$Daily.Unlikes)
Tuesday_2=subset(page,Day=="Tue", select=c(Daily.Unlikes))
Tuesday_2=mean(Tuesday_2$Daily.Unlikes)
Wednesday_2=subset(page,Day=="Wed", select=c(Daily.Unlikes))
Wednesday_2=mean(Wednesday_2$Daily.Unlikes)
Thursday_2=subset(page,Day=="Thu", select=c(Daily.Unlikes))
Thursday_2=mean(Thursday_2$Daily.Unlikes)
Friday_2=subset(page,Day=="Fri", select=c(Daily.Unlikes))
Friday_2=mean(Friday_2$Daily.Unlikes)
Saturday_2=subset(page,Day=="Sat", select=c(Daily.Unlikes))
Saturday_2=mean(Saturday_2$Daily.Unlikes)
Sunday_2=subset(page,Day=="Sun", select=c(Daily.Unlikes))
Sunday_2=mean(Sunday_2$Daily.Unlikes)

Monday_2; Tuesday_2; Wednesday_2; Thursday_2; Friday_2; Saturday_2; Sunday_2
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Average.Unlikes.by.day <- c(Monday_2, Tuesday_2, Wednesday_2, Thursday_2, Friday_2, Saturday_2, Sunday_2)
Average.Unlikes.by.day=round(Average.Unlikes.by.day,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Average.Unlikes.by.day, main="Average Number of Daily Unlikes", ylab=NA, ylim=c(0,30), las=1, col=colors, names=names)

#------look at post data now------------------------------------------------------------------------------------------------
#load qtrly post files (source data has to be extracted qtrly and columns are not aligned in each file so consolidation needs to be done this way)
Q1_2016_post=read.csv("H:/Project Dissertation/Source data/2016 Q1 post M.csv", header=T)
Q2_2016_post=read.csv("H:/Project Dissertation/Source data/2016 Q2 post M.csv", header=T)
Q3_2016_post=read.csv("H:/Project Dissertation/Source data/2016 Q3 post M.csv", header=T)
Q4_2016_post=read.csv("H:/Project Dissertation/Source data/2016 Q4 post M.csv", header=T)
Q1_2017_post=read.csv("H:/Project Dissertation/Source data/2017 Q1 post M.csv", header=T)
Q2_2017_post=read.csv("H:/Project Dissertation/Source data/2017 Q2 post M.csv", header=T)
Q3_2017_post=read.csv("H:/Project Dissertation/Source data/2017 Q3 post M.csv", header=T)
Q4_2017_post=read.csv("H:/Project Dissertation/Source data/2017 Q4 post M.csv", header=T)
Q1_2018_post=read.csv("H:/Project Dissertation/Source data/2018 Q1 post M.csv", header=T)
Q2_2018_post=read.csv("H:/Project Dissertation/Source data/2018 Q2 post M.csv", header=T)

#first look at the page data
names(Q1_2016_post);str(Q1_2016_post);ncol(Q1_2016_post);nrow(Q1_2016_post)
dim(Q1_2016_post);dim(Q2_2016_post);dim(Q3_2016_post);dim(Q4_2016_post);dim(Q1_2017_post);dim(Q2_2017_post);dim(Q3_2017_post);dim(Q4_2017_post);dim(Q1_2018_post);dim(Q2_2018_post)
class(Q1_2016_post$Posted)

#---> fix individualy by quarter rather than after quarters consolidated in df 'page' , so can also analyse quarterly data if choose

#shape
str(Q1_2016_post);str(Q2_2016_post);str(Q3_2016_post);str(Q4_2016_post);str(Q1_2017_post);str(Q2_2017_post);str(Q3_2017_post);str(Q4_2017_post);str(Q1_2018_post);str(Q2_2018_post);
dim(Q1_2016_post);dim(Q2_2016_post);dim(Q3_2016_post);dim(Q4_2016_post);dim(Q1_2017_post);dim(Q2_2017_post);dim(Q3_2017_post);dim(Q4_2017_post);dim(Q1_2018_post);dim(Q2_2018_post);

any(is.na(Q1_2016_post))
any(is.null(Q1_2016_post))
is.data.frame(Q1_2016_post) && nrow(Q1_2016_post)==0

#remove na
Q1_2016_post[is.na(Q1_2016_post)]<-0;Q2_2016_post[is.na(Q2_2016_post)]<-0;Q3_2016_post[is.na(Q3_2016_post)]<-0;Q4_2016_post[is.na(Q4_2016_post)]<-0
Q1_2017_post[is.na(Q1_2017_post)]<-0;Q2_2017_post[is.na(Q2_2017_post)]<-0;Q3_2017_post[is.na(Q3_2017_post)]<-0;Q4_2017_post[is.na(Q4_2017_post)]<-0
Q1_2018_post[is.na(Q1_2018_post)]<-0;Q2_2018_post[is.na(Q2_2018_post)]<-0
dim(post)
#check date format
class(Q1_2016_post$Posted)

#convert date from factor to date format
class(Q1_2016_post$Posted)
head(Q1_2016_post$Posted)
Q1_2016_post$Posted=as.Date(Q1_2016_post$Posted, format = "%m/%d/%Y")
class(Q1_2016_post$Posted)
Q1_2016_post$Posted
class(Q2_2016_post$Posted)
head(Q2_2016_post$Posted)
Q2_2016_post$Posted=as.Date(Q2_2016_post$Posted, format = "%m/%d/%Y")
class(Q2_2016_post$Posted)
Q2_2016_post$Posted
class(Q3_2016_post$Posted)
head(Q3_2016_post$Posted)
Q3_2016_post$Posted=as.Date(Q3_2016_post$Posted, format = "%m/%d/%Y")
class(Q3_2016_post$Posted)
Q3_2016_post$Posted
class(Q4_2016_post$Posted)
head(Q4_2016_post$Posted)
Q4_2016_post$Posted=as.Date(Q4_2016_post$Posted, format = "%m/%d/%Y")
class(Q4_2016_post$Posted)
Q4_2016_post$Posted
class(Q1_2017_post$Posted)
head(Q1_2017_post$Posted)
Q1_2017_post$Posted=as.Date(Q1_2017_post$Posted, format = "%m/%d/%Y")
class(Q1_2017_post$Posted)
Q1_2017_post$Posted
class(Q2_2017_post$Posted)
head(Q2_2017_post$Posted)
Q2_2017_post$Posted=as.Date(Q2_2017_post$Posted, format = "%m/%d/%Y")
class(Q2_2017_post$Posted)
Q2_2017_post$Posted
class(Q3_2017_post$Posted)
head(Q3_2017_post$Posted)
Q3_2017_post$Posted=as.Date(Q3_2017_post$Posted, format = "%m/%d/%Y")
class(Q3_2017_post$Posted)
Q3_2017_post$Posted
class(Q4_2017_post$Posted)
head(Q4_2017_post$Posted)
Q4_2017_post$Posted=as.Date(Q4_2017_post$Posted, format = "%m/%d/%Y")
class(Q4_2017_post$Posted)
Q4_2017_post$Posted
class(Q1_2018_post$Posted)
head(Q1_2018_post$Posted)
Q1_2018_post$Posted=as.Date(Q1_2018_post$Posted, format = "%m/%d/%Y")
class(Q1_2018_post$Posted)
Q1_2018_post$Posted
class(Q2_2018_post$Posted)
head(Q2_2018_post$Posted)
Q2_2018_post$Posted=as.Date(Q2_2018_post$Posted, format = "%m/%d/%Y")
class(Q2_2018_post$Posted)
Q2_2018_post$Posted
names(post)
#add Engagement / Reach ratio (add by qtr for corr rather than 1 shot in consolidated file so qtr analysis is allowed)
Q1_2016_post$Performance=Q1_2016_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q1_2016_post$Lifetime.Post.Total.Reach
Q1_2016_post$Performance[is.na(Q1_2016_post$Performance)] <- 0
Q2_2016_post$Performance=Q2_2016_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q2_2016_post$Lifetime.Post.Total.Reach
Q2_2016_post$Performance[is.na(Q2_2016_post$Performance)] <- 0
Q3_2016_post$Performance=Q3_2016_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q3_2016_post$Lifetime.Post.Total.Reach
Q3_2016_post$Performance[is.na(Q3_2016_post$Performance)] <- 0
Q4_2016_post$Performance=Q4_2016_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q4_2016_post$Lifetime.Post.Total.Reach
Q4_2016_post$Performance[is.na(Q4_2016_post$Performance)] <- 0
Q1_2017_post$Performance=Q1_2017_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q1_2017_post$Lifetime.Post.Total.Reach
Q1_2017_post$Performance[is.na(Q1_2017_post$Performance)] <- 0
Q2_2017_post$Performance=Q2_2017_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q2_2017_post$Lifetime.Post.Total.Reach
Q2_2017_post$Performance[is.na(Q2_2017_post$Performance)] <- 0
Q3_2017_post$Performance=Q3_2017_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q3_2017_post$Lifetime.Post.Total.Reach
Q3_2017_post$Performance[is.na(Q3_2017_post$Performance)] <- 0
Q4_2017_post$Performance=Q4_2017_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q4_2017_post$Lifetime.Post.Total.Reach
Q4_2017_post$Performance[is.na(Q4_2017_post$Performance)] <- 0
Q1_2018_post$Performance=Q1_2018_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q1_2018_post$Lifetime.Post.Total.Reach
Q1_2018_post$Performance[is.na(Q1_2018_post$Performance)] <- 0
Q2_2018_post$Performance=Q2_2018_post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post/Q2_2018_post$Lifetime.Post.Total.Reach
Q2_2018_post$Performance[is.na(Q2_2018_post$Performance)] <- 0

#standardise Like source columns
Q1_2016_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q1_2016_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))
Q1_2016_post$Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks<- rep(c(0))
Q1_2016_post$Lifetime.Negative.Feedback...report_spam_clicks<- rep(c(0))

Q2_2016_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q2_2016_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))
Q2_2016_post$Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks<- rep(c(0))
Q2_2016_post$Lifetime.Negative.Feedback...report_spam_clicks<- rep(c(0))

Q3_2016_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q3_2016_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))

Q4_2016_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q4_2016_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))

Q1_2017_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q1_2017_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))

Q2_2017_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q2_2017_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))

Q3_2017_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q3_2017_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))

Q1_2018_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q1_2018_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))

names(Q2_2018_post)[52]=paste("Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks")
names(Q2_2018_post)[53]=paste("Lifetime.Negative.feedback.from.users.by.type...hide_clicks")
names(Q2_2018_post)[54]=paste("Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks")
Q2_2018_post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks<- rep(c(0))
Q2_2018_post$Lifetime.Negative.Feedback...unlike_page_clicks<- rep(c(0))

dim(Q1_2016_post);dim(Q2_2016_post);dim(Q3_2016_post);dim(Q4_2016_post);dim(Q1_2017_post);dim(Q2_2017_post);dim(Q3_2017_post);dim(Q4_2017_post);dim(Q1_2018_post);dim(Q2_2018_post);

names(Q1_2016_post)
names(Q2_2016_post)
names(Q3_2016_post)
names(Q4_2016_post)
names(Q1_2017_post)
names(Q2_2017_post)
names(Q3_2017_post)
names(Q4_2017_post)
names(Q1_2018_post)
names(Q2_2018_post)

#consolidate post files
post<-rbind(Q1_2016_post,Q2_2016_post,Q3_2016_post,Q4_2016_post,Q1_2017_post,Q2_2017_post,Q3_2017_post,Q4_2017_post,Q1_2018_post,Q2_2018_post)
names(post)
head(post)
dim(post)
tail(post)
str(post)
class(post)

#add days of week
library(lubridate)
post$Day <- as.Date(post$Posted)
post$Day=wday(post$Posted, label=TRUE)
post$Day
names(post)
head(post)

#reposition Day drop categorical
post=subset(post,select=c(Posted, Day,Lifetime.Post.Total.Reach,Lifetime.Post.Organic.Reach,Lifetime.Post.Paid.Reach,Lifetime.Post.Total.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Post.Paid.Impressions,Lifetime.Engaged.Users,Lifetime.Post.Consumers,Lifetime.Post.Consumptions,Lifetime.Negative.Feedback,Lifetime.Negative.feedback.from.users,Lifetime.Post.impressions.by.people.who.have.liked.your.Page,Lifetime.Post.reach.by.people.who.like.your.Page,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,Lifetime.Organic.views.to.95.,Lifetime.Organic.views.to.95..1,Lifetime.Paid.views.to.95.,Lifetime.Paid.views.to.95..1,Lifetime.Organic.Video.Views,Lifetime.Organic.Video.Views.1,Lifetime.Paid.Video.Views,Lifetime.Paid.Video.Views.1,Lifetime.Average.time.video.viewed,Lifetime.Video.length,Lifetime.Talking.about.this..post..by.action.type...share,Lifetime.Talking.about.this..post..by.action.type...like,Lifetime.Talking.about.this..post..by.action.type...comment,Lifetime.Post.stories.by.action.type...share,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.consumers.by.type...video.play,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumptions.by.type...video.play,Lifetime.Post.consumptions.by.type...other.clicks,Lifetime.Post.consumptions.by.type...photo.view,Lifetime.Post.consumptions.by.type...link.clicks,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks,Lifetime.Negative.feedback.from.users.by.type...hide_clicks,Lifetime.Negative.Feedback...hide_all_clicks,Lifetime.Negative.Feedback...hide_clicks,Performance,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks,Lifetime.Negative.Feedback...unlike_page_clicks,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks,Lifetime.Negative.Feedback...report_spam_clicks))

#see missing data
is.na(post$Lifetime.post.consumers.by.type...other.clicks)
is.na(post$Lifetime.post.consumers.by.type...link.clicks)
is.na(post$Lifetime.post.consumers.by.type...photo.view)
is.na(post$Lifetime.post.consumers.by.type...video.play)

post[is.na(post)]<-0

#prepare plots
str(post$Performance)
class(post$Performance)
is.na(post$Performance)
is.na(post)<-sapply(post, is.infinite)

dim(post)
names(post)
post_1=post$Lifetime.Post.Total.Reach
post_2=post$Lifetime.Post.Paid.Reach
post_3=post$Lifetime.Post.Organic.Impressions
post_4=post$Lifetime.Engaged.Users
post_5=post$Lifetime.Post.Consumptions
post_6=post$Lifetime.Negative.feedback.from.users
post_7=post$Lifetime.Post.reach.by.people.who.like.your.Page
post_8=post$Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page
post_9=post$Lifetime.Organic.views.to.95.
post_10=post$Lifetime.Paid.views.to.95.
post_11=post$Lifetime.Organic.Video.Views
post_12=post$Lifetime.Paid.Video.Views
post_13=post$Lifetime.Average.time.video.viewed
post_14=post$Lifetime.Talking.about.this..post..by.action.type...share
post_15=post$Lifetime.Talking.about.this..post..by.action.type...comment
post_16=post$Lifetime.Post.stories.by.action.type...like
post_17=post$Lifetime.Post.consumers.by.type...video.play
post_18=post$Lifetime.Post.consumers.by.type...photo.view
post_19=post$Lifetime.Post.consumptions.by.type...video.play
post_20=post$Lifetime.Post.consumptions.by.type...photo.view
post_21=post$Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks
post_22=post$Lifetime.Negative.Feedback...hide_all_clicks
post_23=post$Performance
post_24=post$Lifetime.Negative.Feedback...unlike_page_clicks
post_25=post$Lifetime.Negative.Feedback...report_spam_clicks
post_26=post$Lifetime.Post.Organic.Reach
post_27=post$Lifetime.Post.Total.Impressions
post_28=post$Lifetime.Post.Paid.Impressions
post_29=post$Lifetime.Post.Consumers
post_30=post$Lifetime.Negative.Feedback
post_31=post$Lifetime.Post.impressions.by.people.who.have.liked.your.Page
post_32=post$Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page
post_33=post$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post
post_34=post$Lifetime.Organic.views.to.95..1
post_35=post$Lifetime.Paid.views.to.95..1
post_36=post$Lifetime.Organic.Video.Views.1
post_37=post$Lifetime.Paid.Video.Views.1
post_38=post$Lifetime.Video.length
post_39=post$Lifetime.Talking.about.this..post..by.action.type...like
post_40=post$Lifetime.Post.stories.by.action.type...share
post_41=post$Lifetime.Post.stories.by.action.type...comment
post_42=post$Lifetime.Post.consumers.by.type...other.clicks
post_43=post$Lifetime.Post.consumers.by.type...link.clicks
post_44=post$Lifetime.Post.consumptions.by.type...other.clicks
post_45=post$Lifetime.Post.consumptions.by.type...link.clicks
post_46=post$Lifetime.Negative.feedback.from.users.by.type...hide_clicks
post_47=post$Lifetime.Negative.Feedback...hide_clicks
post_48=post$Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks
post_49=post$Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks

library(openxlsx)
write.xlsx(post, 'post_look.xlsx')

dim(post)

#normally distributed?
par(mfrow=c(3,4))
hist(post$Lifetime.Post.Total.Reach,col = 'red', breaks = 100)
hist(post$Lifetime.Engaged.Users,col = 'blue', breaks = 100)
hist(post$Lifetime.Post.Consumers,col = 'green', breaks = 100)
hist(post$Lifetime.Negative.feedback.from.users,col = 'darkorange', breaks = 100)
plot(post$Lifetime.Post.Total.Reach,col = 'red')
plot(post$Lifetime.Engaged.Users,col = 'blue')
plot(post$Lifetime.Post.Consumers,col = 'green')
plot(post$Lifetime.Negative.feedback.from.users,col = 'darkorange');
qqnorm(post$Lifetime.Post.Total.Reach,col = 'red')
qqnorm(post$Lifetime.Engaged.Users,col = 'blue')
qqnorm(post$Lifetime.Post.Consumptions,col = 'green')
qqnorm(post$Lifetime.Negative.feedback.from.users,col = 'darkorange')

#install.packages("ggpubr")
library(ggpubr)

ggdensity(post$Lifetime.Post.Total.Reach, 
          main = "Reach",
          xlab = "Post")

ggqqplot(post$Lifetime.Post.Total.Reach)

ggdensity(post$Lifetime.Engaged.Users, 
          main = "Engaged Users",
          xlab = "Post")

ggqqplot(post$Lifetime.Engaged.Users)

ggdensity(post$Lifetime.Post.Consumptions, 
          main = "Consumptions",
          xlab = "Post")

ggqqplot(post$Lifetime.Post.Consumptions)

ggdensity(post$Lifetime.Negative.feedback.from.users, 
          main = "Negative Feedback",
          xlab = "Post")

ggqqplot(post$Lifetime.Negative.feedback.from.users)

#p value less than 0.5 NOT normally dist.
a=rnorm(1000, 2) #base
shapiro.test(a)
qqnorm(a)

summary(post$Lifetime.Post.Total.Reach)
var(post$Lifetime.Post.Total.Reach, na.rm = T)
sd(post$Lifetime.Post.Total.Reach, na.rm = T)
max(post$Lifetime.Post.Total.Reach, na.rm = T)

summary(post$Lifetime.Engaged.Users)
var(post$Lifetime.Engaged.Users, na.rm = T)
sd(post$Lifetime.Engaged.Users, na.rm = T)
max(post$Lifetime.Engaged.Users, na.rm = T)

summary(post$Lifetime.Post.Consumptions)
var(post$Lifetime.Post.Consumptions, na.rm = T)
sd(post$Lifetime.Post.Consumptions, na.rm = T)
max(post$Lifetime.Post.Consumptions, na.rm = T)

summary(post$Lifetime.Negative.feedback.from.users)
var(post$Lifetime.Negative.feedback.from.users, na.rm = T)
sd(post$Lifetime.Negative.feedback.from.users, na.rm = T)
max(post$Lifetime.Negative.feedback.from.users, na.rm = T)

shapiro.test(post$Lifetime.Post.Total.Reach)
shapiro.test(post$Lifetime.Engaged.Users)
shapiro.test(post$Lifetime.Post.Consumptions)
shapiro.test(post$Lifetime.Negative.feedback.from.users)

#install.packages("timeSeries")
library(timeSeries)

par(mfrow=c(2,2))
#plot timerseries 'post' likes over time (improve plot)
plot(as.timeSeries(post$Lifetime.Post.Total.Reach), at = "chic", minor.ticks="daily",col="red",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Post data - 1.1.2016 to 30.6.2018")
legend("left",legend=c("Post Total Reach"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

#plot timerseries 'post' likes over time (improve plot)
plot(as.timeSeries(post$Lifetime.Engaged.Users), at = "chic", minor.ticks="daily",col="blue",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Post data - 1.1.2016 to 30.6.2018")
legend("left",legend=c("Post Engaged Users"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

#plot timerseries 'post' likes over time (improve plot)
plot(as.timeSeries(post$Lifetime.Post.Consumptions), at = "chic", minor.ticks="daily",col="green",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Post data - 1.1.2016 to 30.6.2018")
legend("left",legend=c("Post Consumptions"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

#plot timerseries 'post' likes over time (improve plot)
plot(as.timeSeries(post$Lifetime.Negative.feedback.from.users), at = "chic", minor.ticks="daily",col="grey",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Post data - 1.1.2016 to 30.6.2018")
legend("left",legend=c("Post Negative Feedback"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

#plot timerseries 'post' likes over time (improve plot)
plot(as.timeSeries(post$Performance), at = "chic", minor.ticks="daily",col="black",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Post data - 1.1.2016 to 30.6.2018")
legend("left",legend=c("Post Performance"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

#ggplot for date series on x axis---------------------------------------------------------------------
theme_set(theme_minimal())

ts_page_1=ggplot(data = post, aes(x = Posted, y = post$Lifetime.Post.Total.Reach )) + 
  geom_line(color = "red", size = 1.0) +ylab("count")
ts_page_1+scale_x_date(date_labels = "%m/%Y") +ggtitle("POST TOTAL REACH")

ts_page_1=ggplot(data = post, aes(x = Posted, y = post$Lifetime.Engaged.Users )) + 
  geom_line(color = "blue", size = 1.0)+ylab("count")
ts_page_1+scale_x_date(date_labels = "%m/%Y") +ggtitle("TOTAL ENGAGED USERS")

ts_page_1=ggplot(data = post, aes(x = Posted, y = post$Lifetime.Post.Consumptions )) + 
  geom_line(color = "green", size = 1.0)+ylab("count")
ts_page_1+scale_x_date(date_labels = "%m/%Y") +ggtitle("TOTAL POST CONSUMPTIONS")

ts_page_1=ggplot(data = post, aes(x = Posted, y = post$Lifetime.Negative.feedback.from.users )) + 
  geom_line(color = "BLACK", size = 1.0)+ylab("count")
ts_page_1+scale_x_date(date_labels = "%m/%Y") +ggtitle("TOTAL NEGATIVE FEEDBACK")

#correlation loop post data-------------------------------------------------------
attach(post)
names(post)
post=data.frame(Posted,Day,Lifetime.Post.Total.Reach,Lifetime.Post.Organic.Reach,Lifetime.Post.Paid.Reach,Lifetime.Post.Total.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Post.Paid.Impressions,Lifetime.Engaged.Users,Lifetime.Post.Consumers,Lifetime.Post.Consumptions,Lifetime.Negative.Feedback,Lifetime.Negative.feedback.from.users,Lifetime.Post.impressions.by.people.who.have.liked.your.Page,Lifetime.Post.reach.by.people.who.like.your.Page,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,Lifetime.Organic.views.to.95.,Lifetime.Organic.views.to.95..1,Lifetime.Paid.views.to.95.,Lifetime.Paid.views.to.95..1,Lifetime.Organic.Video.Views,Lifetime.Organic.Video.Views.1,Lifetime.Paid.Video.Views,Lifetime.Paid.Video.Views.1,Lifetime.Average.time.video.viewed,Lifetime.Video.length,Lifetime.Talking.about.this..post..by.action.type...share,Lifetime.Talking.about.this..post..by.action.type...like,Lifetime.Talking.about.this..post..by.action.type...comment,Lifetime.Post.stories.by.action.type...share,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.consumers.by.type...video.play,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumptions.by.type...video.play,Lifetime.Post.consumptions.by.type...other.clicks,Lifetime.Post.consumptions.by.type...photo.view,Lifetime.Post.consumptions.by.type...link.clicks,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks,Lifetime.Negative.feedback.from.users.by.type...hide_clicks,Lifetime.Negative.Feedback...hide_all_clicks,Lifetime.Negative.Feedback...hide_clicks,Performance,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks,Lifetime.Negative.Feedback...unlike_page_clicks,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks,Lifetime.Negative.Feedback...report_spam_clicks) 
head(post);detach(post)
names(post)
length(names(post))
post_loop_length=length(names(post))-3 
cor.coeffs.post<-vector("numeric",length=post_loop_length)
# run a loop and populate corr.coeffs with correlation coefficients
for(i in 1:post_loop_length){
  cor.coeffs.post[i]<-cor(post$Lifetime.Post.Total.Reach, post[, i+3])
};cor.coeffs.post[is.na(cor.coeffs.post)] <- 0

#tst
cor(post$Lifetime.Post.Total.Reach,post$Lifetime.Post.Organic.Reach)

#add corr with +0.7 and visualise
Main_cor.coeffs.post=cor.coeffs.post[cor.coeffs.post>=0.7]

names(cor.coeffs.post)<-names(post)[4:ncol(post)]

#add new likes as baseline = 1
cor.coeffs.post=round(cor.coeffs.post,digits=2)
cor.coeffs.post=append(cor.coeffs.post, 1.00,0)
#cor.coeffs.post=append(cor.coeffs.post, -1.00,0)
names(cor.coeffs.post)[1]<-"Lifetime.Post.Total.Reach"
#names(cor.coeffs.post)[2]<-"Lifetime.Post.Total.Reach"
length(cor.coeffs.post)

par(mfrow=c(1,1))
#plot corrs
ylim <- c(0.0, 1.2)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(cor.coeffs.post, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim,
              main = "Correlation - Post Metrics v 'Post Reach' 01 Jan 2016 to 30 Jun 2018",x.main=2.5, 
              ylab = "Correlation Strength & Direction",col=c("yellow",rep("dodgerblue",56)),border="black")
## Add text at top of bars
text(x = xx, y = cor.coeffs.post, label = cor.coeffs.post,pos = 3, cex = 0.9, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=names(cor.coeffs.post), tick=FALSE, las=2, line=-0.5, cex.axis=0.9)
abline(h = 0.7, col = "darkorange",lwd=2,lty=3);abline(h = -0.7, col = "darkorange",lwd=2,lty=3);abline(h = 0.0, col = "black",lwd=2,lty=1)

library(dplyr)
#post coeffs rank
cor.coeffs.post=cor.coeffs.post[2:49]
names(cor.coeffs.post)=names(cor.coeffs.post[1:48])
cor.coeffs.post=abs(cor.coeffs.post)
ranktable_names<-names(cor.coeffs.post)
#cor.coeffs.post=lapply(cor.coeffs.post,abs);cor.coeffs.post=as.numeric(cor.coeffs.post)
ranktable_coeffs=cor.coeffs.post
ranktable <- data.frame(ranktable_names, ranktable_coeffs)
ranktable <- arrange(ranktable, desc(ranktable_coeffs)) %>%
  mutate(rank = 1:nrow(ranktable))
ranktable
write.xlsx(ranktable, 'post_ranktable.xlsx')  #exports to H:\Documents

#box plot 
boxplotchart <- ggplot(post, aes(x=Day, y = Lifetime.Post.Total.Reach)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Lifetime.Post.Total.Reach") + 
  xlab("Day") 

boxplotchart <- ggplot(post, aes(x=Day, y = Lifetime.Post.Consumptions)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Lifetime.Post.Consumptions") + 
  xlab("Day") 

boxplotchart <- ggplot(post, aes(x=Day, y = Lifetime.Engaged.Users)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Lifetime.Engaged.Users") + 
  xlab("Day") 

boxplotchart <- ggplot(post, aes(x=Day, y = Lifetime.Negative.Feedback)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Post Negative Feedback") + 
  xlab("Day") 

par(mfrow=c(2,2))
#bar plot number of posts
POST_Monday=subset(post,Day=="Mon", select=c(Day))
POST_Monday=length(POST_Monday$Day);
POST_Tuesday=subset(post,Day=="Tue", select=c(Day))
POST_Tuesday=length(POST_Tuesday$Day);
POST_Wednesday=subset(post,Day=="Wed", select=c(Day))
POST_Wednesday=length(POST_Wednesday$Day)
POST_Thursday=subset(post,Day=="Thu", select=c(Day))
POST_Thursday=length(POST_Thursday$Day)
POST_Friday=subset(post,Day=="Fri", select=c(Day))
POST_Friday=length(POST_Friday$Day)
POST_Saturday=subset(post,Day=="Sat", select=c(Day))
POST_Saturday=length(POST_Saturday$Day)
POST_Sunday=subset(post,Day=="Sun", select=c(Day))
POST_Sunday=length(POST_Sunday$Day)

POST_Monday; POST_Tuesday; POST_Wednesday; POST_Thursday; POST_Friday; POST_Saturday; POST_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Total.Number.of.Posts <- c(POST_Monday, POST_Tuesday, POST_Wednesday, POST_Thursday, POST_Friday, POST_Saturday, POST_Sunday)
Total.Number.of.Posts=round(Total.Number.of.Posts,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp1 <- barplot (Total.Number.of.Posts, main="Total Number of Posts", ylab=NA, ylim=c(0,600), las=1, col=colors, names=names)

#bar plot total post reach
POST_Monday=subset(post,Day=="Mon", select=c(Lifetime.Post.Total.Reach))
POST_Monday=sum(POST_Monday$Lifetime.Post.Total.Reach);
POST_Tuesday=subset(post,Day=="Tue", select=c(Lifetime.Post.Total.Reach))
POST_Tuesday=sum(POST_Tuesday$Lifetime.Post.Total.Reach);
POST_Wednesday=subset(post,Day=="Wed", select=c(Lifetime.Post.Total.Reach))
POST_Wednesday=sum(POST_Wednesday$Lifetime.Post.Total.Reach)
POST_Thursday=subset(post,Day=="Thu", select=c(Lifetime.Post.Total.Reach))
POST_Thursday=sum(POST_Thursday$Lifetime.Post.Total.Reach)
POST_Friday=subset(post,Day=="Fri", select=c(Lifetime.Post.Total.Reach))
POST_Friday=sum(POST_Friday$Lifetime.Post.Total.Reach)
POST_Saturday=subset(post,Day=="Sat", select=c(Lifetime.Post.Total.Reach))
POST_Saturday=sum(POST_Saturday$Lifetime.Post.Total.Reach)
POST_Sunday=subset(post,Day=="Sun", select=c(Lifetime.Post.Total.Reach))
POST_Sunday=sum(POST_Sunday$Lifetime.Post.Total.Reach)

POST_Monday; POST_Tuesday; POST_Wednesday; POST_Thursday; POST_Friday; POST_Saturday; POST_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Total.Post.Reach <- c(POST_Monday, POST_Tuesday, POST_Wednesday, POST_Thursday, POST_Friday, POST_Saturday, POST_Sunday)
Total.Post.Reach=round(Total.Post.Reach,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp2 <- barplot (Total.Post.Reach, main="Total Post Reach", ylab=NA, ylim=c(0,14000000), las=1, col=colors, names=names)

##bar plot post engaged users
POST_3_Monday=subset(post,Day=="Mon", select=c(Lifetime.Post.Consumptions))
POST_3_Monday=sum(POST_3_Monday$Lifetime.Post.Consumptions)
POST_3_Tuesday=subset(post,Day=="Tue", select=c(Lifetime.Post.Consumptions))
POST_3_Tuesday=sum(POST_3_Tuesday$Lifetime.Post.Consumptions)
POST_3_Wednesday=subset(post,Day=="Wed", select=c(Lifetime.Post.Consumptions))
POST_3_Wednesday=sum(POST_3_Wednesday$Lifetime.Post.Consumptions)
POST_3_Thursday=subset(post,Day=="Thu", select=c(Lifetime.Post.Consumptions))
POST_3_Thursday=sum(POST_3_Thursday$Lifetime.Post.Consumptions)
POST_3_Friday=subset(post,Day=="Fri", select=c(Lifetime.Post.Consumptions))
POST_3_Friday=sum(POST_3_Friday$Lifetime.Post.Consumptions)
POST_3_Saturday=subset(post,Day=="Sat", select=c(Lifetime.Post.Consumptions))
POST_3_Saturday=sum(POST_3_Saturday$Lifetime.Post.Consumptions)
POST_3_Sunday=subset(post,Day=="Sun", select=c(Lifetime.Post.Consumptions))
POST_3_Sunday=sum(POST_3_Sunday$Lifetime.Post.Consumptions)

POST_3_Monday; POST_3_Tuesday; POST_3_Wednesday; POST_3_Thursday; POST_3_Friday; POST_3_Saturday; POST_3_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Total.Post.Consumptions <- c(POST_3_Monday, POST_3_Tuesday, POST_3_Wednesday, POST_3_Thursday, POST_3_Friday, POST_3_Saturday, POST_3_Sunday)
Total.Post.Consumptions=round(Total.Post.Consumptions,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp3 <- barplot (Total.Post.Consumptions, main="Total Post Consumptions", ylab=NA, ylim=c(0,1600000), las=1, col=colors, names=names)

##bar plot post engaged users
POST_2_Monday=subset(post,Day=="Mon", select=c(Lifetime.Engaged.Users))
POST_2_Monday=sum(POST_2_Monday$Lifetime.Engaged.Users)
POST_2_Tuesday=subset(post,Day=="Tue", select=c(Lifetime.Engaged.Users))
POST_2_Tuesday=sum(POST_2_Tuesday$Lifetime.Engaged.Users)
POST_2_Wednesday=subset(post,Day=="Wed", select=c(Lifetime.Engaged.Users))
POST_2_Wednesday=sum(POST_2_Wednesday$Lifetime.Engaged.Users)
POST_2_Thursday=subset(post,Day=="Thu", select=c(Lifetime.Engaged.Users))
POST_2_Thursday=sum(POST_2_Thursday$Lifetime.Engaged.Users)
POST_2_Friday=subset(post,Day=="Fri", select=c(Lifetime.Engaged.Users))
POST_2_Friday=sum(POST_2_Friday$Lifetime.Engaged.Users)
POST_2_Saturday=subset(post,Day=="Sat", select=c(Lifetime.Engaged.Users))
POST_2_Saturday=sum(POST_2_Saturday$Lifetime.Engaged.Users)
POST_2_Sunday=subset(post,Day=="Sun", select=c(Lifetime.Engaged.Users))
POST_2_Sunday=sum(POST_2_Sunday$Lifetime.Engaged.Users)

POST_2_Monday; POST_2_Tuesday; POST_2_Wednesday; POST_2_Thursday; POST_2_Friday; POST_2_Saturday; POST_2_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Total.Enaged.Users <- c(POST_2_Monday, POST_2_Tuesday, POST_2_Wednesday, POST_2_Thursday, POST_2_Friday, POST_2_Saturday, POST_2_Sunday)
Total.Enaged.Users=round(Total.Enaged.Users,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp4 <- barplot (Total.Enaged.Users, main="Total Enaged Users", ylab=NA, ylim=c(0,820000), las=1, col=colors, names=names)

##bar plot post neg feedback
POST_1_Monday=subset(post,Day=="Mon", select=c(Lifetime.Negative.feedback.from.users))
POST_1_Monday=sum(POST_1_Monday$Lifetime.Negative.feedback.from.users)
POST_1_Tuesday=subset(post,Day=="Tue", select=c(Lifetime.Negative.feedback.from.users))
POST_1_Tuesday=sum(POST_1_Tuesday$Lifetime.Negative.feedback.from.users)
POST_1_Wednesday=subset(post,Day=="Wed", select=c(Lifetime.Negative.feedback.from.users))
POST_1_Wednesday=sum(POST_1_Wednesday$Lifetime.Negative.feedback.from.users)
POST_1_Thursday=subset(post,Day=="Thu", select=c(Lifetime.Negative.feedback.from.users))
POST_1_Thursday=sum(POST_1_Thursday$Lifetime.Negative.feedback.from.users)
POST_1_Friday=subset(post,Day=="Fri", select=c(Lifetime.Negative.feedback.from.users))
POST_1_Friday=sum(POST_1_Friday$Lifetime.Negative.feedback.from.users)
POST_1_Saturday=subset(post,Day=="Sat", select=c(Lifetime.Negative.feedback.from.users))
POST_1_Saturday=sum(POST_1_Saturday$Lifetime.Negative.feedback.from.users)
POST_1_Sunday=subset(post,Day=="Sun", select=c(Lifetime.Negative.feedback.from.users))
POST_1_Sunday=sum(POST_1_Sunday$Lifetime.Negative.feedback.from.users)

POST_1_Monday; POST_1_Tuesday; POST_1_Wednesday; POST_1_Thursday; POST_1_Friday; POST_1_Saturday; POST_1_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Negative.Feedback <- c(POST_1_Monday, POST_1_Tuesday, POST_1_Wednesday, POST_1_Thursday, POST_1_Friday, POST_1_Saturday, POST_1_Sunday)
Negative.Feedback=round(Negative.Feedback,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp5 <- barplot (Negative.Feedback, main="Total Post Negative Feedback", ylab=NA, ylim=c(0,13000), las=1, col=colors, names=names)

#----------video data-------------------------------------------------------------------------------------------------------------------------------------
#load qtrly video files (source data has to be extracted qtrly and columns are not aligned in each file so consolidation needs to be done this way)
Q1_2016_video=read.csv("H:/Project Dissertation/Source data/2016 Q1 video M.csv", header=T)
Q2_2016_video=read.csv("H:/Project Dissertation/Source data/2016 Q2 video M.csv", header=T)
Q3_2016_video=read.csv("H:/Project Dissertation/Source data/2016 Q3 video M.csv", header=T)
Q4_2016_video=read.csv("H:/Project Dissertation/Source data/2016 Q4 video M.csv", header=T)
Q1_2017_video=read.csv("H:/Project Dissertation/Source data/2017 Q1 video M.csv", header=T)
Q2_2017_video=read.csv("H:/Project Dissertation/Source data/2017 Q2 video M.csv", header=T)
Q3_2017_video=read.csv("H:/Project Dissertation/Source data/2017 Q3 video M.csv", header=T)
Q4_2017_video=read.csv("H:/Project Dissertation/Source data/2017 Q4 video M.csv", header=T)
Q1_2018_video=read.csv("H:/Project Dissertation/Source data/2018 Q1 video M.csv", header=T)
Q2_2018_video=read.csv("H:/Project Dissertation/Source data/2018 Q2 video M.csv", header=T)

dim(Q1_2016_video);dim(Q2_2016_video);dim(Q3_2016_video);dim(Q4_2016_video);dim(Q1_2017_video);dim(Q2_2017_video);dim(Q3_2017_video);dim(Q4_2017_video);dim(Q1_2018_video);dim(Q2_2018_video);
str(Q1_2016_video);str(Q2_2016_video);str(Q3_2016_video);str(Q4_2016_video);str(Q1_2017_video);str(Q2_2017_video);str(Q3_2017_video);str(Q4_2017_video);str(Q1_2018_video);str(Q2_2018_video);

#convert date from factor to date format
class(Q1_2016_video$Posted)
head(Q1_2016_video$Posted)
Q1_2016_video$Posted=as.Date(Q1_2016_video$Posted, format = "%d/%m/%Y")
class(Q1_2016_video$Posted)
Q1_2016_video$Posted
class(Q2_2016_video$Posted)
head(Q2_2016_video$Posted)
Q2_2016_video$Posted=as.Date(Q2_2016_video$Posted, format = "%d/%m/%Y")
class(Q2_2016_video$Posted)
Q2_2016_video$Posted
class(Q3_2016_video$Posted)
head(Q3_2016_video$Posted)
Q3_2016_video$Posted=as.Date(Q3_2016_video$Posted, format = "%d/%m/%Y")
class(Q3_2016_video$Posted)
Q3_2016_video$Posted
class(Q4_2016_video$Posted)
head(Q4_2016_video$Posted)
Q4_2016_video$Posted=as.Date(Q4_2016_video$Posted, format = "%d/%m/%Y")
class(Q4_2016_video$Posted)
Q4_2016_video$Posted
class(Q1_2017_video$Posted)
head(Q1_2017_video$Posted)
Q1_2017_video$Posted=as.Date(Q1_2017_video$Posted, format = "%d/%m/%Y")
class(Q1_2017_video$Posted)
Q1_2017_video$Posted
class(Q2_2017_video$Posted)
head(Q2_2017_video$Posted)
Q2_2017_video$Posted=as.Date(Q2_2017_video$Posted, format = "%d/%m/%Y")
class(Q2_2017_video$Posted)
Q2_2017_video$Posted
class(Q3_2017_video$Posted)
head(Q3_2017_video$Posted)
Q3_2017_video$Posted=as.Date(Q3_2017_video$Posted, format = "%d/%m/%Y")
class(Q3_2017_video$Posted)
Q3_2017_video$Posted
class(Q4_2017_video$Posted)
head(Q4_2017_video$Posted)
Q4_2017_video$Posted=as.Date(Q4_2017_video$Posted, format = "%d/%m/%Y")
class(Q4_2017_video$Posted)
Q4_2017_video$Posted
class(Q1_2018_video$Posted)
head(Q1_2018_video$Posted)
Q1_2018_video$Posted=as.Date(Q1_2018_video$Posted, format = "%d/%m/%Y")
class(Q1_2018_video$Posted)
Q1_2018_video$Posted
class(Q2_2018_video$Posted)
head(Q2_2018_video$Posted)
Q2_2018_video$Posted=as.Date(Q2_2018_video$Posted, format = "%d/%m/%Y")
class(Q2_2018_video$Posted)
Q2_2018_video$Posted

Q1_2016_video=subset(Q1_2016_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q2_2016_video=subset(Q2_2016_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q3_2016_video=subset(Q3_2016_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q4_2016_video=subset(Q4_2016_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q1_2017_video=subset(Q1_2017_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q2_2017_video=subset(Q2_2017_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q3_2017_video=subset(Q3_2017_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q4_2017_video=subset(Q4_2017_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q1_2018_video=subset(Q1_2018_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
Q2_2018_video=subset(Q2_2018_video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))

#consolidate video files
video<-rbind(Q1_2016_video,Q2_2016_video,Q3_2016_video,Q4_2016_video,Q1_2017_video,Q2_2017_video,Q3_2017_video,Q4_2017_video,Q1_2018_video,Q2_2018_video)
video=subset(video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
ncol(video)

dim(video)

#add days of week
video_original=video

#install.packages("lubridate")
library(lubridate)
video$Day <- as.Date(video$Posted)
video$Day=wday(video$Posted, label=TRUE)
video$Day
names(video)
head(video)

#reposition Day
video=subset(video,select=c(Post.ID,Permalink,Post.Message,Type,Countries,Languages,Audience.targeting,Content.tags,Custom.labels,Is.broadcast,Universal.Video.ID,Posted,Day,Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))

library(openxlsx)
write.xlsx(video, 'video_look.xlsx')  #exports to H:\Documents

#replace NA with zero
video[is.na(video)]<-0

#update name to avoid dup. with Post
names(video)[14]<-"Video.Lifetime.Post.Total.Reach"

head(video)
nrow(video)
tail(video)
str(video)
class(video)
names(video)
is.na(video)

#video count per day
video_X=read.csv("H:/Project Dissertation/Source data/video_count2.csv", header=T)
names(video_X)

#build video_X
video_X$No_Views=as.numeric(video_X$No_Views)
class(video_X$No_Videos)
class(video_X$No_Views)
#add days of week
library(lubridate)
video_X$Day <- as.Date(video_X$Date)
video_X$Day=wday(video_X$Date, label=TRUE)
video_X$Day

names(video_X)
head(video_X)
video_X=subset(video_X,select=c(Quarter,Date,Day,No_Videos,No_Views))

class(video_X$Date)

video_TV=video_X$No_Views;video_TC=video_X$No_Videos

#install.packages("timeSeries")
library(timeSeries)
#plot timerseries video views

par(mfrow=c(1,2))

#plot timerseries video views 
plot(as.timeSeries(video_TC), at = "chic", minor.ticks="daily",col="blue",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Number of Videos Posted")

plot(as.timeSeries(video_TV), at = "chic", minor.ticks="daily",col="red",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Number of Views")

#ggp---------------------------------
#install.packages("ggplot2")
library(ggplot2)
theme_set(theme_minimal())
names(video)

Video_Total_Views=video_TV
Video_Total_Posts=video_TC
video_X$Date=as.Date(video_X$Date, format = "%d/%m/%Y")

ts_page_1=ggplot(data = video_X, aes(x = Date, y = Video_Total_Posts )) + 
  geom_line(color = "blue", size = 0.25)
ts_page_1+scale_x_date(date_labels = "%b/%Y") +ggtitle("Total Videos Posted")

ts_page_1=ggplot(data = video_X, aes(x = Date, y = Video_Total_Views )) + 
  geom_line(color = "red", size = 0.25)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("Total Video Views")
#ts_page_1 + stat_smooth(color = "blue",size = 0.25)

theme_set(theme_minimal())
dim(video)

names(video)
#normally distributed?
par(mfrow=c(3,2))
hist(video$Video.Lifetime.Post.Total.Reach,col = 'red', breaks = 100)
hist(video$Lifetime.Total.Video.Views,col = 'blue', breaks = 100)
plot(video$Video.Lifetime.Post.Total.Reach,col = 'red')
plot(video$Lifetime.Total.Video.Views,col = 'blue')
qqnorm(video$Video.Lifetime.Post.Total,col = 'red')
qqnorm(video$Lifetime.Total.Video.Views,col = 'blue')

summary(video$Video.Lifetime.Post.Total.Reach)
var(video$Video.Lifetime.Post.Total.Reach, na.rm = T)
sd(video$Video.Lifetime.Post.Total.Reach, na.rm = T)

summary(video$Lifetime.Total.Video.Views)
var(video$Lifetime.Total.Video.Views, na.rm = T)
sd(video$Lifetime.Total.Video.Views, na.rm = T)

shapiro.test(video$Video.Lifetime.Post.Total.Reach)
shapiro.test(video$Lifetime.Total.Video.Views)

#video cor----------------------------------------------
names(video)
length(names(video))
video_loop_length=length(names(video))-14 
cor.coeffs.video<-vector("numeric",length=video_loop_length)
# run a loop and populate corr.coeffs with correlation coefficients
for(i in 1:video_loop_length){
  cor.coeffs.video[i]<-cor(video$Video.Lifetime.Post.Total.Reach, video[, i+14])
};cor.coeffs.video[is.na(cor.coeffs.video)] <- 0

#tst
cor(video$Video.Lifetime.Post.Total.Reach,video$Lifetime.Post.Total.Impressions)

#add corr with +0.7 and visualise
Main_cor.coeffs.video=cor.coeffs.video[cor.coeffs.video>=0.7]
names(video)
names(cor.coeffs.video)<-names(video)[15:ncol(video)]

#add new likes as baseline = 1
cor.coeffs.video=round(cor.coeffs.video,digits=2)
cor.coeffs.video=append(cor.coeffs.video, 1.00,0)
#cor.coeffs.video=append(cor.coeffs.video, -1.00,0)
names(cor.coeffs.video)[1]<-"Video.Lifetime.Post.Total.Reach"
#names(cor.coeffs.video)[2]<-"Video.Lifetime.Post.Total.Reach"
cor.coeffs.video

par(mfrow=c(1,1))
#plot video corrs
ylim <- c(0.0, 1.2)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(cor.coeffs.video, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim,
              main = "Corr. Coeff. Video Metrics v 'Video Post Reach' 01 Jan 2016 to 30 Jun 2018",x.main=0.9, 
              ylab = "Correlation Strength & Direction",col=c("yellow",rep("dodgerblue",56)),border="black")
## Add text at top of bars
text(x = xx, y = cor.coeffs.video, label = cor.coeffs.video,pos = 3, cex = 0.6, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=names(cor.coeffs.video), tick=FALSE, las=2, line=-0.5, cex.axis=0.6)
abline(h = 0.7, col = "darkorange",lwd=2,lty=3);abline(h = -0.7, col = "darkorange",lwd=2,lty=3);abline(h = 0.0, col = "black",lwd=2,lty=1)

library(dplyr)
#video coeffs rank
cor.coeffs.video=cor.coeffs.video[2:36]
names(cor.coeffs.video)
cor.coeffs.video=abs(cor.coeffs.video)
ranktable_names<-names(cor.coeffs.video)
#cor.coeffs.video=lapply(cor.coeffs.video,abs);cor.coeffs.video=as.numeric(cor.coeffs.video)
ranktable_coeffs=cor.coeffs.video
ranktable <- data.frame(ranktable_names, ranktable_coeffs)
ranktable <- arrange(ranktable, desc(ranktable_coeffs)) %>%
  mutate(rank = 1:nrow(ranktable))
ranktable
write.xlsx(ranktable, 'video_ranktable.xlsx')  #exports to H:\Documents

boxplot(video$Video.Lifetime.Post.Total.Reach, vertical = T) # lot of outliers
boxplot(video$Lifetime.Total.Video.Views, vertical = T) # lot of outliers

par(mfrow=c(2,2))
names(video_X)
##bar plot video posts
VIDEO_2_Monday=subset(video_X,Day=="Mon", select=c(No_Videos))
VIDEO_2_Monday=sum(VIDEO_2_Monday$No_Videos)
VIDEO_2_Tuesday=subset(video_X,Day=="Tue", select=c(No_Videos))
VIDEO_2_Tuesday=sum(VIDEO_2_Tuesday$No_Videos)
VIDEO_2_Wednesday=subset(video_X,Day=="Wed", select=c(No_Videos))
VIDEO_2_Wednesday=sum(VIDEO_2_Wednesday$No_Videos)
VIDEO_2_Thursday=subset(video_X,Day=="Thu", select=c(No_Videos))
VIDEO_2_Thursday=sum(VIDEO_2_Thursday$No_Videos)
VIDEO_2_Friday=subset(video_X,Day=="Fri", select=c(No_Videos))
VIDEO_2_Friday=sum(VIDEO_2_Friday$No_Videos)
VIDEO_2_Saturday=subset(video_X,Day=="Sat", select=c(No_Videos))
VIDEO_2_Saturday=sum(VIDEO_2_Saturday$No_Videos)
VIDEO_2_Sunday=subset(video_X,Day=="Sun", select=c(No_Videos))
VIDEO_2_Sunday=sum(VIDEO_2_Sunday$No_Videos)

VIDEO_2_Monday; VIDEO_2_Tuesday; VIDEO_2_Wednesday; VIDEO_2_Thursday; VIDEO_2_Friday; VIDEO_2_Saturday; VIDEO_2_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Number.Video.Posts <- c(VIDEO_2_Monday, VIDEO_2_Tuesday, VIDEO_2_Wednesday, VIDEO_2_Thursday, VIDEO_2_Friday, VIDEO_2_Saturday, VIDEO_2_Sunday)
Number.Video.Posts=round(Number.Video.Posts,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Number.Video.Posts, main="Number of Video Posts", ylab=NA, ylim=c(0,80), las=1, col=colors, names=names)

##bar plot video views
VIDEO_1_Monday=subset(video,Day=="Mon", select=c(Lifetime.Total.Video.Views))
VIDEO_1_Monday=sum(VIDEO_1_Monday$Lifetime.Total.Video.Views)
VIDEO_1_Tuesday=subset(video,Day=="Tue", select=c(Lifetime.Total.Video.Views))
VIDEO_1_Tuesday=sum(VIDEO_1_Tuesday$Lifetime.Total.Video.Views)
VIDEO_1_Wednesday=subset(video,Day=="Wed", select=c(Lifetime.Total.Video.Views))
VIDEO_1_Wednesday=sum(VIDEO_1_Wednesday$Lifetime.Total.Video.Views)
VIDEO_1_Thursday=subset(video,Day=="Thu", select=c(Lifetime.Total.Video.Views))
VIDEO_1_Thursday=sum(VIDEO_1_Thursday$Lifetime.Total.Video.Views)
VIDEO_1_Friday=subset(video,Day=="Fri", select=c(Lifetime.Total.Video.Views))
VIDEO_1_Friday=sum(VIDEO_1_Friday$Lifetime.Total.Video.Views)
VIDEO_1_Saturday=subset(video,Day=="Sat", select=c(Lifetime.Total.Video.Views))
VIDEO_1_Saturday=sum(VIDEO_1_Saturday$Lifetime.Total.Video.Views)
VIDEO_1_Sunday=subset(video,Day=="Sun", select=c(Lifetime.Total.Video.Views))
VIDEO_1_Sunday=sum(VIDEO_1_Sunday$Lifetime.Total.Video.Views)

VIDEO_1_Monday; VIDEO_1_Tuesday; VIDEO_1_Wednesday; VIDEO_1_Thursday; VIDEO_1_Friday; VIDEO_1_Saturday; VIDEO_1_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Total.Total.Video.Views <- c(VIDEO_1_Monday, VIDEO_1_Tuesday, VIDEO_1_Wednesday, VIDEO_1_Thursday, VIDEO_1_Friday, VIDEO_1_Saturday, VIDEO_1_Sunday)
Total.Total.Video.Views=round(Total.Total.Video.Views,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Total.Total.Video.Views, main="Total Video Views", ylab=NA, ylim=c(0,2000000), las=1, col=colors, names=names)

##bar plot video views per post
VIDEO_3_Monday=VIDEO_1_Monday/VIDEO_2_Monday
VIDEO_3_Tuesday=VIDEO_1_Tuesday/VIDEO_2_Tuesday
VIDEO_3_Wednesday=VIDEO_1_Wednesday/VIDEO_2_Wednesday
VIDEO_3_Thursday=VIDEO_1_Thursday/VIDEO_2_Thursday
VIDEO_3_Friday=VIDEO_1_Friday/VIDEO_2_Friday
VIDEO_3_Saturday=VIDEO_1_Saturday/VIDEO_2_Saturday
VIDEO_3_Sunday=VIDEO_1_Sunday/VIDEO_2_Sunday

VIDEO_3_Monday; VIDEO_3_Tuesday; VIDEO_3_Wednesday; VIDEO_3_Thursday; VIDEO_3_Friday; VIDEO_3_Saturday; VIDEO_3_Sunday

#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Views.per.Post <- c(VIDEO_3_Monday, VIDEO_3_Tuesday, VIDEO_3_Wednesday, VIDEO_3_Thursday, VIDEO_3_Friday, VIDEO_3_Saturday, VIDEO_3_Sunday)
Views.per.Post=round(Views.per.Post,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Views.per.Post, main="Views per Post", ylab=NA, ylim=c(0,30000), las=1, col=colors, names=names)

##bar plot video shares
VIDEO_4_Monday=subset(video,Day=="Mon", select=c(Lifetime.Talking.About.This..Post..by.action.type...share))
VIDEO_4_Monday=sum(VIDEO_4_Monday$Lifetime.Talking.About.This..Post..by.action.type...share)
VIDEO_4_Tuesday=subset(video,Day=="Tue", select=c(Lifetime.Talking.About.This..Post..by.action.type...share))
VIDEO_4_Tuesday=sum(VIDEO_4_Tuesday$Lifetime.Talking.About.This..Post..by.action.type...share)
VIDEO_4_Wednesday=subset(video,Day=="Wed", select=c(Lifetime.Talking.About.This..Post..by.action.type...share))
VIDEO_4_Wednesday=sum(VIDEO_4_Wednesday$Lifetime.Talking.About.This..Post..by.action.type...share)
VIDEO_4_Thursday=subset(video,Day=="Thu", select=c(Lifetime.Talking.About.This..Post..by.action.type...share))
VIDEO_4_Thursday=sum(VIDEO_4_Thursday$Lifetime.Talking.About.This..Post..by.action.type...share)
VIDEO_4_Friday=subset(video,Day=="Fri", select=c(Lifetime.Talking.About.This..Post..by.action.type...share))
VIDEO_4_Friday=sum(VIDEO_4_Friday$Lifetime.Talking.About.This..Post..by.action.type...share)
VIDEO_4_Saturday=subset(video,Day=="Sat", select=c(Lifetime.Talking.About.This..Post..by.action.type...share))
VIDEO_4_Saturday=sum(VIDEO_4_Saturday$Lifetime.Talking.About.This..Post..by.action.type...share)
VIDEO_4_Sunday=subset(video,Day=="Sun", select=c(Lifetime.Talking.About.This..Post..by.action.type...share))
VIDEO_4_Sunday=sum(VIDEO_4_Sunday$Lifetime.Talking.About.This..Post..by.action.type...share)

VIDEO_4_Monday; VIDEO_4_Tuesday; VIDEO_4_Wednesday; VIDEO_4_Thursday; VIDEO_4_Friday; VIDEO_4_Saturday; VIDEO_4_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Total.Video.Shares <- c(VIDEO_4_Monday, VIDEO_4_Tuesday, VIDEO_4_Wednesday, VIDEO_4_Thursday, VIDEO_4_Friday, VIDEO_4_Saturday, VIDEO_4_Sunday)
Total.Video.Shares=round(Total.Video.Shares,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Total.Video.Shares, main="Total Video Shares", ylab=NA, ylim=c(0,4000), las=1, col=colors, names=names)
#pie with types of share, video post etc
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#add followers
followers=read.csv("H:/Project Dissertation/Source data/Q1 2016 to Q2 2018 Followers.csv", header=T)
dim(followers)
str(followers)
names(followers)

class(followers$Date)
head(followers$Date)
followers$Date=as.Date(followers$Date, format = "%d/%m/%Y")
head(followers)
tail(followers)
class(followers$Total.Followers)
tail(followers$Total.Followers)
class(followers$Daily.New.Followers)

followers$Total.Followers<- as.numeric(gsub(",", "", as.character(followers$Total.Followers))) #correct format from factor
followers$Total.Followers[is.na(followers$Total.Followers)] <- 0
class(followers$Total.Followers)

followers$Daily.New.Followers<- as.numeric(gsub(",", "", as.character(followers$Daily.New.Followers))) #correct format from factor
followers$Daily.New.Followers[is.na(followers$Daily.New.Followers)] <- 0
class(followers$Daily.New.Followers)

#add days of week
library(lubridate)
followers$Date <- as.Date(followers$Date)
followers$Day=wday(followers$Date, label=TRUE)
followers$Day
names(followers)
head(followers)

#reposition Day
followers=subset(followers,select=c(Date, Day,Total.Followers,Daily.New.Followers))

par(mfrow=c(2,2))
hist(followers$Total.Followers,col = 'red', breaks = 10, xlim=range(followers$Total.Followers), xlab= "Total Followers", 
     ylab="Count", main="Histogram Total Followers", ps=50, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, cex.sub=1.5)
hist(followers$Daily.New.Followers,col = 'blue', breaks = 10, xlim=range(followers$Daily.New.Followers), xlab= "New Followers", 
     ylab="Count", main="Histogram New Followers", ps=50, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, cex.sub=1.5)
plot(followers$Total.Followers,col = 'red')
plot(followers$Daily.New.Followers,col = 'blue')
qqnorm(followers$Total.Followers,col = 'red')
qqnorm(followers$Daily.New.Followers,col = 'blue')

#install.packages("timeSeries")
library(timeSeries)
#plot timerseries 
plot(as.timeSeries(followers$Total.Followers), at = "chic", minor.ticks="daily",col="red",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Total Followers (Daily)")

plot(as.timeSeries(followers$Daily.New.Followers), at = "chic", minor.ticks="daily",col="red",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Change in Followers (Daily)")

#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)

ggdensity(followers$Total.Followers, 
          main = "Total Fiollowers",
          xlab = "Post")

ggqqplot(followers$Total.Followers)

ggdensity(followers$Daily.New.Followers, 
          main = "New Followers",
          xlab = "Post")

ggqqplot(followers$Daily.New.Followers)

#p value less than 0.5 NOT normally dist.

#Change in followers excluding the initial load splike, make same size as page vector and add average if needed for first month or so
TFF=subset(followers,Date>"2016-07-07",select=c(Date,Total.Followers,Daily.New.Followers))
TFF<- as.numeric(gsub(",", "", as.character(TFF$Total.Followers)))
TFF[is.na(TFF)] <- 0

#Change in followers excluding the initial load splike (changed date range)
CNF=subset(followers,Date>="2016-07-07",select=c(Date,Total.Followers,Daily.New.Followers))
CNF<- as.numeric(gsub(",", "", as.character(CNF$Daily.New.Followers)))
CNF[is.na(CNF)] <- 0

#cut blank data that was added to make full date vector
followers_gg=subset(followers,select=c(Date,Day,Total.Followers,Daily.New.Followers))
followers_gg=followers_gg[189:911,]
head(followers_gg)
tail(followers_gg)

par(mfrow=c(3,2))
hist(followers_gg$Total.Followers,col = 'red', breaks = 10, xlim=range(followers_gg$Total.Followers ), xlab= "Total Followers", 
     ylab="Count", main="Histogram Total Followers", ps=50, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, cex.sub=1.5)
hist(followers_gg$Daily.New.Followers,col = 'blue', breaks = 50, xlim=range(followers_gg$Daily.New.Followers), xlab= "New Followers", 
     ylab="Count", main="Histogram New Followers", ps=50, cex.lab=1.5, cex.axis=1.5, cex.main=2.0, cex.sub=1.5)
plot(followers_gg$Total.Followers,col = 'red')
plot(followers_gg$Daily.New.Followers,col = 'blue')
qqnorm(followers_gg$Total.Followers,col = 'red')
qqnorm(followers_gg$Daily.New.Followers,col = 'blue')

summary(followers_gg$Total.Followers)
var(followers_gg$Total.Followers, na.rm = T)
sd(followers_gg$Total.Followers, na.rm = T)
cor(followers_gg$Total.Followers,followers_gg$Daily.New.Followers)
summary(followers_gg$Daily.New.Followers)
var(followers_gg$Daily.New.Followers, na.rm = T)
sd(followers_gg$Daily.New.Followers, na.rm = T)

#p value less than 0.5 NOT normally dist.
shapiro.test(followers_gg$Total.Followers)
shapiro.test(followers_gg$Daily.New.Followers)
tail(followers_gg)
ggdensity(TFF, 
          main = "Total Followers",
          xlab = "Post")

ggdensity(CNF, 
          main = "New Followers",
          xlab = "Post")

ggqqplot(TFF)
ggqqplot(CNF)

#install.packages("timeSeries")
library(timeSeries)

plot(as.timeSeries(TFF), at = "chic", minor.ticks="daily",col="blue",type="l",lwd=3.0,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Followers 12-06-2016 to 30-06-2018")
legend("top",legend=c("Total Followers"),fill=c("blue"),horiz=FALSE,cex=0.9,xpd=TRUE)

plot(as.timeSeries(CNF), at = "chic", minor.ticks="daily",col="red",type="l",lwd=3.0,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("New Followers by Day - 07/07/2016 to 29/6/2018")

boxplotchart <- ggplot(followers_gg, aes(x=Day, y = Daily.New.Followers )) +
  geom_boxplot() 
boxplotchart + 
  ylab("New Followers") + 
  xlab("") 

#cormat
length(followers_gg$Total.Followers)
page_foll=page[26:748,]
names(page_foll)

#create df foll mat
follmat=data.frame(page_foll$Date, page_foll$Lifetime.Total.Likes, page_foll$Daily.New.Likes, followers_gg$Total.Followers, followers_gg$Daily.New.Followers)
names(follmat)
scatmat_foll=follmat[c("page_foll.Date","page_foll.Lifetime.Total.Likes","followers_gg.Total.Followers","page_foll.Daily.New.Likes","followers_gg.Daily.New.Followers")]

#visualise - scatter matrix 
pairs(scatmat_foll[2:3],
      pch = 19, col = adjustcolor(c("red","red","red","red","red"), alpha.f = 0.4),oma=c(8,4,8,4),
      main = "Total Followers versus Total Likes")

pairs(scatmat_foll[4:5],
      pch = 19, col = adjustcolor(c("dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue"), alpha.f = 0.4),oma=c(8,4,8,4),
      main = "Daily New Followers versus Daily New Likes")

cor(follmat$page_foll.Lifetime.Total.Likes,follmat$followers_gg.Total.Followers)
cor(follmat$page_foll.Daily.New.Likes,follmat$followers_gg.Daily.New.Followers)
#-------------------------------------------------plot timeseries using ggplot------------------ 

theme_set(theme_minimal())
ts_page_1=ggplot(data = followers_gg, aes(x = Date, y = Total.Followers )) + 
  geom_line(color = "blue", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("TOTAL FOLLOWERS")

ts_page_1=ggplot(data = followers_gg, aes(x = Date, y = Daily.New.Followers)) + 
  geom_line(color = "red", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("CHANGE IN DAILY NEW FOLLOWERS")

par(mfrow=c(1,1))
##bar plot new followers
FOLLOWERS_1_Monday=subset(followers_gg,Day=="Mon", select=c(Daily.New.Followers))
FOLLOWERS_1_Monday=sum(FOLLOWERS_1_Monday$Daily.New.Followers)
FOLLOWERS_1_Tuesday=subset(followers_gg,Day=="Tue", select=c(Daily.New.Followers))
FOLLOWERS_1_Tuesday=sum(FOLLOWERS_1_Tuesday$Daily.New.Followers)
FOLLOWERS_1_Wednesday=subset(followers_gg,Day=="Wed", select=c(Daily.New.Followers))
FOLLOWERS_1_Wednesday=sum(FOLLOWERS_1_Wednesday$Daily.New.Followers)
FOLLOWERS_1_Thursday=subset(followers_gg,Day=="Thu", select=c(Daily.New.Followers))
FOLLOWERS_1_Thursday=sum(FOLLOWERS_1_Thursday$Daily.New.Followers)
FOLLOWERS_1_Friday=subset(followers_gg,Day=="Fri", select=c(Daily.New.Followers))
FOLLOWERS_1_Friday=sum(FOLLOWERS_1_Friday$Daily.New.Followers)
FOLLOWERS_1_Saturday=subset(followers_gg,Day=="Sat", select=c(Daily.New.Followers))
FOLLOWERS_1_Saturday=sum(FOLLOWERS_1_Saturday$Daily.New.Followers)
FOLLOWERS_1_Sunday=subset(followers_gg,Day=="Sun", select=c(Daily.New.Followers))
FOLLOWERS_1_Sunday=sum(FOLLOWERS_1_Sunday$Daily.New.Followers)

FOLLOWERS_1_Monday; FOLLOWERS_1_Tuesday; FOLLOWERS_1_Wednesday; FOLLOWERS_1_Thursday; FOLLOWERS_1_Friday; FOLLOWERS_1_Saturday; FOLLOWERS_1_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
New.followers <- c(FOLLOWERS_1_Monday, FOLLOWERS_1_Tuesday, FOLLOWERS_1_Wednesday, FOLLOWERS_1_Thursday, FOLLOWERS_1_Friday, FOLLOWERS_1_Saturday, FOLLOWERS_1_Sunday)
New.followers=round(New.followers,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (New.followers, main="Daily New followers", ylab=NA, ylim=c(0,20000), las=1, col=colors, names=names)

#install.packages("openxlxs")
library(openxlsx)
write.xlsx(followers, 'followers_look.xlsx')

#-----------sales data---------------------------------------------------------------------------------------------------------------------------------------------

#add main response variable, New Ticket Sales
#============================================

#get data from database using MS server

#install.packages("DBI")
#library(DBI)
#con <- dbConnect(odbc::odbc(), Driver = "SQL Server",
                 #Server = "edi-sqla",
                 #Database = "data_hub",
                 #Trusted_Connection="Yes",
                 #Port = 1433)

#dbListTables(con)

# test query on small table to check connection:
#res <- dbSendQuery(con, "select * from Cognos.tbl_area_names")
#dbFetch(res)

#sales query
#res <- dbSendQuery(con, "select CAST(created_time as Date),  mac, payment_method, COUNT(*)
                   #from [Data_Hub].[Insight].[srcvw_participation_changes] a 
                   #left join [Data_Hub].[Insight].[srcvw_earliest_payment_method] b on 
                   #(a.account_holder_number=b.account_holder_number) and  
                   #(a.b_period=b.b_period) and
                   #(a.b_transaction=b.b_transaction)
                   #--(a.participation_number=b.participation_number)
                   #where 
                   #created_time>='2016-01-01' and created_time<'2018-07-01' 
                   #and
                   #a.mac Not In ('TMO.2807.M','TMO.2788.M','TMO.2789.M','TMO.2790.M','TMO.3001.M','TST.4020.T') AND 
                   #((a.participation_fee)='REGULAR') AND 
                   #((a.end_draw_period) Is Null) AND 
                   #((a.ticket_revision)=1)
                   #Group BY CAST(created_time as Date),  mac, payment_method
                   #order by CAST(created_time as Date),  mac, payment_method;")
#dbFetch(res)

#export to excel for project as academic advisor will not be able to connect to the DB when checking file
#library(openxlsx)
#write.xlsx(a, 'peace.xlsx')  #exports to H:\Documents

#dbClearResult(res)
#----------------------

#load csv
Ticket_Sales=read.csv("H:/Project Dissertation/Source data/Ticket Sales for R.csv", header=T)
head(Ticket_Sales)  #from 1 Jan 2016
tail(Ticket_Sales) #to 30 Jun 2018
names(Ticket_Sales) # types of ticket sales
str(Ticket_Sales) # all data factorised
dim(Ticket_Sales)

#reclassify format to numeric
Ticket_Sales$Total<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Total)));
Ticket_Sales$Total[is.na(Ticket_Sales$Total)]<-0
Ticket_Sales$X1Call.Direct<- as.numeric(gsub(",", "", as.character(Ticket_Sales$X1Call.Direct)));
Ticket_Sales$X1Call.Direct[is.na(Ticket_Sales$X1Call.Direct)]<-0
Ticket_Sales$Abandoned.Web.Leads<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Abandoned.Web.Leads)));
Ticket_Sales$Abandoned.Web.Leads[is.na(Ticket_Sales$Abandoned.Web.Leads)]<-0
Ticket_Sales$Charity.Web<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Charity.Web)));
Ticket_Sales$Charity.Web[is.na(Ticket_Sales$Charity.Web)]<-0
Ticket_Sales$Desktop.PPC<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Desktop.PPC)));
Ticket_Sales$Desktop.PPC[is.na(Ticket_Sales$Desktop.PPC)]<-0
Ticket_Sales$Direct.Mail<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Direct.Mail)));
Ticket_Sales$Direct.Mail[is.na(Ticket_Sales$Direct.Mail)]<-0
Ticket_Sales$Display.Ads<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Display.Ads)));
Ticket_Sales$Display.Ads[is.na(Ticket_Sales$Display.Ads)]<-0
Ticket_Sales$Door.Drop<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Door.Drop)));
Ticket_Sales$Door.Drop[is.na(Ticket_Sales$Door.Drop)]<-0
Ticket_Sales$DRTV<- as.numeric(gsub(",", "", as.character(Ticket_Sales$DRTV)));
Ticket_Sales$DRTV[is.na(Ticket_Sales$DRTV)]<-0
Ticket_Sales$Email.Newsletter<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Email.Newsletter)));
Ticket_Sales$Email.Newsletter[is.na(Ticket_Sales$Email.Newsletter)]<-0
Ticket_Sales$Failed.Transactions<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Failed.Transactions)));
Ticket_Sales$Failed.Transactions[is.na(Ticket_Sales$Failed.Transactions)]<-0
Ticket_Sales$Internal.TM<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Internal.TM)));
Ticket_Sales$Internal.TM[is.na(Ticket_Sales$Internal.TM)]<-0
Ticket_Sales$Maggies<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Maggies)));
Ticket_Sales$Maggies[is.na(Ticket_Sales$Maggies)]<-0
Ticket_Sales$Missing.People<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Missing.People)));
Ticket_Sales$Missing.People[is.na(Ticket_Sales$Missing.People)]<-0
Ticket_Sales$Mobile.App<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Mobile.App)));
Ticket_Sales$Mobile.App[is.na(Ticket_Sales$Mobile.App)]<-0
Ticket_Sales$Mobile.Generic<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Mobile.Generic)));
Ticket_Sales$Mobile.Generic[is.na(Ticket_Sales$Mobile.Generic)]<-0
Ticket_Sales$Mobile.PPC<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Mobile.PPC)));
Ticket_Sales$Mobile.PPC[is.na(Ticket_Sales$Mobile.PPC)]<-0
Ticket_Sales$MS_Email_Outbound<- as.numeric(gsub(",", "", as.character(Ticket_Sales$MS_Email_Outbound)));
Ticket_Sales$MS_Email_Outbound[is.na(Ticket_Sales$MS_Email_Outbound)]<-0
Ticket_Sales$PPL.Email.Outbound<- as.numeric(gsub(",", "", as.character(Ticket_Sales$PPL.Email.Outbound)));
Ticket_Sales$PPL.Email.Outbound[is.na(Ticket_Sales$PPL.Email.Outbound)]<-0
Ticket_Sales$PPL.Postal.Outbound<- as.numeric(gsub(",", "", as.character(Ticket_Sales$PPL.Postal.Outbound)));
Ticket_Sales$PPL.Postal.Outbound[is.na(Ticket_Sales$PPL.Postal.Outbound)]<-0
Ticket_Sales$Other.Web.Gen<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Other.Web.Gen)));
Ticket_Sales$Other.Web.Gen[is.na(Ticket_Sales$Other.Web.Gen)]<-0
Ticket_Sales$Weekly.Draw.Results<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Weekly.Draw.Results)));
Ticket_Sales$Weekly.Draw.Results[is.na(Ticket_Sales$Weekly.Draw.Results)]<-0
Ticket_Sales$Old.Inbound<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Old.Inbound)));
Ticket_Sales$Old.Inbound[is.na(Ticket_Sales$Old.Inbound)]<-0
Ticket_Sales$Press<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Press)));
Ticket_Sales$Press[is.na(Ticket_Sales$Press)]<-0
Ticket_Sales$Social.Media<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Social.Media)));
Ticket_Sales$Social.Media[is.na(Ticket_Sales$Social.Media)]<-0
Ticket_Sales$Summer.Millions.Mobile<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Summer.Millions.Mobile)));
Ticket_Sales$Summer.Millions.Mobile[is.na(Ticket_Sales$Summer.Millions.Mobile)]<-0
Ticket_Sales$Website.Gen<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Website.Gen)));
Ticket_Sales$Website.Gen[is.na(Ticket_Sales$Website.Gen)]<-0
Ticket_Sales$Test<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Test)));
Ticket_Sales$Test[is.na(Ticket_Sales$Test)]<-0
Ticket_Sales$Email.Outbound<- as.numeric(gsub(",", "", as.character(Ticket_Sales$Email.Outbound)));
Ticket_Sales$Email.Outbound[is.na(Ticket_Sales$Email.Outbound)]<-0

#cannot convert date, use page vector
Ticket_Sales$Date=original_page$Date
class(Ticket_Sales$Date)

names(Ticket_Sales)
str(Ticket_Sales) # corrected factors
str(Ticket_Sales$Date) # corrected factors

Ticket_Sales[is.na(Ticket_Sales)]<-0

#add days of week
Original_Ticket_Sales=Ticket_Sales
library(lubridate)
Ticket_Sales$Day=wday(Ticket_Sales$Date, label=TRUE)
Ticket_Sales$Day
names(Ticket_Sales)

#reposition Day
Ticket_Sales=subset(Ticket_Sales,select=c(Date, Day,X1Call.Direct,Abandoned.Web.Leads,Charity.Web,Desktop.PPC,Direct.Mail,Display.Ads,Door.Drop,DRTV,Internal.TM,Maggies,Missing.People,Mobile.App,Mobile.Generic,Mobile.PPC,MS_Email_Outbound,Other.Web.Gen,PPL.Email.Outbound,PPL.Postal.Outbound,Press,Social.Media,Summer.Millions.Mobile,Test,Website.Gen,Weekly.Draw.Results,Old.Inbound,Total))

#normally distributed?
par(mfrow=c(3,2))
hist(Ticket_Sales$Total,col = 'red', breaks = 50, xlim=range(Ticket_Sales$Total ), xlab= "Total Sales", 
     ylab="Count", main="Histogram Total Sales", ps=50, cex.lab=1.0, cex.axis=1.0, cex.main=1.5, cex.sub=1.0)
hist(Ticket_Sales$Social.Media,col = 'blue', breaks = 100, xlim=range(Ticket_Sales$Social.Media), xlab= "Social Media Volumes", 
     ylab="Count", main="Histogram Social Media Sales", ps=50, cex.lab=1.0, cex.axis=1.5, cex.main=1.5, cex.sub=1.0)
plot(Ticket_Sales$Total,col = 'red')
plot(Ticket_Sales$Social.Media,col = 'blue')
qqnorm(Ticket_Sales$Total,col = 'red')
qqnorm(Ticket_Sales$Social.Media,col = 'blue')

summary(Ticket_Sales$Total)
var(Ticket_Sales$Total, na.rm = T)
sd(Ticket_Sales$Total, na.rm = T)

summary(Ticket_Sales$Social.Media)
var(Ticket_Sales$Social.Media, na.rm = T)
sd(Ticket_Sales$Social.Media, na.rm = T)

shapiro.test(Ticket_Sales$Total)
shapiro.test(Ticket_Sales$Social.Media)

cor(Ticket_Sales$Total,Ticket_Sales$Social.Media)

#create df foll mat
salesmat=data.frame(Ticket_Sales$Date,Ticket_Sales$Total,Ticket_Sales$Social.Media)
names(salesmat)

#visualise - scatter matrix 
pairs(salesmat[2:3],
      pch = 19, col = adjustcolor(c("dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue"), alpha.f = 0.4),oma=c(8,4,8,4),
      main = "Total Ticket Sales versus Social Media Ticket Sales")

par(mfrow=c(2,2))
#install.packages("timeSeries")
library(timeSeries)
plot(as.timeSeries(Ticket_Sales$Total), at = "chic", minor.ticks="daily",col="blue",type="l",lwd=0.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Total Ticket Sales by Day")

plot(as.timeSeries(Ticket_Sales$Social.Media), at = "chic", minor.ticks="daily",col="red",type="l",lwd=3.0,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Social Media Sales 12.6.2018 to 30.6.2018")
legend("top",legend=c("Social Media Sales per Day"),fill=c("red"),horiz=FALSE,cex=0.9,xpd=TRUE)

theme_set(theme_minimal())
ts_sales_1=ggplot(data = Ticket_Sales, aes(x = Ticket_Sales$Date, y = Ticket_Sales$Total)) + 
  geom_line(color = "blue", size = 0.25) +ggtitle("TOTAL TICKET SALES")
ts_sales_1+scale_x_date(date_labels = "%b/%Y")
ts_sales_1 + stat_smooth(color = "red",size = 0.25)

ts_sales_1=ggplot(data = Ticket_Sales, aes(x = Ticket_Sales$Date, y = Ticket_Sales$Social.Media)) + 
  geom_line(color = "red", size = 0.25) +ggtitle("SOCIAL MEDIA TICKET SALES")
ts_sales_1+scale_x_date(date_labels = "%b/%Y")
ts_sales_1 + stat_smooth(color = "green",size = 0.25)

boxplot(Ticket_Sales$Total, horizontal = T) # lot of outliers
boxplot(Ticket_Sales$Social.Media, horizontal = T) # lot of outliers

boxplotchart <- ggplot(Ticket_Sales, aes(x=Day, y = Total)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Total Ticket Sales") + 
  xlab("Day of the Week") 

boxplotchart <- ggplot(Ticket_Sales, aes(x=Day, y = Social.Media)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Social Media Ticket Sales") + 
  xlab("Day of the Week") 

ggdensity(Ticket_Sales$Total, 
          main = "Total Sales",
          xlab = "Days")

ggqqplot(Ticket_Sales$Total)

ggdensity(Ticket_Sales$Social.Media, 
          main = "Social Media Sales",
          xlab = "Post")

ggqqplot(Ticket_Sales$Social.Media)

# barplot for sales
#------------------------------------------
##bar plot new Ticket_Sales
SALES_1_Monday=subset(Ticket_Sales,Day=="Mon", select=c(Total))
SALES_1_Monday=sum(SALES_1_Monday$Total)
SALES_1_Tuesday=subset(Ticket_Sales,Day=="Tue", select=c(Total))
SALES_1_Tuesday=sum(SALES_1_Tuesday$Total)
SALES_1_Wednesday=subset(Ticket_Sales,Day=="Wed", select=c(Total))
SALES_1_Wednesday=sum(SALES_1_Wednesday$Total)
SALES_1_Thursday=subset(Ticket_Sales,Day=="Thu", select=c(Total))
SALES_1_Thursday=sum(SALES_1_Thursday$Total)
SALES_1_Friday=subset(Ticket_Sales,Day=="Fri", select=c(Total))
SALES_1_Friday=sum(SALES_1_Friday$Total)
SALES_1_Saturday=subset(Ticket_Sales,Day=="Sat", select=c(Total))
SALES_1_Saturday=sum(SALES_1_Saturday$Total)
SALES_1_Sunday=subset(Ticket_Sales,Day=="Sun", select=c(Total))
SALES_1_Sunday=sum(SALES_1_Sunday$Total)

SALES_1_Monday; SALES_1_Tuesday; SALES_1_Wednesday; SALES_1_Thursday; SALES_1_Friday; SALES_1_Saturday; SALES_1_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Ticket.Sales <- c(SALES_1_Monday, SALES_1_Tuesday, SALES_1_Wednesday, SALES_1_Thursday, SALES_1_Friday, SALES_1_Saturday, SALES_1_Sunday)
Ticket.Sales=round(Ticket.Sales,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Ticket.Sales, main="Total Ticket Sales - All Platforms", ylab=NA, ylim=c(0,500000), las=1, col=colors, names=names)

# barplot for sales
#------------------------------------------
##bar plot new Ticket_Sales
SALES_2_Monday=subset(Ticket_Sales,Day=="Mon", select=c(Social.Media))
SALES_2_Monday=sum(SALES_2_Monday$Social.Media)
SALES_2_Tuesday=subset(Ticket_Sales,Day=="Tue", select=c(Social.Media))
SALES_2_Tuesday=sum(SALES_2_Tuesday$Social.Media)
SALES_2_Wednesday=subset(Ticket_Sales,Day=="Wed", select=c(Social.Media))
SALES_2_Wednesday=sum(SALES_2_Wednesday$Social.Media)
SALES_2_Thursday=subset(Ticket_Sales,Day=="Thu", select=c(Social.Media))
SALES_2_Thursday=sum(SALES_2_Thursday$Social.Media)
SALES_2_Friday=subset(Ticket_Sales,Day=="Fri", select=c(Social.Media))
SALES_2_Friday=sum(SALES_2_Friday$Social.Media)
SALES_2_Saturday=subset(Ticket_Sales,Day=="Sat", select=c(Social.Media))
SALES_2_Saturday=sum(SALES_2_Saturday$Social.Media)
SALES_2_Sunday=subset(Ticket_Sales,Day=="Sun", select=c(Social.Media))
SALES_2_Sunday=sum(SALES_2_Sunday$Social.Media)

SALES_2_Monday; SALES_2_Tuesday; SALES_2_Wednesday; SALES_2_Thursday; SALES_2_Friday; SALES_2_Saturday; SALES_2_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
Ticket.Sales <- c(SALES_2_Monday, SALES_2_Tuesday, SALES_2_Wednesday, SALES_2_Thursday, SALES_2_Friday, SALES_2_Saturday, SALES_2_Sunday)
Ticket.Sales=round(Ticket.Sales,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp <- barplot (Ticket.Sales, main="Tickets Sold - Social Media", ylab=NA, ylim=c(0,8000), las=1, col=colors, names=names)

attach(Ticket_Sales)
names(Ticket_Sales)

#X1Call.Direct=sum(Ticket_Sales$X1Call.Direct)
#Charity.Web=sum(Ticket_Sales$Charity.Web)
#Door.Drop=sum(Ticket_Sales$Door.Drop)
#Missing.People=sum(Ticket_Sales$Missing.People)
#MS_Email_Outbound=sum(Ticket_Sales$MS_Email_Outbound)
#Press=sum(Ticket_Sales$Press)
#Website.Gen=sum(Ticket_Sales$Website.Gen)
#Abandoned.Web.Leads=sum(Ticket_Sales$Abandoned.Web.Leads)
#Desktop.PPC=sum(Ticket_Sales$Desktop.PPC)
#DRTV=sum(Ticket_Sales$DRTV)
#Mobile.App=sum(Ticket_Sales$Mobile.App)
#Other.Web.Gen=sum(Ticket_Sales$Other.Web.Gen)
#Social.Media=sum(Ticket_Sales$Social.Media)
#Weekly.Draw.Results=sum(Ticket_Sales$Weekly.Draw.Results)
#Direct.Mail=sum(Ticket_Sales$Direct.Mail)
#Internal.TM=sum(Ticket_Sales$Internal.TM)
#Mobile.Generic=sum(Ticket_Sales$Mobile.Generic)
#PPL.Email.Outbound=sum(Ticket_Sales$PPL.Email.Outbound)
#Summer.Millions.Mobile=sum(Ticket_Sales$Summer.Millions.Mobile)
#Old.Inbound=sum(Ticket_Sales$Old.Inbound)
#Display.Ads=sum(Ticket_Sales$Display.Ads)
#Maggies=sum(Ticket_Sales$Maggie)
#Mobile.PPC=sum(Ticket_Sales$Mobile.PPC)
#PPL.Postal.Outbound=sum(Ticket_Sales$PPL.Postal.Outbound)
#Test=sum(Ticket_Sales$Test)
#Total=sum(Ticket_Sales$Total)

#X1Call.Direct;Charity.Web;Door.Drop;Missing.People;MS_Email_Outbound;Press;Website.Gen;Abandoned.Web.Leads;Desktop.PPC;DRTV;Mobile.App;Other.Web.Gen;Social.Media;Weekly.Draw.Results;Direct.Mail;Internal.TM;Mobile.Generic;PPL.Email.Outbound;Summer.Millions.Mobile;Old.Inbound;Display.Ads;Maggies;Mobile.PPC;PPL.Postal.Outbound;Test;Total
#All.Ticket.Sales <- c(X1Call.Direct,Charity.Web,Door.Drop,Missing.People,MS_Email_Outbound,Press,Website.Gen,Abandoned.Web.Leads,Desktop.PPC,DRTV,Mobile.App,Other.Web.Gen,Social.Media,Weekly.Draw.Results,Direct.Mail,Internal.TM,Mobile.Generic,PPL.Email.Outbound,Summer.Millions.Mobile,Old.Inbound,Display.Ads,Maggies,Mobile.PPC,PPL.Postal.Outbound,Test)
#colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
#names <- c("X1Call.Direct","Charity.Web","Door.Drop","Missing.People","MS_Email_Outbound","Press","Website.Gen","Abandoned.Web.Leads","Desktop.PPC","DRTV","Mobile.App","Other.Web.Gen","Social.Media","Weekly.Draw.Results","Direct.Mail","Internal.TM","Mobile.Generic","PPL.Email.Outbound","Summer.Millions.Mobile","Old.Inbound","Display.Ads","Maggies","Mobile.PPC","PPL.Postal.Outbound","Test")
#bp <- barplot (All.Ticket.Sales, main="Tickets Sold - All Platforms", ylab=NA, ylim=c(0,1100000), las=1, col=colors, names=names)
#axis(1, at=xx, labels=names, tick=FALSE, las=2, line=-0.5, cex.axis=0.6)
#detach(Ticket_Sales)

dim(adv_vols);dim(adv_cost)
#load advertising data
adv_vols=read.csv("H:/Project Dissertation/Source data/advertising_vols.csv", header=T,as.is=T, stringsAsFactors=F)
head(adv_vols)
names(adv_vols)
str(adv_vols)
dim(adv_vols)
tail(adv_vols$Date)

#fix Date - change format in source file
#convert date from factor to date format
class(adv_vols$Date)
head(adv_vols$Date)
adv_vols$Date=as.Date(as.character(adv_vols$Date, format = "%Y/%m/%d"))

#complete missing rows - data range starts from 2016-12-21 but for consuistency make from 2016-01-01
nrow(adv_vols)
attach(adv_vols)
date_range <- seq(as.Date("2016-01-01"), as.Date("2018-06-29"),  # range is 911 rows
                  by = "1 day")
date_range <- data.frame(Date = date_range)

adv_vols <- merge(date_range, adv_vols, by = "Date", 
                  all.x = TRUE)
nrow(adv_vols)
adv_vols[is.na(adv_vols)]<-0
detach(adv_vols)

sum(adv_vols$adv_vols.Total);sum(adv_vols$adv_vols.Social.Media) # new total as date clipped at 29/6/2018, checked and good

adv_cost=read.csv("H:/Project Dissertation/Source data/advertising_cost.csv", header=T, as.is=T,stringsAsFactors=F)
head(adv_cost)
names(adv_cost)
str(adv_cost)
dim(adv_cost)

#fix Date
#convert date from factor to date format
class(adv_cost$Date)
head(adv_cost$Date)
adv_cost$Date=as.Date(as.character(adv_cost$Date, format = "%Y/%m/%d"))

sum(adv_cost$adv_cost.Total);sum(adv_cost$adv_cost.Social.Media)

#complete missing rows - data range starts from 2016-12-21 but for consuistency make from 2016-01-01
nrow(adv_cost)
attach(adv_cost)
date_range <- seq(as.Date("2016-01-01"), as.Date("2018-06-29"),  # range is 911 rows
                  by = "1 day")
date_range <- data.frame(Date = date_range)

adv_cost <- merge(date_range, adv_cost, by = "Date", 
                  all.x = TRUE)
nrow(adv_cost)
adv_cost[is.na(adv_cost)]<-0
detach(adv_cost)

sum(adv_cost$adv_cost.Total);sum(adv_cost$adv_cost.Social.Media)

names(adv_vols)
#add days of week
#install.packages("lubridate")
library(lubridate)
library(dplyr)
library(stringi)
adv_vols$Date <- as.Date(adv_vols$Date)
adv_vols$Day=wday(adv_vols$Date, label=TRUE)
adv_vols$Day
names(adv_vols)

names(adv_vols)
#add days of week
#install.packages("lubridate")
library(lubridate)
adv_cost$Date <- as.Date(adv_cost$Date)
adv_cost$Day=wday(adv_cost$Date, label=TRUE)
adv_cost$Day
names(adv_cost)

#drop the blank range
original_adv_vols=adv_vols
adv_vols=adv_vols[356:911,] # exclude zero rows
dim(adv_vols)
head(adv_vols$Date)
tail(adv_vols$Date); head(adv_vols$Date)

original_adv_cost=adv_cost
adv_cost=adv_cost[356:911,] # exclude zero rows
dim(adv_cost)
head(adv_cost$Date)
tail(adv_cost$Date); head(adv_cost$Date)

par(mfrow=c(3,2))
hist(adv_vols$adv_vols.Total,col = 'red', breaks = 15, xlim=range(adv_vols$adv_vols.Total ), xlab= "Total Volumes", 
     ylab="Count", main="Histogram Total Ad Volumes", ps=50, cex.lab=1.0, cex.axis=1.0, cex.main=1.5, cex.sub=1.0)
hist(adv_vols$adv_vols.Social.Media,col = 'blue', breaks = 10, xlim=range(adv_vols$adv_vols.Social.Media), xlab= "Social Media Volumes", 
     ylab="Count", main="Histogram Social Media Ad Volumes", ps=50, cex.lab=1.0, cex.axis=1.5, cex.main=1.5, cex.sub=1.0)
plot(adv_vols$adv_vols.Total,col = 'red')
plot(adv_vols$adv_vols.Social.Media,col = 'blue')
qqnorm(adv_vols$adv_vols.Total,col = 'red')
qqnorm(adv_vols$adv_vols.Social.Media,col = 'blue')

hist(adv_cost$adv_cost.Total,col = 'red', breaks = 10, xlim=range(adv_cost$adv_cost.Total ), xlab= "Total Cost", 
     ylab="Count", main="Histogram Total Ad Cost", ps=50, cex.lab=1.0, cex.axis=1.0, cex.main=1.5, cex.sub=1.0)
hist(adv_cost$adv_cost.Social.Media,col = 'blue', breaks = 10, xlim=range(adv_cost$adv_cost.Social.Media), xlab= "Social Media Cost", 
     ylab="Count", main="Histogram Social Media Ad Cost", ps=50, cex.lab=1.0, cex.axis=1.5, cex.main=1.5, cex.sub=1.0)
plot(adv_cost$adv_cost.Total,col = 'red')
plot(adv_cost$adv_cost.Social.Media,col = 'blue')
qqnorm(adv_cost$adv_cost.Total,col = 'red')
qqnorm(adv_cost$adv_cost.Social.Media,col = 'blue')

summary(adv_vols$adv_vols.Total)
var(adv_vols$adv_vols.Total, na.rm = T)
sd(adv_vols$adv_vols.Total, na.rm = T)

summary(adv_vols$adv_vols.Social.Media)
var(adv_vols$adv_vols.Social.Media, na.rm = T)
sd(adv_vols$adv_vols.Social.Media, na.rm = T)

summary(adv_cost$adv_cost.Total)
var(adv_cost$adv_cost.Total, na.rm = T)
sd(adv_cost$adv_cost.Total, na.rm = T)

summary(adv_cost$adv_cost.Social.Media)
var(adv_cost$adv_cost.Social.Media, na.rm = T)
sd(adv_cost$adv_cost.Social.Media, na.rm = T)

shapiro.test(adv_vols$adv_vols.Total)
shapiro.test(adv_vols$adv_vols.Social.Media)
shapiro.test(adv_cost$adv_cost.Total)
shapiro.test(adv_cost$adv_cost.Social.Media)

cor(adv_vols$adv_vols.Total,adv_cost$adv_cost.Total)
cor(adv_vols$adv_vols.Social.Media,adv_cost$adv_cost.Social.Media)

#create df foll mat
advmat=data.frame(adv_vols$Date, adv_vols$adv_vols.Social.Media, adv_cost$adv_cost.Social.Media,adv_vols$adv_vols.Total, adv_cost$adv_cost.Total)
names(advmat)

#visualise - scatter matrix 
pairs(advmat[2:5],
      pch = 19, col = adjustcolor(c("red","red","red","red","red"), alpha.f = 0.4),oma=c(8,4,8,4),
      main = "Ad Volumes versus Cost")
pairs(advmat[2:3],
      pch = 19, col = adjustcolor(c("red","red","red","red","red"), alpha.f = 0.4),oma=c(8,4,8,4),
      main = "Social Media Ad Volumes versus Cost")
pairs(advmat[4:5],
      pch = 19, col = adjustcolor(c("dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue"), alpha.f = 0.4),oma=c(8,4,8,4),
      main = "Total Ad Volumes versus Cost")

#---line plot
library(ggplot2)
theme_set(theme_minimal())
ts_page_1=ggplot(data = adv_vols, aes(x = Date, y = adv_vols.Total )) + 
  geom_line(color = "RED", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("TOTAL ADVERTISING VOLUMES")

ts_page_1=ggplot(data = adv_cost, aes(x = Date, y = adv_cost.Total )) + 
  geom_line(color = "blue", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("TOTAL ADVERTISING COSTS")

ts_page_1=ggplot(data = adv_vols, aes(x = Date, y = adv_vols.Social.Media )) + 
  geom_line(color = "darkorange", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("SOCIAL MEDIA ADVERTISING VOLUMES")

ts_page_1=ggplot(data = adv_cost, aes(x = Date, y = adv_cost.Social.Media )) + 
  geom_line(color = "black", size = 1.0)
ts_page_1+scale_x_date(date_labels = "%b/%Y")+ggtitle("SOCIAL MEDIA ADVERTISING COSTS")

library(timeSeries)
par(mfrow=c(2,2))
#plot timerseries 
plot(as.timeSeries(adv_vols$adv_vols.Total), at = "chic", minor.ticks="daily",col="red",type="l",lwd=1.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Period 21.12.2016 - 19.7.2018")
legend("top",legend=c("Total Ad Vols"),fill=c("red"),horiz=FALSE,cex=0.7,xpd=TRUE)

plot(as.timeSeries(adv_vols$adv_vols.Social.Media), at = "chic", minor.ticks="daily",col="blue",type="l",lwd=1.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Period 21.12.2016 - 19.7.2018")
legend("top",legend=c("Social Media Ad Vols"),fill=c("blue"),horiz=FALSE,cex=0.7,xpd=TRUE)

plot(as.timeSeries(adv_cost$adv_cost.Total), at = "chic", minor.ticks="daily",col="green",type="l",lwd=1.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Period 21.12.2016 - 19.7.2018")
legend("top",legend=c("Total Ad Costs"),fill=c("green"),horiz=FALSE,cex=0.7,xpd=TRUE)

plot(as.timeSeries(adv_cost$adv_cost.Social.Media), at = "chic", minor.ticks="daily",col="black",type="l",lwd=1.5,lty=1,
     mar.multi = c(0.18, 5.0, 0.18, 1.2), oma.multi = c(5, 0, 5, 0),
     col = .colorwheelPalette(10), cex.lab = 0.7, cex.axis = 0.7)
title("Period 21.12.2016 - 19.7.2018")
legend("top",legend=c("Social Media Ad Costs"),fill=c("black"),horiz=FALSE,cex=0.7,xpd=TRUE)

boxplotchart <- ggplot(adv_vols, aes(x=Day, y = adv_vols.Total)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Total Ad Volumes") + 
  xlab("") 

boxplotchart <- ggplot(adv_vols, aes(x=Day, y = adv_vols.Social.Media)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Social Media Ad Vols") + 
  xlab("") 

boxplotchart <- ggplot(adv_cost, aes(x=Day, y = adv_cost.Total)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Total Ad Costs") + 
  xlab("") 

boxplotchart <- ggplot(adv_cost, aes(x=Day, y = adv_cost.Social.Media)) +
  geom_boxplot() 
boxplotchart + 
  ylab("Social Media Ad Costs") + 
  xlab("") 

# barplot for adv volst#------------------------------------------
ADV_vols_Monday=subset(adv_vols,Day=="Mon", select=c(adv_vols.Total))
ADV_vols_Monday=sum(ADV_vols_Monday$adv_vols.Total)
ADV_vols_Tuesday=subset(adv_vols,Day=="Tue", select=c(adv_vols.Total))
ADV_vols_Tuesday=sum(ADV_vols_Tuesday$adv_vols.Total)
ADV_vols_Wednesday=subset(adv_vols,Day=="Wed", select=c(adv_vols.Total))
ADV_vols_Wednesday=sum(ADV_vols_Wednesday$adv_vols.Total)
ADV_vols_Thursday=subset(adv_vols,Day=="Thu", select=c(adv_vols.Total))
ADV_vols_Thursday=sum(ADV_vols_Thursday$adv_vols.Total)
ADV_vols_Friday=subset(adv_vols,Day=="Fri", select=c(adv_vols.Total))
ADV_vols_Friday=sum(ADV_vols_Friday$adv_vols.Total)
ADV_vols_Saturday=subset(adv_vols,Day=="Sat", select=c(adv_vols.Total))
ADV_vols_Saturday=sum(ADV_vols_Saturday$adv_vols.Total)
ADV_vols_Sunday=subset(adv_vols,Day=="Sun", select=c(adv_vols.Total))
ADV_vols_Sunday=sum(ADV_vols_Sunday$adv_vols.Total)

ADV_vols_Monday; ADV_vols_Tuesday; ADV_vols_Wednesday; ADV_vols_Thursday; ADV_vols_Friday; ADV_vols_Saturday; ADV_vols_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
adv.vols <- c(ADV_vols_Monday, ADV_vols_Tuesday, ADV_vols_Wednesday, ADV_vols_Thursday, ADV_vols_Friday, ADV_vols_Saturday, ADV_vols_Sunday)
adv.vols=round(adv.vols,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp1 <- barplot (adv.vols, main="Total Advertising Volumes", ylab=NA, ylim=c(0,1000000000), las=1, col=colors, names=names)

# barplot for adv cost#------------------------------------------
ADV_COST_Monday=subset(adv_cost,Day=="Mon", select=c(adv_cost.Total))
ADV_COST_Monday=sum(ADV_COST_Monday$adv_cost.Total)
ADV_COST_Tuesday=subset(adv_cost,Day=="Tue", select=c(adv_cost.Total))
ADV_COST_Tuesday=sum(ADV_COST_Tuesday$adv_cost.Total)
ADV_COST_Wednesday=subset(adv_cost,Day=="Wed", select=c(adv_cost.Total))
ADV_COST_Wednesday=sum(ADV_COST_Wednesday$adv_cost.Total)
ADV_COST_Thursday=subset(adv_cost,Day=="Thu", select=c(adv_cost.Total))
ADV_COST_Thursday=sum(ADV_COST_Thursday$adv_cost.Total)
ADV_COST_Friday=subset(adv_cost,Day=="Fri", select=c(adv_cost.Total))
ADV_COST_Friday=sum(ADV_COST_Friday$adv_cost.Total)
ADV_COST_Saturday=subset(adv_cost,Day=="Sat", select=c(adv_cost.Total))
ADV_COST_Saturday=sum(ADV_COST_Saturday$adv_cost.Total)
ADV_COST_Sunday=subset(adv_cost,Day=="Sun", select=c(adv_cost.Total))
ADV_COST_Sunday=sum(ADV_COST_Sunday$adv_cost.Total)

ADV_COST_Monday; ADV_COST_Tuesday; ADV_COST_Wednesday; ADV_COST_Thursday; ADV_COST_Friday; ADV_COST_Saturday; ADV_COST_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
adv.cost <- c(ADV_COST_Monday, ADV_COST_Tuesday, ADV_COST_Wednesday, ADV_COST_Thursday, ADV_COST_Friday, ADV_COST_Saturday, ADV_COST_Sunday)
adv.cost=round(adv.cost,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp2 <- barplot (adv.cost, main="Total Advertising Costs", ylab=NA, ylim=c(0,10000000), las=1, col=colors, names=names)

# barplot for adv volst#------------------------------------------
ADV_vols_Monday=subset(adv_vols,Day=="Mon", select=c(adv_vols.Social.Media))
ADV_vols_Monday=sum(ADV_vols_Monday$adv_vols.Social.Media)
ADV_vols_Tuesday=subset(adv_vols,Day=="Tue", select=c(adv_vols.Social.Media))
ADV_vols_Tuesday=sum(ADV_vols_Tuesday$adv_vols.Social.Media)
ADV_vols_Wednesday=subset(adv_vols,Day=="Wed", select=c(adv_vols.Social.Media))
ADV_vols_Wednesday=sum(ADV_vols_Wednesday$adv_vols.Social.Media)
ADV_vols_Thursday=subset(adv_vols,Day=="Thu", select=c(adv_vols.Social.Media))
ADV_vols_Thursday=sum(ADV_vols_Thursday$adv_vols.Social.Media)
ADV_vols_Friday=subset(adv_vols,Day=="Fri", select=c(adv_vols.Social.Media))
ADV_vols_Friday=sum(ADV_vols_Friday$adv_vols.Social.Media)
ADV_vols_Saturday=subset(adv_vols,Day=="Sat", select=c(adv_vols.Social.Media))
ADV_vols_Saturday=sum(ADV_vols_Saturday$adv_vols.Social.Media)
ADV_vols_Sunday=subset(adv_vols,Day=="Sun", select=c(adv_vols.Social.Media))
ADV_vols_Sunday=sum(ADV_vols_Sunday$adv_vols.Social.Media)

ADV_vols_Monday; ADV_vols_Tuesday; ADV_vols_Wednesday; ADV_vols_Thursday; ADV_vols_Friday; ADV_vols_Saturday; ADV_vols_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
adv.vols <- c(ADV_vols_Monday, ADV_vols_Tuesday, ADV_vols_Wednesday, ADV_vols_Thursday, ADV_vols_Friday, ADV_vols_Saturday, ADV_vols_Sunday)
adv.vols=round(adv.vols,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp3 <- barplot (adv.vols, main="Social Media Advertising Volumes", ylab=NA, ylim=c(0,80000000), las=1, col=colors, names=names)

# barplot for adv cost#------------------------------------------
ADV_COST_Monday=subset(adv_cost,Day=="Mon", select=c(adv_cost.Social.Media))
ADV_COST_Monday=sum(ADV_COST_Monday$adv_cost.Social.Media)
ADV_COST_Tuesday=subset(adv_cost,Day=="Tue", select=c(adv_cost.Social.Media))
ADV_COST_Tuesday=sum(ADV_COST_Tuesday$adv_cost.Social.Media)
ADV_COST_Wednesday=subset(adv_cost,Day=="Wed", select=c(adv_cost.Social.Media))
ADV_COST_Wednesday=sum(ADV_COST_Wednesday$adv_cost.Social.Media)
ADV_COST_Thursday=subset(adv_cost,Day=="Thu", select=c(adv_cost.Social.Media))
ADV_COST_Thursday=sum(ADV_COST_Thursday$adv_cost.Social.Media)
ADV_COST_Friday=subset(adv_cost,Day=="Fri", select=c(adv_cost.Social.Media))
ADV_COST_Friday=sum(ADV_COST_Friday$adv_cost.Social.Media)
ADV_COST_Saturday=subset(adv_cost,Day=="Sat", select=c(adv_cost.Social.Media))
ADV_COST_Saturday=sum(ADV_COST_Saturday$adv_cost.Social.Media)
ADV_COST_Sunday=subset(adv_cost,Day=="Sun", select=c(adv_cost.Social.Media))
ADV_COST_Sunday=sum(ADV_COST_Sunday$adv_cost.Social.Media)

ADV_COST_Monday; ADV_COST_Tuesday; ADV_COST_Wednesday; ADV_COST_Thursday; ADV_COST_Friday; ADV_COST_Saturday; ADV_COST_Sunday
#par(mfrow=c(1,0), mar=c(6, 6, 2, 1), mgp=c(0,4,0))
adv.cost <- c(ADV_COST_Monday, ADV_COST_Tuesday, ADV_COST_Wednesday, ADV_COST_Thursday, ADV_COST_Friday, ADV_COST_Saturday, ADV_COST_Sunday)
adv.cost=round(adv.cost,digits=1)
colors <- c("dodgerblue","pink", "black", "green", "red", "lightblue",  "darkorange")
names <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
bp4 <- barplot (adv.cost, main="Social Media Advertising Costs", ylab=NA, ylim=c(0,70000), las=1, col=colors, names=names)

#add adv vectors to cors df
adv_vols_cors=adv_vols
adv_cost_cors=adv_cost

names(adv_vols_cors)
names(adv_cost_cors)
names(cors)
nrow(cors)
dim(adv_cost); dim(adv_vols)

#---------------------PREPARE CONSOLIDATED DF FOR REGRESSION MODEL----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------
#page - create vector from 7/7/2016 
page_cors=original_page[189:911,]
nrow(page_cors)
class(page_cors)
head(page_cors)
names(page_cors)

#post - create vector from 7/7/2016 
post_reg_M=post
names(post_reg_M)
#install.packages("dplyr")
library(dplyr)

post_reg_M=post_reg_M %>%
  group_by(Posted) %>%
  summarise(Lifetime.Post.Total.Reach=sum(Lifetime.Post.Total.Reach),
            Lifetime.Post.Paid.Reach=sum(Lifetime.Post.Paid.Reach),
            Lifetime.Post.Organic.Impressions=sum(Lifetime.Post.Organic.Impressions),
            Lifetime.Engaged.Users=sum(Lifetime.Engaged.Users),
            Lifetime.Post.Consumptions=sum(Lifetime.Post.Consumptions),
            Lifetime.Negative.feedback.from.users=sum(Lifetime.Negative.feedback.from.users),
            Lifetime.Post.reach.by.people.who.like.your.Page=sum(Lifetime.Post.reach.by.people.who.like.your.Page),
            Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=sum(Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page),
            Lifetime.Organic.views.to.95.=sum(Lifetime.Organic.views.to.95.),
            Lifetime.Paid.views.to.95.=sum(Lifetime.Paid.views.to.95.),
            Lifetime.Organic.Video.Views=sum(Lifetime.Organic.Video.Views),
            Lifetime.Paid.Video.Views=sum(Lifetime.Paid.Video.Views),
            Lifetime.Average.time.video.viewed=sum(Lifetime.Average.time.video.viewed),
            Lifetime.Talking.about.this..post..by.action.type...share=sum(Lifetime.Talking.about.this..post..by.action.type...share),
            Lifetime.Talking.about.this..post..by.action.type...comment=sum(Lifetime.Talking.about.this..post..by.action.type...comment),
            Lifetime.Post.stories.by.action.type...like=sum(Lifetime.Post.stories.by.action.type...like),
            Lifetime.Post.consumers.by.type...video.play=sum(Lifetime.Post.consumers.by.type...video.play),
            Lifetime.Post.consumers.by.type...photo.view=sum(Lifetime.Post.consumers.by.type...photo.view),
            Lifetime.Post.consumptions.by.type...video.play=sum(Lifetime.Post.consumptions.by.type...video.play),
            Lifetime.Post.consumptions.by.type...photo.view=sum(Lifetime.Post.consumptions.by.type...photo.view),
            Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=sum(Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks),
            Lifetime.Negative.Feedback...hide_all_clicks=sum(Lifetime.Negative.Feedback...hide_all_clicks),
            Lifetime.Negative.Feedback...unlike_page_clicks=sum(Lifetime.Negative.Feedback...unlike_page_clicks),
            Lifetime.Negative.Feedback...report_spam_clicks=sum(Lifetime.Negative.Feedback...report_spam_clicks),
            Lifetime.Post.Organic.Reach=sum(Lifetime.Post.Organic.Reach),
            Lifetime.Post.Total.Impressions=sum(Lifetime.Post.Total.Impressions),
            Lifetime.Post.Paid.Impressions=sum(Lifetime.Post.Paid.Impressions),
            Lifetime.Post.Consumers=sum(Lifetime.Post.Consumers),
            Lifetime.Negative.Feedback=sum(Lifetime.Negative.Feedback),
            Lifetime.Post.impressions.by.people.who.have.liked.your.Page=sum(Lifetime.Post.impressions.by.people.who.have.liked.your.Page),
            Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=sum(Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page),
            Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=sum(Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post),
            Lifetime.Organic.views.to.95..1=sum(Lifetime.Organic.views.to.95..1),
            Lifetime.Paid.views.to.95..1=sum(Lifetime.Paid.views.to.95..1),
            Lifetime.Organic.Video.Views.1=sum(Lifetime.Organic.Video.Views.1),
            Lifetime.Paid.Video.Views.1=sum(Lifetime.Paid.Video.Views.1),
            Lifetime.Video.length=sum(Lifetime.Video.length),
            Lifetime.Talking.about.this..post..by.action.type...like=sum(Lifetime.Talking.about.this..post..by.action.type...like),
            Lifetime.Post.stories.by.action.type...share=sum(Lifetime.Post.stories.by.action.type...share),
            Lifetime.Post.stories.by.action.type...comment=sum(Lifetime.Post.stories.by.action.type...comment),
            Lifetime.Post.consumers.by.type...other.clicks=sum(Lifetime.Post.consumers.by.type...other.clicks),
            Lifetime.Post.consumers.by.type...link.clicks=sum(Lifetime.Post.consumers.by.type...link.clicks),
            Lifetime.Post.consumptions.by.type...other.clicks=sum(Lifetime.Post.consumptions.by.type...other.clicks),
            Lifetime.Post.consumptions.by.type...link.clicks=sum(Lifetime.Post.consumptions.by.type...link.clicks),
            Lifetime.Negative.feedback.from.users.by.type...hide_clicks=sum(Lifetime.Negative.feedback.from.users.by.type...hide_clicks),
            Lifetime.Negative.Feedback...hide_clicks=sum(Lifetime.Negative.Feedback...hide_clicks),
            Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=sum(Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks),
            Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=sum(Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks),
            Performance=sum(Performance))

names(post_reg_M)            
nrow(post_reg_M)
tail(post_reg_M)

#consistent date name, change "Posted" to "Date
names(post_reg_M)[1]<- "Date"
class(post_reg_M$Date)

#add rows for dates with no data to get the full 911 rows range that regM will be
newRow <- data.frame(Date="2016-01-03",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-01-10",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-01-17",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-01-24",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-01-31",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-02-07",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-02-14",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-02-21",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-02-28",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-03-13",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-04-03",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-04-10",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-04-17",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-04-24",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-05-01",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-05-22",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-05-29",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-06-12",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-06-26",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-07-10",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-03-20",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-08-21",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-08-28",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-10-23",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-11-06",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2016-11-13",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)
newRow <- data.frame(Date="2017-06-04",Lifetime.Post.Total.Reach=0,Lifetime.Post.Paid.Reach=0,Lifetime.Post.Organic.Impressions=0,Lifetime.Engaged.Users=0,Lifetime.Post.Consumptions=0,Lifetime.Negative.feedback.from.users=0,Lifetime.Post.reach.by.people.who.like.your.Page=0,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page=0,Lifetime.Organic.views.to.95.=0,Lifetime.Paid.views.to.95.=0,Lifetime.Organic.Video.Views=0,Lifetime.Paid.Video.Views=0,Lifetime.Average.time.video.viewed=0,Lifetime.Talking.about.this..post..by.action.type...share=0,Lifetime.Talking.about.this..post..by.action.type...comment=0,Lifetime.Post.stories.by.action.type...like=0,Lifetime.Post.consumers.by.type...video.play=0,Lifetime.Post.consumers.by.type...photo.view=0,Lifetime.Post.consumptions.by.type...video.play=0,Lifetime.Post.consumptions.by.type...photo.view=0,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks=0,Lifetime.Negative.Feedback...hide_all_clicks=0,Lifetime.Negative.Feedback...unlike_page_clicks=0,Lifetime.Negative.Feedback...report_spam_clicks=0,Lifetime.Post.Organic.Reach=0,Lifetime.Post.Total.Impressions=0,Lifetime.Post.Paid.Impressions=0,Lifetime.Post.Consumers=0,Lifetime.Negative.Feedback=0,Lifetime.Post.impressions.by.people.who.have.liked.your.Page=0,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page=0,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post=0,Lifetime.Organic.views.to.95..1=0,Lifetime.Paid.views.to.95..1=0,Lifetime.Organic.Video.Views.1=0,Lifetime.Paid.Video.Views.1=0,Lifetime.Video.length=0,Lifetime.Talking.about.this..post..by.action.type...like=0,Lifetime.Post.stories.by.action.type...share=0,Lifetime.Post.stories.by.action.type...comment=0,Lifetime.Post.consumers.by.type...other.clicks=0,Lifetime.Post.consumers.by.type...link.clicks=0,Lifetime.Post.consumptions.by.type...other.clicks=0,Lifetime.Post.consumptions.by.type...link.clicks=0,Lifetime.Negative.feedback.from.users.by.type...hide_clicks=0,Lifetime.Negative.Feedback...hide_clicks=0,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks=0,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks=0,Performance=0)
post_reg_M <- rbind(post_reg_M,newRow)

nrow(post_reg_M)

#order post_reg_M by Date
#--------------------------
post_reg_M=post_reg_M[order(as.Date(post_reg_M$Date, format="%Y/%m/%d")),]
head(post_reg_M)
tail(post_reg_M)
names(post_reg_M)
nrow(post_reg_M)

#reposition Day
post_reg_M=subset(post_reg_M,select=c(Date,Lifetime.Post.Total.Reach,Lifetime.Post.Organic.Reach,Lifetime.Post.Paid.Reach,Lifetime.Post.Total.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Post.Paid.Impressions,Lifetime.Engaged.Users,Lifetime.Post.Consumers,Lifetime.Post.Consumptions,Lifetime.Negative.Feedback,Lifetime.Negative.feedback.from.users,Lifetime.Post.impressions.by.people.who.have.liked.your.Page,Lifetime.Post.reach.by.people.who.like.your.Page,Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page,Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page,Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,Lifetime.Organic.views.to.95.,Lifetime.Organic.views.to.95..1,Lifetime.Paid.views.to.95.,Lifetime.Paid.views.to.95..1,Lifetime.Organic.Video.Views,Lifetime.Organic.Video.Views.1,Lifetime.Paid.Video.Views,Lifetime.Paid.Video.Views.1,Lifetime.Average.time.video.viewed,Lifetime.Video.length,Lifetime.Talking.about.this..post..by.action.type...share,Lifetime.Talking.about.this..post..by.action.type...like,Lifetime.Talking.about.this..post..by.action.type...comment,Lifetime.Post.stories.by.action.type...share,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.consumers.by.type...video.play,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumptions.by.type...video.play,Lifetime.Post.consumptions.by.type...other.clicks,Lifetime.Post.consumptions.by.type...photo.view,Lifetime.Post.consumptions.by.type...link.clicks,Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks,Lifetime.Negative.feedback.from.users.by.type...hide_clicks,Lifetime.Negative.Feedback...hide_all_clicks,Lifetime.Negative.Feedback...hide_clicks,Performance,Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks,Lifetime.Negative.Feedback...unlike_page_clicks,Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks,Lifetime.Negative.Feedback...report_spam_clicks))
head(post_reg_M)
nrow(post_reg_M)
class(post_reg_M)

#install.packages("openxlsx")
library(openxlsx)
write.xlsx(post_reg_M, 'post_cors.xlsx') # reclass to dataframe
post_cors=read.csv("H:/Project Dissertation/Source data/post_cors.csv", header=T)
names(post_cors)
class(post_cors)
nrow(post_cors)
#convert date from factor to date format
class(post_cors$Date);post_cors$Date=as.Date(post_cors$Date, format = "%d/%m/%Y")

#create vector from 7/7/2016 why?  To match page length?
post_cors=post_cors[189:911,]
head(post_cors)
nrow(post_cors)

#video - create vector from 7/7/2016 prepare
#381 rows to 911 dates
video_cors=video
names(video_cors)[12]<- "Date" #date name consistency
#complete missing rows
date_range <- seq(as.Date("2016-01-01"), as.Date("2018-06-29"),  # range is 911 rows
                  by = "1 day")
date_range <- data.frame(Date = date_range)

video_cors <- merge(date_range, video_cors, by = "Date", 
                 all.x = TRUE)
nrow(video_cors)
video_cors[is.na(video_cors)]<-0
#resize video metrics to by day (look at indicidual posting seoarately, need a view by day for regression model)
video_cors=subset(video_cors,select=c(Date,Video.Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,
                                      Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,
                                      Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,
                                      Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,
                                      Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,
                                      Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,
                                      Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,
                                      Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,
                                      Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,
                                      Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,
                                      Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,
                                      Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,
                                      Lifetime.Talking.About.This..Post..by.action.type...share))
video_cors=aggregate(. ~Date, data=video_cors, sum, na.rm=TRUE)

#reposition Day
video_cors=subset(video_cors,select=c(Date,Video.Lifetime.Post.Total.Reach,Lifetime.Post.Total.Impressions,Lifetime.Total.Video.Views,Lifetime.Unique.Video.Views,Lifetime.Total.30.second.Views,Lifetime.Unique.30.second.Views,Lifetime.Total.watches.at.95.,Lifetime.Unique.watches.at.95.,Lifetime.Auto.played.Video.Views,Lifetime.Clicked.to.play.Video.Views,Lifetime.Auto.played.30.second.Views,Lifetime.Clicked.to.play.30.second.Views,Lifetime.Auto.played.watches.at.95.,Lifetime.Click.to.play.watches.at.95.,Lifetime.Post.Paid.Impressions,Lifetime.Post.Organic.Impressions,Lifetime.Paid.Video.Views,Lifetime.Organic.Video.Views,Lifetime.Paid.30.second.Views,Lifetime.Organic.30.second.Views,Lifetime.Paid.watches.at.95.,Lifetime.Organic.watches.at.95.,Lifetime.Post.Consumptions.by.type...clicks.to.play,Lifetime.Post.Consumptions.by.type...link.clicks,Lifetime.Post.Consumptions.by.type...other.clicks,Lifetime.Post.Consumptions.by.type...photo.view,Lifetime.Post.consumers.by.type...clicks.to.play,Lifetime.Post.consumers.by.type...link.clicks,Lifetime.Post.consumers.by.type...other.clicks,Lifetime.Post.consumers.by.type...photo.view,Lifetime.Post.stories.by.action.type...comment,Lifetime.Post.stories.by.action.type...like,Lifetime.Post.stories.by.action.type...share,Lifetime.Talking.About.This..Post..by.action.type...comment,Lifetime.Talking.About.This..Post..by.action.type...like,Lifetime.Talking.About.This..Post..by.action.type...share))
video_cors=video_cors[189:911,] #7/7/2016 --->
nrow(video_cors)
class(video_cors)
head(video_cors)

#followers 7/7/2016 --->
names(followers)
head(followers)
followers_cors=subset(followers,select=c(Date,Total.Followers,Daily.New.Followers))
followers_cors=followers_cors[189:911,]
nrow(followers_cors)
class(followers_cors)
head(followers_cors)
names(followers_cors)

#Ticket_Sales 7/7/2016 --->
names(Original_Ticket_Sales)
head(Original_Ticket_Sales)
Ticket_Sales_cors=Original_Ticket_Sales[189:911,]
nrow(Ticket_Sales_cors)
class(Ticket_Sales_cors)
head(Ticket_Sales_cors)
names(Ticket_Sales_cors)

nrow(page_cors); nrow(post_cors); nrow(video_cors); nrow(followers_cors); nrow(Ticket_Sales_cors)
class(page_cors); class(post_cors); class(video_cors); class(followers_cors); class(Ticket_Sales_cors)
names(page_cors); names(post_cors); names(video_cors); names(followers_cors); names(Ticket_Sales_cors)

#merge dfs on Date to create consolidated df of variables for regression model; video_cors ;page_cors ;post_cors; Followers_cors
#install.packages("data.table")
library(data.table)
class(post_cors$Date)

x=setDT(page_cors); setDT(post_cors)
x=merge(page_cors,post_cors, by="Date",allow.cartesian=T)
cors=x
x=setDT(cors); setDT(video_cors)
x=merge(cors,video_cors, by="Date",allow.cartesian=T)
cors=x
x=setDT(cors); setDT(followers_cors)
x=merge(cors,followers_cors, by="Date",allow.cartesian=T)
cors=x
x=setDT(cors); setDT(Ticket_Sales_cors)
x=merge(cors,Ticket_Sales_cors, by="Date",allow.cartesian=T)
cors=x

class(cors) # this is an issue. needs resolved "data.table" "data.frame"
nrow(cors)
ncol(cors)
names(cors)
head(cors)
tail(cors)

#export to remove duplicate dataframe status
#install.packages("openxlsx")
library(openxlsx)
write.xlsx(cors, 'cors.xlsx')  #exports to H:\Documents

#reload
cors=read.csv("H:/Project Dissertation/Source data/cors.csv", header=T)
class(cors) #corrected
nrow(cors)
ncol(cors)
names(cors)
head(cors$Date) #07/7/2016
tail(cors$Date) #29/06/2018

#update CORS names to identify data source
names(page_cors); names(post_cors); names(video_cors); names(followers_cors); names(Ticket_Sales_cors); names(cors)

#update cors names
names(cors)[3]="page_Lifetime.Total.Likes"
names(cors)[4]="page_Daily.New.Likes"
names(cors)[5]="page_Daily.Net.Likes"
names(cors)[6]="page_Daily.Auto.played.30.second.Views"
names(cors)[7]="page_Daily.Logged.in.Page.Views"
names(cors)[8]="page_Daily.Negative.Feedback"
names(cors)[9]="page_Daily.Organic.Impressions"
names(cors)[10]="page_Daily.Organic.impressions.of.your.posts"
names(cors)[11]="page_Daily.Organic.Reach"
names(cors)[12]="page_Daily.Organic.reach.of.Page.posts"
names(cors)[13]="page_Daily.Page.Consumptions"
names(cors)[14]="page_Daily.Page.Engaged.Users"
names(cors)[15]="page_Daily.Paid.30.second.Views"
names(cors)[16]="page_Daily.Paid.Impressions"
names(cors)[17]="page_Daily.Paid.impressions.of.your.posts"
names(cors)[18]="page_Daily.Paid.Reach"
names(cors)[19]="page_Daily.Paid.reach.of.Page.posts"
names(cors)[20]="page_Daily.Reach.of.Page.posts"
names(cors)[21]="page_Daily.Total.30.Second.Repeats"
names(cors)[22]="page_Daily.Total.30.second.Views"
names(cors)[23]="page_Daily.Total.Auto.Played.Views"
names(cors)[24]="page_Daily.Total.Check.ins"
names(cors)[25]="page_Daily.Total.check.ins.using.mobile.devices"
names(cors)[26]="page_Daily.Total.Clicked.30.Second.Views"
names(cors)[27]="page_Daily.Total.Clicked.Views"
names(cors)[28]="page_Daily.Total.Consumers"
names(cors)[29]="page_Daily.Total.Impressions"
names(cors)[30]="page_Daily.Total.Impressions.of.your.posts"
names(cors)[31]="page_Daily.Total.Organic.30.Second.Views"
names(cors)[32]="page_Daily.Total.Organic.Views"
names(cors)[33]="page_Daily.Total.phone.calls.click.count.per.Page"
names(cors)[34]="page_Daily.Total.Promoted.Views"
names(cors)[35]="page_Daily.Total.Reach"
names(cors)[36]="page_Daily.Total.Unique.30.second.Views"
names(cors)[37]="page_Daily.Total.Unique.Video.Views"
names(cors)[38]="page_Daily.Total.Video.Views"
names(cors)[39]="page_Daily.Total.website.click.count.per.Page"
names(cors)[40]="page_Daily.total..Total.action.count.per.Page"
names(cors)[41]="page_Daily.Unlikes"
names(cors)[42]="page_Daily.Video.Repeats"
names(cors)[43]="page_Daily.Viral.Impressions"
names(cors)[44]="page_Daily.Viral.impressions.of.your.posts"
names(cors)[45]="page_Daily.Viral.Reach"
names(cors)[46]="page_Daily.Viral.reach.of.Page.posts"
names(cors)[47]="page_Daily.Total.get.direction.click.count.per.Page"
names(cors)[48]="post_Lifetime.Post.Total.Reach"
names(cors)[49]="post_Lifetime.Post.Organic.Reach"
names(cors)[50]="post_Lifetime.Post.Paid.Reach"
names(cors)[51]="post_Lifetime.Post.Total.Impressions.x"
names(cors)[52]="post_Lifetime.Post.Organic.Impressions.x"
names(cors)[53]="post_Lifetime.Post.Paid.Impressions.x"
names(cors)[54]="post_Lifetime.Engaged.Users"
names(cors)[55]="post_Lifetime.Post.Consumers"
names(cors)[56]="post_Lifetime.Post.Consumptions"
names(cors)[57]="post_Lifetime.Negative.Feedback"
names(cors)[58]="post_Lifetime.Negative.feedback.from.users"
names(cors)[59]="post_Lifetime.Post.impressions.by.people.who.have.liked.your.Page"
names(cors)[60]="post_Lifetime.Post.reach.by.people.who.like.your.Page"
names(cors)[61]="post_Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page"
names(cors)[62]="post_Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page"
names(cors)[63]="post_Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post"
names(cors)[64]="post_Lifetime.Organic.views.to.95."
names(cors)[65]="post_Lifetime.Organic.views.to.95..1"
names(cors)[66]="post_Lifetime.Paid.views.to.95."
names(cors)[67]="post_Lifetime.Paid.views.to.95..1"
names(cors)[68]="post_Lifetime.Organic.Video.Views.x"
names(cors)[69]="post_Lifetime.Organic.Video.Views.1"
names(cors)[70]="post_Lifetime.Paid.Video.Views.x"
names(cors)[71]="post_Lifetime.Paid.Video.Views.1"
names(cors)[72]="post_Lifetime.Average.time.video.viewed"
names(cors)[73]="post_Lifetime.Video.length"
names(cors)[74]="post_Lifetime.Talking.about.this..post..by.action.type...share"
names(cors)[75]="post_Lifetime.Talking.about.this..post..by.action.type...like"
names(cors)[76]="post_Lifetime.Talking.about.this..post..by.action.type...comment"
names(cors)[77]="post_Lifetime.Post.stories.by.action.type...share.x"
names(cors)[78]="post_Lifetime.Post.stories.by.action.type...like.x"
names(cors)[79]="post_Lifetime.Post.stories.by.action.type...comment.x"
names(cors)[80]="post_Lifetime.Post.consumers.by.type...video.play"
names(cors)[81]="post_Lifetime.Post.consumers.by.type...other.clicks.x"
names(cors)[82]="post_Lifetime.Post.consumers.by.type...photo.view.x"
names(cors)[83]="post_Lifetime.Post.consumers.by.type...link.clicks.x"
names(cors)[84]="post_Lifetime.Post.consumptions.by.type...video.play"
names(cors)[85]="post_Lifetime.Post.consumptions.by.type...other.clicks"
names(cors)[86]="post_Lifetime.Post.consumptions.by.type...photo.view"
names(cors)[87]="post_Lifetime.Post.consumptions.by.type...link.clicks"
names(cors)[88]="post_Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks"
names(cors)[89]="post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks"
names(cors)[90]="post_Lifetime.Negative.Feedback...hide_all_clicks"
names(cors)[91]="post_Lifetime.Negative.Feedback...hide_clicks"
names(cors)[92]="post_Performance"
names(cors)[93]="post_Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks"
names(cors)[94]="post_Lifetime.Negative.Feedback...unlike_page_clicks"
names(cors)[95]="post_Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks"
names(cors)[96]="post_Lifetime.Negative.Feedback...report_spam_clicks"
names(cors)[97]="video_Video.Lifetime.Post.Total.Reach"
names(cors)[98]="video_Lifetime.Post.Total.Impressions.y"
names(cors)[99]="video_Lifetime.Total.Video.Views"
names(cors)[100]="video_Lifetime.Unique.Video.Views"
names(cors)[101]="video_Lifetime.Total.30.second.Views"
names(cors)[102]="video_Lifetime.Unique.30.second.Views"
names(cors)[103]="video_Lifetime.Total.watches.at.95."
names(cors)[104]="video_Lifetime.Unique.watches.at.95."
names(cors)[105]="video_Lifetime.Auto.played.Video.Views"
names(cors)[106]="video_Lifetime.Clicked.to.play.Video.Views"
names(cors)[107]="video_Lifetime.Auto.played.30.second.Views"
names(cors)[108]="video_Lifetime.Clicked.to.play.30.second.Views"
names(cors)[109]="video_Lifetime.Auto.played.watches.at.95."
names(cors)[110]="video_Lifetime.Click.to.play.watches.at.95."
names(cors)[111]="video_Lifetime.Post.Paid.Impressions.y"
names(cors)[112]="video_Lifetime.Post.Organic.Impressions.y"
names(cors)[113]="video_Lifetime.Paid.Video.Views.y"
names(cors)[114]="video_Lifetime.Organic.Video.Views.y"
names(cors)[115]="video_Lifetime.Paid.30.second.Views"
names(cors)[116]="video_Lifetime.Organic.30.second.Views"
names(cors)[117]="video_Lifetime.Paid.watches.at.95."
names(cors)[118]="video_Lifetime.Organic.watches.at.95."
names(cors)[119]="video_Lifetime.Post.Consumptions.by.type...clicks.to.play"
names(cors)[120]="video_Lifetime.Post.Consumptions.by.type...link.clicks"
names(cors)[121]="video_Lifetime.Post.Consumptions.by.type...other.clicks"
names(cors)[122]="video_Lifetime.Post.Consumptions.by.type...photo.view"
names(cors)[123]="video_Lifetime.Post.consumers.by.type...clicks.to.play"
names(cors)[124]="video_Lifetime.Post.consumers.by.type...link.clicks.y"
names(cors)[125]="video_Lifetime.Post.consumers.by.type...other.clicks.y"
names(cors)[126]="video_Lifetime.Post.consumers.by.type...photo.view.y"
names(cors)[127]="video_Lifetime.Post.stories.by.action.type...comment.y"
names(cors)[128]="video_Lifetime.Post.stories.by.action.type...like.y"
names(cors)[129]="video_Lifetime.Post.stories.by.action.type...share.y"
names(cors)[130]="video_Lifetime.Talking.About.This..Post..by.action.type...comment"
names(cors)[131]="video_Lifetime.Talking.About.This..Post..by.action.type...like"
names(cors)[132]="video_Lifetime.Talking.About.This..Post..by.action.type...share"
names(cors)[133]="foll_Total.Followers"
names(cors)[134]="foll_Daily.New.Followers"
names(cors)[135]="sales_X1Call.Direct"
names(cors)[136]="sales_Abandoned.Web.Leads"
names(cors)[137]="sales_Charity.Web"
names(cors)[138]="sales_Desktop.PPC"
names(cors)[139]="sales_Direct.Mail"
names(cors)[140]="sales_Display.Ads"
names(cors)[141]="sales_Door.Drop"
names(cors)[142]="sales_DRTV"
names(cors)[143]="sales_Email.Newsletter"
names(cors)[144]="sales_Email.Outbound"
names(cors)[145]="sales_Failed.Transactions"
names(cors)[146]="sales_Internal.TM"
names(cors)[147]="sales_Maggies"
names(cors)[148]="sales_Missing.People"
names(cors)[149]="sales_Mobile.App"
names(cors)[150]="sales_Mobile.Generic"
names(cors)[151]="sales_Mobile.PPC"
names(cors)[152]="sales_MS_Email_Outbound"
names(cors)[153]="sales_Other.Web.Gen"
names(cors)[154]="sales_PPL.Email.Outbound"
names(cors)[155]="sales_PPL.Postal.Outbound"
names(cors)[156]="sales_Press"
names(cors)[157]="sales_Social.Media"
names(cors)[158]="sales_Summer.Millions.Mobile"
names(cors)[159]="sales_Test"
names(cors)[160]="sales_Website.Gen"
names(cors)[161]="sales_Weekly.Draw.Results"
names(cors)[162]="sales_Old.Inbound"
names(cors)[163]="sales_Total"

names(cors)
dim(cors)
head(cors)

#truncate to start from 21/12/2016 as adv data only availabke from then
Original_cors=cors
cors=cors[168:723,]

head(adv_vols$Date); tail(adv_vols$Date)
head(cors$Date); tail(cors$Date)
length(adv_vols$Date); length(cors$Date)

#add advertsisng data to consolidated dataframe; need to resize CORS from start of 07/07/2016 as data only from 21/12/2016
cors$Adv_Total.Volumes=adv_vols_cors$adv_vols.Total
cors$Adv_Social.Media.Volumes=adv_vols_cors$adv_vols.Social.Media
cors$Adv_Total.cost=adv_cost_cors$adv_cost.Total
cors$Adv_Social.Media.cost=adv_cost_cors$adv_cost.Social.Media
names(cors)

#repos y vars - Total and Social Media sales kept as both of interest as y var, main focus is on social media sales though
cors_1=subset(cors,select=c(Date,sales_Social.Media,
                            sales_Total,
                            foll_Total.Followers,
                            foll_Daily.New.Followers,
                            Adv_Total.Volumes,
                            Adv_Social.Media.Volumes,
                            Adv_Total.cost,
                            Adv_Social.Media.cost,
                            page_Lifetime.Total.Likes,
                            page_Daily.New.Likes,
                            page_Daily.Auto.played.30.second.Views,
                            page_Daily.Logged.in.Page.Views,
                            page_Daily.Negative.Feedback,
                            page_Daily.Organic.Impressions,
                            page_Daily.Organic.impressions.of.your.posts,
                            page_Daily.Organic.Reach,
                            page_Daily.Organic.reach.of.Page.posts,
                            page_Daily.Page.Consumptions,
                            page_Daily.Page.Engaged.Users,
                            page_Daily.Paid.30.second.Views,
                            page_Daily.Paid.Impressions,
                            page_Daily.Paid.impressions.of.your.posts,
                            page_Daily.Paid.Reach,
                            page_Daily.Paid.reach.of.Page.posts,
                            page_Daily.Reach.of.Page.posts,
                            page_Daily.Total.30.Second.Repeats,                                      
                            page_Daily.Total.30.second.Views,                                        
                            page_Daily.Total.Auto.Played.Views,                                      
                            page_Daily.Total.Check.ins,                                              
                            #from analysis 1: page_Daily.Total.check.ins.using.mobile.devices,                         
                            page_Daily.Total.Clicked.30.Second.Views,                                
                            page_Daily.Total.Clicked.Views,                                          
                            page_Daily.Total.Consumers,                                              
                            page_Daily.Total.Impressions,                                            
                            page_Daily.Total.Impressions.of.your.posts,                              
                            page_Daily.Total.Organic.30.Second.Views,                                
                            page_Daily.Total.Organic.Views,                                          
                            #page_Daily.Total.phone.calls.click.count.per.Page,                       
                            page_Daily.Total.Promoted.Views,                                         
                            page_Daily.Total.Reach,                                                  
                            page_Daily.Total.Unique.30.second.Views,                                 
                            page_Daily.Total.Unique.Video.Views,                                     
                            page_Daily.Total.Video.Views,                                            
                            page_Daily.Total.website.click.count.per.Page,                           
                            page_Daily.total..Total.action.count.per.Page,                           
                            page_Daily.Unlikes,                                                      
                            page_Daily.Video.Repeats,                                                
                            page_Daily.Viral.Impressions,                                            
                            page_Daily.Viral.impressions.of.your.posts,                              
                            page_Daily.Viral.Reach,                                                  
                            page_Daily.Viral.reach.of.Page.posts,                                    
                            page_Daily.Total.get.direction.click.count.per.Page,                     
                            post_Lifetime.Post.Total.Reach,                                          
                            post_Lifetime.Post.Organic.Reach,                                        
                            post_Lifetime.Post.Paid.Reach,                                           
                            post_Lifetime.Post.Total.Impressions.x,                                  
                            post_Lifetime.Post.Organic.Impressions.x,                                
                            post_Lifetime.Post.Paid.Impressions.x,                                   
                            post_Lifetime.Engaged.Users,                                             
                            post_Lifetime.Post.Consumers,                                            
                            post_Lifetime.Post.Consumptions,                                         
                            post_Lifetime.Negative.Feedback,                                         
                            post_Lifetime.Negative.feedback.from.users,                              
                            post_Lifetime.Post.impressions.by.people.who.have.liked.your.Page,       
                            post_Lifetime.Post.reach.by.people.who.like.your.Page,                   
                            post_Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page,  
                            post_Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page,         
                            post_Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,
                            post_Lifetime.Organic.views.to.95.,                                      
                            post_Lifetime.Organic.views.to.95..1,                                    
                            post_Lifetime.Paid.views.to.95.,                                         
                            post_Lifetime.Paid.views.to.95..1,                                       
                            post_Lifetime.Organic.Video.Views.x,                                     
                            post_Lifetime.Organic.Video.Views.1,                                     
                            post_Lifetime.Paid.Video.Views.x,                                        
                            post_Lifetime.Paid.Video.Views.1,                                        
                            post_Lifetime.Average.time.video.viewed,                                 
                            post_Lifetime.Video.length,                                              
                            post_Lifetime.Talking.about.this..post..by.action.type...share,          
                            post_Lifetime.Talking.about.this..post..by.action.type...like,           
                            post_Lifetime.Talking.about.this..post..by.action.type...comment,        
                            post_Lifetime.Post.stories.by.action.type...share.x,                     
                            post_Lifetime.Post.stories.by.action.type...like.x,                      
                            post_Lifetime.Post.stories.by.action.type...comment.x,                   
                            post_Lifetime.Post.consumers.by.type...video.play,                       
                            post_Lifetime.Post.consumers.by.type...other.clicks.x,                   
                            post_Lifetime.Post.consumers.by.type...photo.view.x,                     
                            post_Lifetime.Post.consumers.by.type...link.clicks.x,                    
                            post_Lifetime.Post.consumptions.by.type...video.play,                    
                            post_Lifetime.Post.consumptions.by.type...other.clicks,                  
                            post_Lifetime.Post.consumptions.by.type...photo.view,                    
                            post_Lifetime.Post.consumptions.by.type...link.clicks,                   
                            post_Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks,    
                            post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks,        
                            post_Lifetime.Negative.Feedback...hide_all_clicks,                       
                            post_Lifetime.Negative.Feedback...hide_clicks,                           
                            #post_Performance,                                                        
                            #post_Lifetime.Negative.feedback.from.users.by.type...unlike_page_clicks, 
                            #post_Lifetime.Negative.Feedback...unlike_page_clicks,                    
                            post_Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks, 
                            post_Lifetime.Negative.Feedback...report_spam_clicks,                    
                            video_Video.Lifetime.Post.Total.Reach,                                   
                            video_Lifetime.Post.Total.Impressions.y,                                 
                            video_Lifetime.Total.Video.Views,                                        
                            video_Lifetime.Unique.Video.Views,                                       
                            video_Lifetime.Total.30.second.Views,                                    
                            video_Lifetime.Unique.30.second.Views,                                   
                            video_Lifetime.Total.watches.at.95.,                                     
                            video_Lifetime.Unique.watches.at.95.,                                    
                            video_Lifetime.Auto.played.Video.Views,                                  
                            video_Lifetime.Clicked.to.play.Video.Views,                              
                            video_Lifetime.Auto.played.30.second.Views,                              
                            video_Lifetime.Clicked.to.play.30.second.Views,                          
                            video_Lifetime.Auto.played.watches.at.95.,                               
                            video_Lifetime.Click.to.play.watches.at.95.,                             
                            video_Lifetime.Post.Paid.Impressions.y,                                  
                            video_Lifetime.Post.Organic.Impressions.y,                               
                            video_Lifetime.Paid.Video.Views.y,                                       
                            video_Lifetime.Organic.Video.Views.y,                                    
                            video_Lifetime.Paid.30.second.Views,                                     
                            video_Lifetime.Organic.30.second.Views,                                  
                            video_Lifetime.Paid.watches.at.95.,                                      
                            video_Lifetime.Organic.watches.at.95.,                                   
                            video_Lifetime.Post.Consumptions.by.type...clicks.to.play,               
                            video_Lifetime.Post.Consumptions.by.type...link.clicks,                  
                            video_Lifetime.Post.Consumptions.by.type...other.clicks,                 
                            video_Lifetime.Post.Consumptions.by.type...photo.view,                   
                            video_Lifetime.Post.consumers.by.type...clicks.to.play,                  
                            video_Lifetime.Post.consumers.by.type...link.clicks.y,                   
                            video_Lifetime.Post.consumers.by.type...other.clicks.y,                  
                            video_Lifetime.Post.consumers.by.type...photo.view.y,                    
                            video_Lifetime.Post.stories.by.action.type...comment.y,                  
                            video_Lifetime.Post.stories.by.action.type...like.y,                     
                            video_Lifetime.Post.stories.by.action.type...share.y,                    
                            video_Lifetime.Talking.About.This..Post..by.action.type...comment,       
                            video_Lifetime.Talking.About.This..Post..by.action.type...like,          
                            video_Lifetime.Talking.About.This..Post..by.action.type...share,
                            #sales_X1Call.Direct,                                                     
                            sales_Abandoned.Web.Leads,                                               
                            sales_Charity.Web,                                                       
                            sales_Desktop.PPC,                                                       
                            sales_Direct.Mail,                                                       
                            sales_Display.Ads,                                                       
                            sales_Door.Drop,                                                         
                            sales_DRTV,                                                              
                            sales_Email.Newsletter,                                                  
                            sales_Email.Outbound,                                                    
                            sales_Failed.Transactions,                                               
                            sales_Internal.TM,                                                       
                            #from analysis 1: sales_Maggies,                                                           
                            #from analysis 1:sales_Missing.People,                                                    
                            #sales_Mobile.App,                                                        
                            #from analysis 1:sales_Mobile.Generic,                                                    
                            #sales_Mobile.PPC,                                                        
                            #sales_MS_Email_Outbound,                                                 
                            sales_Other.Web.Gen,                                                     
                            sales_PPL.Email.Outbound,                                                
                            sales_PPL.Postal.Outbound,                                               
                            sales_Press,                                          
                            #from analysis 1:sales_Summer.Millions.Mobile,                             
                            sales_Website.Gen,                                                       
                            sales_Weekly.Draw.Results,                                               
                            #sales_Old.Inbound
                            sales_Test
                            ))

dim(cors_1)
class(cors_1)
names(cors_1)
is.na(cors_1)
str(cors_1)
class(cors_1$Date)
cors_1$Date=as.Date(cors_1$Date, format = "%d/%m/%Y")
head(cors_1$Date); tail(cors_1$Date)

library(openxlsx)
write.xlsx(cors_1, 'cors_1.xlsx')

#split data (pareto 80 - 20) for regM, train / test and cut other sales data from cors in case it leads the model learning
#-------------------------------------------------
x=nrow(cors_1)*4/5;train_cors_1=cors_1$Date[x];cors_1$Date[x] #pareto split 
train_cors_1=subset(cors_1,Date<="2018-03-09",select=c(0,1:133)) #133 as exclude sales vars from column 134 ----->
test_cors_1=subset(cors_1,Date>"2018-03-09",select=c(0,1:133))
nrow(cors_1); nrow(train_cors_1); nrow(test_cors_1); nrow(cors_1)-(nrow(train_cors_1)+nrow(test_cors_1))
head(train_cors_1$Date)
tail(train_cors_1$Date)
head(test_cors_1$Date)
tail(test_cors_1$Date)

names(cors_1)
names(train_cors_1) # sales cut expect y1 y2
names(test_cors_1)
dim(train_cors_1); dim(test_cors_1)

#look at cors in consolidated data frame; is regression feasible?  And multicolinearity check
cormat_1=train_cors_1[,c(1,2:133)] #fix empty data
names(cormat_1)
cormatrix_1=cor(cormat_1[,-c(1,134)])
tail(cormatrix_1)
install.packages("corrplot")
library(corrplot)
#corrplot(cormatrix_1, method="number")

#install.packages("openxlsx")
library(openxlsx)
write.xlsx(cormatrix_1, 'cormatrix_1.xlsx')
write.xlsx(train_cors_1, 'cors_1.xlsx')

#make same row size as cors 7/7/16 - 29/6/2018 and transform all
train_cors_2=train_cors_1;nrow(train_cors_2)
test_cors_2=test_cors_1;nrow(test_cors_2)
head(train_cors_2$Date);tail(train_cors_2$Date)
head(test_cors_2$Date);tail(test_cors_2$Date)

dim(train_cors_2); dim(train_cors_1)
train_cors_2[, 3:133] <- log(train_cors_2[3:133]) #log all xvars, exclude y1 
train_cors_2[mapply(is.infinite, train_cors_2)] <- 0
train_cors_2[mapply(is.nan, train_cors_2)] <- 0
test_cors_2[, 3:133] <- log(test_cors_2[3:133]) #log all xvars, exclude y1 
test_cors_2[mapply(is.infinite, test_cors_2)] <- 0
test_cors_2[mapply(is.nan, test_cors_2)] <- 0

#multicolinearity check
cormat_2=train_cors_2[,c(1,2:133)] #fix empty data
cormatrix_2=cor(cormat_2[,-c(1,134)])

#install.packages("openxlsx")
library(openxlsx)
write.xlsx(cormatrix_2, 'cormatrix_2.xlsx')
write.xlsx(train_cors_2, 'cors_2.xlsx')
names(train_cors_2)

#cors_renamed to regM and non y1 y2 sales vars excluded  
regM=subset(train_cors_1,select=c(0,1:133)) # sales vars excluded (toggle train_cors_1 / 2 for non transformed vars)
names(regM)
length(names(regM)); dim(regM)
regM_loop_length=length(names(regM))-2 
cor.coeffs.regM<-vector("numeric",length=regM_loop_length)
# run a loop and populate corr.coeffs with correlation coefficients
for(i in 1:regM_loop_length){
  cor.coeffs.regM[i]<-cor(regM$sales_Social.Media, regM[, i+2])
};cor.coeffs.regM[is.na(cor.coeffs.regM)] <- 0

#test to see if loop in correct order
cor(regM$sales_Social.Media,regM$sales_Total) # ok

#add corr with +0.7 and visualise
Main_cor.coeffs.regM=cor.coeffs.regM[cor.coeffs.regM>=0.4]

#add names
names(cor.coeffs.regM)<-names(regM)[3:ncol(regM)]

cor(regM$foll_Total.Followers, regM$page_Lifetime.Total.Likes)
cor(regM$page_Daily.New.Likes, regM$foll_Daily.New.Followers)

#add new likes as baseline = 1 +-1 for graph
cor.coeffs.regM=round(cor.coeffs.regM,digits=2)
cor.coeffs.regM=append(cor.coeffs.regM, 1.00,0)
#cor.coeffs.regM=append(cor.coeffs.regM, -1.00,0)
names(cor.coeffs.regM)[1]<-"Sales_Social.Media"
#names(cor.coeffs.regM)[2]<-"Sales_Social.Media"
cor.coeffs.regM

par(mfrow=c(1,1))
#plot regM
#=========
ylim <- c(-0.2, 1.05)
## Plot, and store x-coordinates of bars in xx
xx <- barplot(cor.coeffs.regM, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim,
              main = "SOCIAL MEDIA SALES CORRELATION COEFFICIENTS (Training Set)",x.main=10.0, 
              ylab = "Correlation Strength & Direction",col=c("yellow",rep("dodgerblue",156)),border="black")
## Add text at top of bars
text(x = xx, y = cor.coeffs.regM, label = cor.coeffs.regM,pos = 3, cex = 0.6, col = "red")
## Add x-axis labels 
axis(1, at=xx, labels=names(cor.coeffs.regM), tick=FALSE, las=2, line=-0.5, cex.axis=0.9)
abline(h = 0.4, col = "darkorange",lwd=2,lty=3)
abline(h = -0.4, col = "darkorange",lwd=2,lty=3)
abline(h = 0.0, col = "black",lwd=2,lty=1)

#install.packages("dplyr")
library(dplyr)
#regM coeffs rank
names(cor.coeffs.regM)
length(cor.coeffs.regM)
cor.coeffs.regM=abs(cor.coeffs.regM)
ranktable_names<-names(cor.coeffs.regM)
#cor.coeffs.regM=lapply(cor.coeffs.regM,abs);cor.coeffs.regM=as.numeric(cor.coeffs.regM)
ranktable_coeffs=cor.coeffs.regM
ranktable <- data.frame(ranktable_names, ranktable_coeffs)
ranktable <- arrange(ranktable, desc(ranktable_coeffs)) %>%
  mutate(rank = 1:nrow(ranktable))
ranktable
write.xlsx(ranktable, 'train_cors_1_ranktable.xlsx')  #exports to H:\Documents
#excel graph from transformation analysis file

#BUILD LINEAR MODELS
#------------------------------------------------------------------------------------------------------------------------------------------
cor(train_cors_1$sales_Social.Media,train_cors_1$sales_Total)
cor(test_cors_1$sales_Social.Media,test_cors_1$sales_Total)

cor(train_cors_2$sales_Social.Media,train_cors_2$sales_Total)
cor(test_cors_2$sales_Social.Media,test_cors_2$sales_Total)
names(train_cors_1)

#model 1 all variables as baseline - toggle between 'data = train_cors_1 (original data) and train_cors_2 (transformed data)
#---------------------------------------------------------------------------------------------------------------------------
model_1=lm(sales_Social.Media~sales_Total+
             foll_Total.Followers+
             foll_Daily.New.Followers+
             Adv_Social.Media.cost+
             Adv_Total.Volumes+
             Adv_Social.Media.Volumes+
             Adv_Total.cost+
             page_Lifetime.Total.Likes+
             page_Daily.New.Likes+
             page_Daily.Auto.played.30.second.Views+
             page_Daily.Logged.in.Page.Views+
             page_Daily.Negative.Feedback+
             page_Daily.Organic.Impressions+
             page_Daily.Organic.impressions.of.your.posts+
             page_Daily.Organic.Reach+
             page_Daily.Organic.reach.of.Page.posts+
             page_Daily.Page.Consumptions+
             page_Daily.Page.Engaged.Users+
             page_Daily.Paid.30.second.Views+
             page_Daily.Paid.Impressions+
             page_Daily.Paid.impressions.of.your.posts+
             page_Daily.Paid.Reach+
             page_Daily.Paid.reach.of.Page.posts+
             page_Daily.Reach.of.Page.posts+
             page_Daily.Total.30.Second.Repeats+
             page_Daily.Total.30.second.Views+
             page_Daily.Total.Auto.Played.Views+
             page_Daily.Total.Check.ins+
             page_Daily.Total.Clicked.30.Second.Views+
             page_Daily.Total.Clicked.Views+
             page_Daily.Total.Consumers+
             page_Daily.Total.Impressions+
             page_Daily.Total.Impressions.of.your.posts+
             page_Daily.Total.Organic.30.Second.Views+
             page_Daily.Total.Organic.Views+
             page_Daily.Total.Promoted.Views+
             page_Daily.Total.Reach+
             page_Daily.Total.Unique.30.second.Views+
             page_Daily.Total.Unique.Video.Views+
             page_Daily.Total.Video.Views+
             page_Daily.Total.website.click.count.per.Page+
             page_Daily.total..Total.action.count.per.Page+
             page_Daily.Unlikes+
             page_Daily.Video.Repeats+
             page_Daily.Viral.Impressions+
             page_Daily.Viral.impressions.of.your.posts+
             page_Daily.Viral.Reach+
             page_Daily.Viral.reach.of.Page.posts+
             page_Daily.Total.get.direction.click.count.per.Page+
             post_Lifetime.Post.Total.Reach+
             post_Lifetime.Post.Organic.Reach+
             post_Lifetime.Post.Paid.Reach+
             post_Lifetime.Post.Total.Impressions.x+
             post_Lifetime.Post.Organic.Impressions.x+
             post_Lifetime.Post.Paid.Impressions.x+
             post_Lifetime.Engaged.Users+
             post_Lifetime.Post.Consumers+
             post_Lifetime.Post.Consumptions+
             post_Lifetime.Negative.Feedback+
             post_Lifetime.Negative.feedback.from.users+
             post_Lifetime.Post.impressions.by.people.who.have.liked.your.Page+
             post_Lifetime.Post.reach.by.people.who.like.your.Page+
             post_Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page+
             post_Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page+
             post_Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post+
             post_Lifetime.Organic.views.to.95.+
             post_Lifetime.Organic.views.to.95..1+
             post_Lifetime.Paid.views.to.95.+
             post_Lifetime.Paid.views.to.95..1+
             post_Lifetime.Organic.Video.Views.x+
             post_Lifetime.Organic.Video.Views.1+
             post_Lifetime.Paid.Video.Views.x+
             post_Lifetime.Paid.Video.Views.1+
             post_Lifetime.Average.time.video.viewed+
             post_Lifetime.Video.length+
             post_Lifetime.Talking.about.this..post..by.action.type...share+
             post_Lifetime.Talking.about.this..post..by.action.type...like+
             post_Lifetime.Talking.about.this..post..by.action.type...comment+
             post_Lifetime.Post.stories.by.action.type...share.x+
             post_Lifetime.Post.stories.by.action.type...like.x+
             post_Lifetime.Post.stories.by.action.type...comment.x+
             post_Lifetime.Post.consumers.by.type...video.play+
             post_Lifetime.Post.consumers.by.type...other.clicks.x+
             post_Lifetime.Post.consumers.by.type...photo.view.x+
             post_Lifetime.Post.consumers.by.type...link.clicks.x+
             post_Lifetime.Post.consumptions.by.type...video.play+
             post_Lifetime.Post.consumptions.by.type...other.clicks+
             post_Lifetime.Post.consumptions.by.type...photo.view+
             post_Lifetime.Post.consumptions.by.type...link.clicks+
             post_Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks+
             post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks+
             post_Lifetime.Negative.Feedback...hide_all_clicks+
             post_Lifetime.Negative.Feedback...hide_clicks+
             post_Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks+
             post_Lifetime.Negative.Feedback...report_spam_clicks+
             video_Video.Lifetime.Post.Total.Reach+
             video_Lifetime.Post.Total.Impressions.y+
             video_Lifetime.Total.Video.Views+
             video_Lifetime.Unique.Video.Views+
             video_Lifetime.Total.30.second.Views+
             video_Lifetime.Unique.30.second.Views+
             video_Lifetime.Total.watches.at.95.+
             video_Lifetime.Unique.watches.at.95.+
             video_Lifetime.Auto.played.Video.Views+
             video_Lifetime.Clicked.to.play.Video.Views+
             video_Lifetime.Auto.played.30.second.Views+
             video_Lifetime.Clicked.to.play.30.second.Views+
             video_Lifetime.Auto.played.watches.at.95.+
             video_Lifetime.Click.to.play.watches.at.95.+
             video_Lifetime.Post.Paid.Impressions.y+
             video_Lifetime.Post.Organic.Impressions.y+
             video_Lifetime.Paid.Video.Views.y+
             video_Lifetime.Organic.Video.Views.y+
             video_Lifetime.Paid.30.second.Views+
             video_Lifetime.Organic.30.second.Views+
             video_Lifetime.Paid.watches.at.95.+
             video_Lifetime.Organic.watches.at.95.+
             video_Lifetime.Post.Consumptions.by.type...clicks.to.play+
             video_Lifetime.Post.Consumptions.by.type...link.clicks+
             video_Lifetime.Post.Consumptions.by.type...other.clicks+
             video_Lifetime.Post.Consumptions.by.type...photo.view+
             video_Lifetime.Post.consumers.by.type...clicks.to.play+
             video_Lifetime.Post.consumers.by.type...link.clicks.y+
             video_Lifetime.Post.consumers.by.type...other.clicks.y+
             video_Lifetime.Post.consumers.by.type...photo.view.y+
             video_Lifetime.Post.stories.by.action.type...comment.y+
             video_Lifetime.Post.stories.by.action.type...like.y+
             video_Lifetime.Post.stories.by.action.type...share.y+
             video_Lifetime.Talking.About.This..Post..by.action.type...comment+
             video_Lifetime.Talking.About.This..Post..by.action.type...like+
             video_Lifetime.Talking.About.This..Post..by.action.type...share, data = train_cors_1) #toggle with _1 for trsfd or not data
summary(model_1)
plot(model_1)
model_1_RSS=c(crossprod(model_1$residuals)); model_1_MSE=(model_1_RSS / length(model_1$residuals)); model_1_RMSE=sqrt(model_1_MSE); model_1_sig2=model_1_RSS / model_1$df.residual
model_1_RSS; model_1_MSE; model_1_RMSE; model_1_sig2
shapiro.test(model_1$residuals) # ALTHOUGH RESIDUAL NON NORMAL AS SAMPLE SIZE IS LARGE MIGHT STILL BE OK

dim(cors_1)
names(cors_1)

# model 2 top 30 
model_2=lm(sales_Social.Media~page_Daily.Paid.Reach+
              page_Daily.Paid.reach.of.Page.posts+
              page_Daily.Paid.Impressions+
              page_Daily.Paid.impressions.of.your.posts+
              page_Daily.Reach.of.Page.posts+
              page_Daily.Total.Reach+
              sales_Total+page_Daily.Total.Impressions.of.your.posts+
              Adv_Total.Volumes+
              page_Daily.Total.Impressions+
              Adv_Total.cost+
              page_Daily.New.Likes+
              page_Daily.Total.Promoted.Views+
              page_Daily.Total.Unique.Video.Views+
              foll_Total.Followers+
              page_Lifetime.Total.Likes+
              page_Daily.Total.Auto.Played.Views+
              page_Daily.Total.Video.Views+
              page_Daily.Total.website.click.count.per.Page+
              post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks+
              post_Lifetime.Negative.Feedback...hide_clicks+
              page_Daily.Unlikes+
              page_Daily.Page.Engaged.Users+
              post_Lifetime.Negative.Feedback+
              post_Lifetime.Negative.feedback.from.users+
              page_Daily.Total.Consumers+
              page_Daily.Negative.Feedback+
              page_Daily.total..Total.action.count.per.Page+
              video_Lifetime.Post.stories.by.action.type...comment.y,data = train_cors_1) 
summary(model_2)
plot(model_2)
model_2_RSS=c(crossprod(model_2$residuals)); model_2_MSE=(model_2_RSS / length(model_2$residuals)); model_2_RMSE=sqrt(model_2_MSE); model_2_sig2=model_2_RSS / model_2$df.residual
model_2_RSS; model_2_MSE; model_2_RMSE; model_2_sig2
shapiro.test(model_2$residuals)

# model 3 top 1 var from each source (crude way to cut out multico)
model_3=lm(sales_Social.Media~page_Daily.Paid.Reach+
             sales_Total+
             Adv_Total.Volumes+
             Adv_Social.Media.cost+
             foll_Daily.New.Followers+
             post_Lifetime.Post.consumers.by.type...video.play+
             video_Lifetime.Post.stories.by.action.type...comment.y,data = train_cors_1) 
summary(model_3)
plot(model_3)
model_3_RSS=c(crossprod(model_3$residuals)); model_3_MSE=(model_3_RSS / length(model_3$residuals)); model_3_RMSE=sqrt(model_3_MSE); model_3_sig2=model_3_RSS / model_3$df.residual
model_3_RSS; model_3_MSE; model_3_RMSE; model_3_sig2
shapiro.test(model_3$residuals)

#model 4 - sales var only
model_4=lm(sales_Social.Media~sales_Total,data = train_cors_1)  
summary(model_4)
plot(model_4)
model_4_RSS=c(crossprod(model_4$residuals)); model_4_MSE=(model_4_RSS / length(model_4$residuals)); model_4_RMSE=sqrt(model_4_MSE); model_4_sig2=model_4_RSS / model_4$df.residual
model_4_RSS; model_4_MSE; model_4_RMSE; model_4_sig2

#model 5 page vars only
#---------------------------------------------------------------------------------------------------------------------------
model_5=lm(sales_Social.Media~page_Lifetime.Total.Likes+
             page_Daily.New.Likes+
             page_Daily.Auto.played.30.second.Views+
             page_Daily.Logged.in.Page.Views+
             page_Daily.Negative.Feedback+
             page_Daily.Organic.Impressions+
             page_Daily.Organic.impressions.of.your.posts+
             page_Daily.Organic.Reach+
             page_Daily.Organic.reach.of.Page.posts+
             page_Daily.Page.Consumptions+
             page_Daily.Page.Engaged.Users+
             page_Daily.Paid.30.second.Views+
             page_Daily.Paid.Impressions+
             page_Daily.Paid.impressions.of.your.posts+
             page_Daily.Paid.Reach+
             page_Daily.Paid.reach.of.Page.posts+
             page_Daily.Reach.of.Page.posts+
             page_Daily.Total.30.Second.Repeats+
             page_Daily.Total.30.second.Views+
             page_Daily.Total.Auto.Played.Views+
             page_Daily.Total.Check.ins+
             page_Daily.Total.Clicked.30.Second.Views+
             page_Daily.Total.Clicked.Views+
             page_Daily.Total.Consumers+
             page_Daily.Total.Impressions+
             page_Daily.Total.Impressions.of.your.posts+
             page_Daily.Total.Organic.30.Second.Views+
             page_Daily.Total.Organic.Views+
             page_Daily.Total.Promoted.Views+
             page_Daily.Total.Reach+
             page_Daily.Total.Unique.30.second.Views+
             page_Daily.Total.Unique.Video.Views+
             page_Daily.Total.Video.Views+
             page_Daily.Total.website.click.count.per.Page+
             page_Daily.total..Total.action.count.per.Page+
             page_Daily.Unlikes+
             page_Daily.Video.Repeats+
             page_Daily.Viral.Impressions+
             page_Daily.Viral.impressions.of.your.posts+
             page_Daily.Viral.Reach+
             page_Daily.Viral.reach.of.Page.posts+
             page_Daily.Total.get.direction.click.count.per.Page, data = train_cors_1) #toggle with _1 for trsfd or not data
summary(model_5)
plot(model_5)
model_5_RSS=c(crossprod(model_5$residuals)); model_5_MSE=(model_5_RSS / length(model_5$residuals)); model_5_RMSE=sqrt(model_5_MSE); model_5_sig2=model_5_RSS / model_5$df.residual
model_5_RSS; model_5_MSE; model_5_RMSE; model_5_sig2
shapiro.test(model_5$residuals) # ALTHOUGH RESIDUAL NON NORMAL AS SAMPLE SIZE IS LARGE MIGHT STILL BE OK

#model 6 post vars only
#---------------------------------------------------------------------------------------------------------------------------
model_6=lm(sales_Social.Media~post_Lifetime.Post.Total.Reach+
             post_Lifetime.Post.Organic.Reach+
             post_Lifetime.Post.Paid.Reach+
             post_Lifetime.Post.Total.Impressions.x+
             post_Lifetime.Post.Organic.Impressions.x+
             post_Lifetime.Post.Paid.Impressions.x+
             post_Lifetime.Engaged.Users+
             post_Lifetime.Post.Consumers+
             post_Lifetime.Post.Consumptions+
             post_Lifetime.Negative.Feedback+
             post_Lifetime.Negative.feedback.from.users+
             post_Lifetime.Post.impressions.by.people.who.have.liked.your.Page+
             post_Lifetime.Post.reach.by.people.who.like.your.Page+
             post_Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page+
             post_Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page+
             post_Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post+
             post_Lifetime.Organic.views.to.95.+
             post_Lifetime.Organic.views.to.95..1+
             post_Lifetime.Paid.views.to.95.+
             post_Lifetime.Paid.views.to.95..1+
             post_Lifetime.Organic.Video.Views.x+
             post_Lifetime.Organic.Video.Views.1+
             post_Lifetime.Paid.Video.Views.x+
             post_Lifetime.Paid.Video.Views.1+
             post_Lifetime.Average.time.video.viewed+
             post_Lifetime.Video.length+
             post_Lifetime.Talking.about.this..post..by.action.type...share+
             post_Lifetime.Talking.about.this..post..by.action.type...like+
             post_Lifetime.Talking.about.this..post..by.action.type...comment+
             post_Lifetime.Post.stories.by.action.type...share.x+
             post_Lifetime.Post.stories.by.action.type...like.x+
             post_Lifetime.Post.stories.by.action.type...comment.x+
             post_Lifetime.Post.consumers.by.type...video.play+
             post_Lifetime.Post.consumers.by.type...other.clicks.x+
             post_Lifetime.Post.consumers.by.type...photo.view.x+
             post_Lifetime.Post.consumers.by.type...link.clicks.x+
             post_Lifetime.Post.consumptions.by.type...video.play+
             post_Lifetime.Post.consumptions.by.type...other.clicks+
             post_Lifetime.Post.consumptions.by.type...photo.view+
             post_Lifetime.Post.consumptions.by.type...link.clicks+
             post_Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks+
             post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks+
             post_Lifetime.Negative.Feedback...hide_all_clicks+
             post_Lifetime.Negative.Feedback...hide_clicks+
             post_Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks+
             post_Lifetime.Negative.Feedback...report_spam_clicks, data = train_cors_1) #toggle with _1 for trsfd or not data
summary(model_6)
plot(model_6)
model_6_RSS=c(crossprod(model_6$residuals)); model_6_MSE=(model_6_RSS / length(model_6$residuals)); model_6_RMSE=sqrt(model_6_MSE); model_6_sig2=model_6_RSS / model_6$df.residual
model_6_RSS; model_6_MSE; model_6_RMSE; model_6_sig2
shapiro.test(model_6$residuals) # ALTHOUGH RESIDUAL NON NORMAL AS SAMPLE SIZE IS LARGE MIGHT STILL BE OK
names(regM)
#model 7 video vars only
#---------------------------------------------------------------------------------------------------------------------------
model_7=lm(sales_Social.Media~video_Video.Lifetime.Post.Total.Reach+
             video_Lifetime.Post.Total.Impressions.y+
             video_Lifetime.Total.Video.Views+
             video_Lifetime.Unique.Video.Views+
             video_Lifetime.Total.30.second.Views+
             video_Lifetime.Unique.30.second.Views+
             video_Lifetime.Total.watches.at.95.+
             video_Lifetime.Unique.watches.at.95.+
             video_Lifetime.Auto.played.Video.Views+
             video_Lifetime.Clicked.to.play.Video.Views+
             video_Lifetime.Auto.played.30.second.Views+
             video_Lifetime.Clicked.to.play.30.second.Views+
             video_Lifetime.Auto.played.watches.at.95.+
             video_Lifetime.Click.to.play.watches.at.95.+
             video_Lifetime.Post.Paid.Impressions.y+
             video_Lifetime.Post.Organic.Impressions.y+
             video_Lifetime.Paid.Video.Views.y+
             video_Lifetime.Organic.Video.Views.y+
             video_Lifetime.Paid.30.second.Views+
             video_Lifetime.Organic.30.second.Views+
             video_Lifetime.Paid.watches.at.95.+
             video_Lifetime.Organic.watches.at.95.+
             video_Lifetime.Post.Consumptions.by.type...clicks.to.play+
             video_Lifetime.Post.Consumptions.by.type...link.clicks+
             video_Lifetime.Post.Consumptions.by.type...other.clicks+
             video_Lifetime.Post.Consumptions.by.type...photo.view+
             video_Lifetime.Post.consumers.by.type...clicks.to.play+
             video_Lifetime.Post.consumers.by.type...link.clicks.y+
             video_Lifetime.Post.consumers.by.type...other.clicks.y+
             video_Lifetime.Post.consumers.by.type...photo.view.y+
             video_Lifetime.Post.stories.by.action.type...comment.y+
             video_Lifetime.Post.stories.by.action.type...like.y+
             video_Lifetime.Post.stories.by.action.type...share.y+
             video_Lifetime.Talking.About.This..Post..by.action.type...comment+
             video_Lifetime.Talking.About.This..Post..by.action.type...like+
             video_Lifetime.Talking.About.This..Post..by.action.type...share, data = train_cors_1) #toggle with _1 for trsfd or not data
summary(model_7)
plot(model_7)
model_7_RSS=c(crossprod(model_7$residuals)); model_7_MSE=(model_7_RSS / length(model_7$residuals)); model_7_RMSE=sqrt(model_7_MSE); model_7_sig2=model_7_RSS / model_7$df.residual
model_7_RSS; model_7_MSE; model_7_RMSE; model_7_sig2
shapiro.test(model_7$residuals) # ALTHOUGH RESIDUAL NON NORMAL AS SAMPLE SIZE IS LARGE MIGHT STILL BE OK

#model 8 adv vars only
#---------------------------------------------------------------------------------------------------------------------------
model_8=lm(sales_Social.Media~Adv_Social.Media.cost+
             Adv_Total.Volumes+
             Adv_Social.Media.Volumes+
             Adv_Total.cost, data = train_cors_1) #toggle with _1 for trsfd or not data
summary(model_8)
plot(model_8)
model_8_RSS=c(crossprod(model_8$residuals)); model_8_MSE=(model_8_RSS / length(model_8$residuals)); model_8_RMSE=sqrt(model_8_MSE); model_8_sig2=model_8_RSS / model_8$df.residual
model_8_RSS; model_8_MSE; model_8_RMSE; model_8_sig2
shapiro.test(model_8$residuals) # ALTHOUGH RESIDUAL NON NORMAL AS SAMPLE SIZE IS LARGE MIGHT STILL BE OK

#model 9 foll vars only
#---------------------------------------------------------------------------------------------------------------------------
model_9=lm(sales_Social.Media~foll_Daily.New.Followers+foll_Total.Followers, data = train_cors_1) #toggle with _1 for trsfd or not data
summary(model_9)
plot(model_9)
model_9_RSS=c(crossprod(model_9$residuals)); model_9_MSE=(model_9_RSS / length(model_9$residuals)); model_9_RMSE=sqrt(model_9_MSE); model_9_sig2=model_9_RSS / model_9$df.residual
model_9_RSS; model_9_MSE; model_9_RMSE; model_9_sig2
shapiro.test(model_9$residuals) # ALTHOUGH RESIDUAL NON NORMAL AS SAMPLE SIZE IS LARGE MIGHT STILL BE OK

#model 10 all exc post and video due to those being low cor models
#---------------------------------------------------------------------------------------------------------------------------
model_10=lm(sales_Social.Media~sales_Total+
             foll_Total.Followers+
             foll_Daily.New.Followers+
             Adv_Social.Media.cost+
             Adv_Total.Volumes+
             Adv_Social.Media.Volumes+
             Adv_Total.cost+
             page_Lifetime.Total.Likes+
             page_Daily.New.Likes+
             page_Daily.Auto.played.30.second.Views+
             page_Daily.Logged.in.Page.Views+
             page_Daily.Negative.Feedback+
             page_Daily.Organic.Impressions+
             page_Daily.Organic.impressions.of.your.posts+
             page_Daily.Organic.Reach+
             page_Daily.Organic.reach.of.Page.posts+
             page_Daily.Page.Consumptions+
             page_Daily.Page.Engaged.Users+
             page_Daily.Paid.30.second.Views+
             page_Daily.Paid.Impressions+
             page_Daily.Paid.impressions.of.your.posts+
             page_Daily.Paid.Reach+
             page_Daily.Paid.reach.of.Page.posts+
             page_Daily.Reach.of.Page.posts+
             page_Daily.Total.30.Second.Repeats+
             page_Daily.Total.30.second.Views+
             page_Daily.Total.Auto.Played.Views+
             page_Daily.Total.Check.ins+
             page_Daily.Total.Clicked.30.Second.Views+
             page_Daily.Total.Clicked.Views+
             page_Daily.Total.Consumers+
             page_Daily.Total.Impressions+
             page_Daily.Total.Impressions.of.your.posts+
             page_Daily.Total.Organic.30.Second.Views+
             page_Daily.Total.Organic.Views+
             page_Daily.Total.Promoted.Views+
             page_Daily.Total.Reach+
             page_Daily.Total.Unique.30.second.Views+
             page_Daily.Total.Unique.Video.Views+
             page_Daily.Total.Video.Views+
             page_Daily.Total.website.click.count.per.Page+
             page_Daily.total..Total.action.count.per.Page+
             page_Daily.Unlikes+
             page_Daily.Video.Repeats+
             page_Daily.Viral.Impressions+
             page_Daily.Viral.impressions.of.your.posts+
             page_Daily.Viral.Reach+
             page_Daily.Viral.reach.of.Page.posts+
             page_Daily.Total.get.direction.click.count.per.Page, data = train_cors_1) #toggle with _1 for trsfd or not data
summary(model_10)
plot(model_10)
model_10_RSS=c(crossprod(model_10$residuals)); model_10_MSE=(model_10_RSS / length(model_10$residuals)); model_10_RMSE=sqrt(model_10_MSE); model_10_sig2=model_10_RSS / model_10$df.residual
model_10_RSS; model_10_MSE; model_10_RMSE; model_10_sig2
shapiro.test(model_10$residuals) # ALTHOUGH RESIDUAL NON NORMAL AS SAMPLE SIZE IS LARGE MIGHT STILL BE OK

# model 11 top 10 vars post leaps
model_11=lm(sales_Social.Media~page_Daily.Paid.reach.of.Page.posts+
             page_Daily.Paid.Impressions+
             page_Daily.Paid.impressions.of.your.posts+
             sales_Total+
             Adv_Total.Volumes,data = train_cors_1) 
summary(model_11)

#evaluate models
#----------------

#compare test / train set - growth trend of time series reflected in x and y vars so model should be ok
mean(train_cors_1$sales_Social.Media)/mean(test_cors_1$sales_Social.Media)
mean(train_cors_1$page_Daily.Paid.Reach)/mean(test_cors_1$page_Daily.Paid.Reach)

#var sel/ leaps for model 2 (less than 30 vars)
#----------------------------
install.packages("leaps")
library(leaps)

names(train_cors_1)
leaps_data=subset(train_cors_1,select=c(Date,sales_Social.Media,page_Daily.Paid.Reach,
                                          page_Daily.Paid.reach.of.Page.posts,
                                          page_Daily.Paid.Impressions,
                                          page_Daily.Paid.impressions.of.your.posts,
                                          page_Daily.Reach.of.Page.posts,
                                          page_Daily.Total.Reach,
                                          sales_Total,page_Daily.Total.Impressions.of.your.posts,
                                          Adv_Total.Volumes,
                                          page_Daily.Total.Impressions,
                                          Adv_Total.cost,
                                          page_Daily.New.Likes,
                                          page_Daily.Total.Promoted.Views,
                                          page_Daily.Total.Unique.Video.Views,
                                          foll_Total.Followers,
                                          page_Lifetime.Total.Likes,
                                          page_Daily.Total.Auto.Played.Views,
                                          page_Daily.Total.Video.Views,
                                          page_Daily.Total.website.click.count.per.Page,
                                          post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks,
                                          post_Lifetime.Negative.Feedback...hide_clicks,
                                          page_Daily.Unlikes,
                                          page_Daily.Page.Engaged.Users,
                                          post_Lifetime.Negative.Feedback,
                                          post_Lifetime.Negative.feedback.from.users,
                                          page_Daily.Total.Consumers,
                                          page_Daily.Negative.Feedback,
                                          page_Daily.total..Total.action.count.per.Page,
                                          video_Lifetime.Post.stories.by.action.type...comment.y))
head(leaps_data[,3:31])

best_subset=leaps(x = leaps_data[, 3:31], y = leaps_data[, 2],
                  nbest = 5, method = "adjr2",
                  names = colnames(leaps_data[,3:31]))

rank_x_variables=data.frame(Size = best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
                            best_subset$which, row.names = NULL)

data.frame(Size = best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
           best_subset$which, row.names = NULL)

head(rank_x_variables)
tail(rank_x_variables)
rank_x_variables
library(openxlsx)
write.xlsx(rank_x_variables, 'leaps_ranking.xlsx')  #exports to H:\Documents

#plot leaps curve add legends etc
plot(best_subset$size, best_subset$adjr2,ylab = "Adjusted R-squared", xlab = "Number of Explanatory Variables (including intercept)", main="Top 30 Model: Leaps Algorithim",pch=1,cex=0.7,col="red")
abline(h=0.685, lty=2,col="blue")
abline(v=14, lty=2,col="blue")

#model_2 matrix colinearity check
#---------------------------------
#create df foll mat
names(train_cors_1)
attach(train_cors_1)
model_2_matrix=data.frame(Date, page_Daily.Paid.Impressions,page_Daily.Reach.of.Page.posts,page_Daily.Total.Reach,sales_Total,page_Daily.Total.Impressions.of.your.posts,page_Daily.Total.Impressions,page_Daily.New.Likes,foll_Total.Followers,page_Lifetime.Total.Likes,page_Daily.Total.website.click.count.per.Page,page_Daily.Total.Consumers,page_Daily.Negative.Feedback,page_Daily.total..Total.action.count.per.Page)
names(model_2_matrix)
scatmat_model_2=model_2_matrix[c("Date","page_Daily.Paid.Impressions","page_Daily.Reach.of.Page.posts","page_Daily.Total.Reach","sales_Total","page_Daily.Total.Impressions.of.your.posts","page_Daily.Total.Impressions","page_Daily.New.Likes","foll_Total.Followers","page_Lifetime.Total.Likes","page_Daily.Total.website.click.count.per.Page","page_Daily.Total.Consumers","page_Daily.Negative.Feedback","page_Daily.total..Total.action.count.per.Page")]
detach(train_cors_1)

#visualise - scatter matrix 
pairs(scatmat_model_2[2:14],
      pch = 19, col = adjustcolor(c("dodgerblue"), alpha.f = 0.4),oma=c(8,4,8,4),
      main = "Model_2 Collinearity")

#look at colinearity in model_2
cormat_model_2=scatmat_model_2[,c(1,2:14)] #fix empty data
names(cormat_model_2)
cormat_model_2_cormatrix=cor(cormat_model_2[,-c(1,14)])

library(openxlsx)
write.xlsx(cormat_model_2_cormatrix, 'cormat_model_2.xlsx')  #exports to H:\Documents

#2.1 model
#============
# model 2.1 top 1 var from each source (crude way to cut out multico)
model_2.1=lm(sales_Social.Media~page_Daily.Paid.Impressions+
                 sales_Total+
                 page_Daily.Total.Impressions.of.your.posts+
                 page_Daily.New.Likes+
                 foll_Total.Followers+
                 page_Lifetime.Total.Likes+
                 page_Daily.Total.website.click.count.per.Page+
                 page_Daily.Total.Consumers+
                 page_Daily.Negative.Feedback,data = train_cors_1) 
summary(model_2.1)
plot(model_2.1)

model_2.1_RSS=c(crossprod(model_2.1$residuals)); model_2.1_MSE=(model_2.1_RSS / length(model_2.1$residuals)); model_2.1_RMSE=sqrt(model_2.1_MSE); model_2.1_sig2=model_2.1_RSS / model_2.1$df.residual
model_2.1_RSS; model_2.1_MSE; model_2.1_RMSE; model_2.1_sig2
shapiro.test(model_2.1$residuals)
plot(model_2.1$residuals)

install.packages("rcompanion")
library(rcompanion)
accuracy(list(model_1,model_2, model_3, model_4,model_5,model_6,model_7, model_8, model_9,model_10, model_2.1),
         plotit=TRUE, digits=3)
#-----------------------------------------------------------------------------------------------------------------------------------------

#MAKE PREDICTIONS using top model
#predict using model 1 as highest r2
Model_1_Predict=lm(sales_Social.Media~sales_Total+
                     foll_Total.Followers+
                     foll_Daily.New.Followers+
                     Adv_Social.Media.cost+
                     Adv_Total.Volumes+
                     Adv_Social.Media.Volumes+
                     Adv_Total.cost+
                     page_Lifetime.Total.Likes+
                     page_Daily.New.Likes+
                     page_Daily.Auto.played.30.second.Views+
                     page_Daily.Logged.in.Page.Views+
                     page_Daily.Negative.Feedback+
                     page_Daily.Organic.Impressions+
                     page_Daily.Organic.impressions.of.your.posts+
                     page_Daily.Organic.Reach+
                     page_Daily.Organic.reach.of.Page.posts+
                     page_Daily.Page.Consumptions+
                     page_Daily.Page.Engaged.Users+
                     page_Daily.Paid.30.second.Views+
                     page_Daily.Paid.Impressions+
                     page_Daily.Paid.impressions.of.your.posts+
                     page_Daily.Paid.Reach+
                     page_Daily.Paid.reach.of.Page.posts+
                     page_Daily.Reach.of.Page.posts+
                     page_Daily.Total.30.Second.Repeats+
                     page_Daily.Total.30.second.Views+
                     page_Daily.Total.Auto.Played.Views+
                     page_Daily.Total.Check.ins+
                     page_Daily.Total.Clicked.30.Second.Views+
                     page_Daily.Total.Clicked.Views+
                     page_Daily.Total.Consumers+
                     page_Daily.Total.Impressions+
                     page_Daily.Total.Impressions.of.your.posts+
                     page_Daily.Total.Organic.30.Second.Views+
                     page_Daily.Total.Organic.Views+
                     page_Daily.Total.Promoted.Views+
                     page_Daily.Total.Reach+
                     page_Daily.Total.Unique.30.second.Views+
                     page_Daily.Total.Unique.Video.Views+
                     page_Daily.Total.Video.Views+
                     page_Daily.Total.website.click.count.per.Page+
                     page_Daily.total..Total.action.count.per.Page+
                     page_Daily.Unlikes+
                     page_Daily.Video.Repeats+
                     page_Daily.Viral.Impressions+
                     page_Daily.Viral.impressions.of.your.posts+
                     page_Daily.Viral.Reach+
                     page_Daily.Viral.reach.of.Page.posts+
                     page_Daily.Total.get.direction.click.count.per.Page+
                     post_Lifetime.Post.Total.Reach+
                     post_Lifetime.Post.Organic.Reach+
                     post_Lifetime.Post.Paid.Reach+
                     post_Lifetime.Post.Total.Impressions.x+
                     post_Lifetime.Post.Organic.Impressions.x+
                     post_Lifetime.Post.Paid.Impressions.x+
                     post_Lifetime.Engaged.Users+
                     post_Lifetime.Post.Consumers+
                     post_Lifetime.Post.Consumptions+
                     post_Lifetime.Negative.Feedback+
                     post_Lifetime.Negative.feedback.from.users+
                     post_Lifetime.Post.impressions.by.people.who.have.liked.your.Page+
                     post_Lifetime.Post.reach.by.people.who.like.your.Page+
                     post_Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page+
                     post_Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page+
                     post_Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post+
                     post_Lifetime.Organic.views.to.95.+
                     post_Lifetime.Organic.views.to.95..1+
                     post_Lifetime.Paid.views.to.95.+
                     post_Lifetime.Paid.views.to.95..1+
                     post_Lifetime.Organic.Video.Views.x+
                     post_Lifetime.Organic.Video.Views.1+
                     post_Lifetime.Paid.Video.Views.x+
                     post_Lifetime.Paid.Video.Views.1+
                     post_Lifetime.Average.time.video.viewed+
                     post_Lifetime.Video.length+
                     post_Lifetime.Talking.about.this..post..by.action.type...share+
                     post_Lifetime.Talking.about.this..post..by.action.type...like+
                     post_Lifetime.Talking.about.this..post..by.action.type...comment+
                     post_Lifetime.Post.stories.by.action.type...share.x+
                     post_Lifetime.Post.stories.by.action.type...like.x+
                     post_Lifetime.Post.stories.by.action.type...comment.x+
                     post_Lifetime.Post.consumers.by.type...video.play+
                     post_Lifetime.Post.consumers.by.type...other.clicks.x+
                     post_Lifetime.Post.consumers.by.type...photo.view.x+
                     post_Lifetime.Post.consumers.by.type...link.clicks.x+
                     post_Lifetime.Post.consumptions.by.type...video.play+
                     post_Lifetime.Post.consumptions.by.type...other.clicks+
                     post_Lifetime.Post.consumptions.by.type...photo.view+
                     post_Lifetime.Post.consumptions.by.type...link.clicks+
                     post_Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks+
                     post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks+
                     post_Lifetime.Negative.Feedback...hide_all_clicks+
                     post_Lifetime.Negative.Feedback...hide_clicks+
                     post_Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks+
                     post_Lifetime.Negative.Feedback...report_spam_clicks+
                     video_Video.Lifetime.Post.Total.Reach+
                     video_Lifetime.Post.Total.Impressions.y+
                     video_Lifetime.Total.Video.Views+
                     video_Lifetime.Unique.Video.Views+
                     video_Lifetime.Total.30.second.Views+
                     video_Lifetime.Unique.30.second.Views+
                     video_Lifetime.Total.watches.at.95.+
                     video_Lifetime.Unique.watches.at.95.+
                     video_Lifetime.Auto.played.Video.Views+
                     video_Lifetime.Clicked.to.play.Video.Views+
                     video_Lifetime.Auto.played.30.second.Views+
                     video_Lifetime.Clicked.to.play.30.second.Views+
                     video_Lifetime.Auto.played.watches.at.95.+
                     video_Lifetime.Click.to.play.watches.at.95.+
                     video_Lifetime.Post.Paid.Impressions.y+
                     video_Lifetime.Post.Organic.Impressions.y+
                     video_Lifetime.Paid.Video.Views.y+
                     video_Lifetime.Organic.Video.Views.y+
                     video_Lifetime.Paid.30.second.Views+
                     video_Lifetime.Organic.30.second.Views+
                     video_Lifetime.Paid.watches.at.95.+
                     video_Lifetime.Organic.watches.at.95.+
                     video_Lifetime.Post.Consumptions.by.type...clicks.to.play+
                     video_Lifetime.Post.Consumptions.by.type...link.clicks+
                     video_Lifetime.Post.Consumptions.by.type...other.clicks+
                     video_Lifetime.Post.Consumptions.by.type...photo.view+
                     video_Lifetime.Post.consumers.by.type...clicks.to.play+
                     video_Lifetime.Post.consumers.by.type...link.clicks.y+
                     video_Lifetime.Post.consumers.by.type...other.clicks.y+
                     video_Lifetime.Post.consumers.by.type...photo.view.y+
                     video_Lifetime.Post.stories.by.action.type...comment.y+
                     video_Lifetime.Post.stories.by.action.type...like.y+
                     video_Lifetime.Post.stories.by.action.type...share.y+
                     video_Lifetime.Talking.About.This..Post..by.action.type...comment+
                     video_Lifetime.Talking.About.This..Post..by.action.type...like+
                     video_Lifetime.Talking.About.This..Post..by.action.type...share,data = test_cors_1) 
                   
predict_1=predict(Model_1_Predict,test_cors_1=data.frame(sales_Social.Media~sales_Total+
                                                           foll_Total.Followers+
                                                           foll_Daily.New.Followers+
                                                           Adv_Social.Media.cost+
                                                           Adv_Total.Volumes+
                                                           Adv_Social.Media.Volumes+
                                                           Adv_Total.cost+
                                                           page_Lifetime.Total.Likes+
                                                           page_Daily.New.Likes+
                                                           page_Daily.Auto.played.30.second.Views+
                                                           page_Daily.Logged.in.Page.Views+
                                                           page_Daily.Negative.Feedback+
                                                           page_Daily.Organic.Impressions+
                                                           page_Daily.Organic.impressions.of.your.posts+
                                                           page_Daily.Organic.Reach+
                                                           page_Daily.Organic.reach.of.Page.posts+
                                                           page_Daily.Page.Consumptions+
                                                           page_Daily.Page.Engaged.Users+
                                                           page_Daily.Paid.30.second.Views+
                                                           page_Daily.Paid.Impressions+
                                                           page_Daily.Paid.impressions.of.your.posts+
                                                           page_Daily.Paid.Reach+
                                                           page_Daily.Paid.reach.of.Page.posts+
                                                           page_Daily.Reach.of.Page.posts+
                                                           page_Daily.Total.30.Second.Repeats+
                                                           page_Daily.Total.30.second.Views+
                                                           page_Daily.Total.Auto.Played.Views+
                                                           page_Daily.Total.Check.ins+
                                                           page_Daily.Total.Clicked.30.Second.Views+
                                                           page_Daily.Total.Clicked.Views+
                                                           page_Daily.Total.Consumers+
                                                           page_Daily.Total.Impressions+
                                                           page_Daily.Total.Impressions.of.your.posts+
                                                           page_Daily.Total.Organic.30.Second.Views+
                                                           page_Daily.Total.Organic.Views+
                                                           page_Daily.Total.Promoted.Views+
                                                           page_Daily.Total.Reach+
                                                           page_Daily.Total.Unique.30.second.Views+
                                                           page_Daily.Total.Unique.Video.Views+
                                                           page_Daily.Total.Video.Views+
                                                           page_Daily.Total.website.click.count.per.Page+
                                                           page_Daily.total..Total.action.count.per.Page+
                                                           page_Daily.Unlikes+
                                                           page_Daily.Video.Repeats+
                                                           page_Daily.Viral.Impressions+
                                                           page_Daily.Viral.impressions.of.your.posts+
                                                           page_Daily.Viral.Reach+
                                                           page_Daily.Viral.reach.of.Page.posts+
                                                           page_Daily.Total.get.direction.click.count.per.Page+
                                                           post_Lifetime.Post.Total.Reach+
                                                           post_Lifetime.Post.Organic.Reach+
                                                           post_Lifetime.Post.Paid.Reach+
                                                           post_Lifetime.Post.Total.Impressions.x+
                                                           post_Lifetime.Post.Organic.Impressions.x+
                                                           post_Lifetime.Post.Paid.Impressions.x+
                                                           post_Lifetime.Engaged.Users+
                                                           post_Lifetime.Post.Consumers+
                                                           post_Lifetime.Post.Consumptions+
                                                           post_Lifetime.Negative.Feedback+
                                                           post_Lifetime.Negative.feedback.from.users+
                                                           post_Lifetime.Post.impressions.by.people.who.have.liked.your.Page+
                                                           post_Lifetime.Post.reach.by.people.who.like.your.Page+
                                                           post_Lifetime.Post.paid.impressions.by.people.who.have.liked.your.Page+
                                                           post_Lifetime.Paid.reach.of.a.post.by.people.who.like.your.Page+
                                                           post_Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post+
                                                           post_Lifetime.Organic.views.to.95.+
                                                           post_Lifetime.Organic.views.to.95..1+
                                                           post_Lifetime.Paid.views.to.95.+
                                                           post_Lifetime.Paid.views.to.95..1+
                                                           post_Lifetime.Organic.Video.Views.x+
                                                           post_Lifetime.Organic.Video.Views.1+
                                                           post_Lifetime.Paid.Video.Views.x+
                                                           post_Lifetime.Paid.Video.Views.1+
                                                           post_Lifetime.Average.time.video.viewed+
                                                           post_Lifetime.Video.length+
                                                           post_Lifetime.Talking.about.this..post..by.action.type...share+
                                                           post_Lifetime.Talking.about.this..post..by.action.type...like+
                                                           post_Lifetime.Talking.about.this..post..by.action.type...comment+
                                                           post_Lifetime.Post.stories.by.action.type...share.x+
                                                           post_Lifetime.Post.stories.by.action.type...like.x+
                                                           post_Lifetime.Post.stories.by.action.type...comment.x+
                                                           post_Lifetime.Post.consumers.by.type...video.play+
                                                           post_Lifetime.Post.consumers.by.type...other.clicks.x+
                                                           post_Lifetime.Post.consumers.by.type...photo.view.x+
                                                           post_Lifetime.Post.consumers.by.type...link.clicks.x+
                                                           post_Lifetime.Post.consumptions.by.type...video.play+
                                                           post_Lifetime.Post.consumptions.by.type...other.clicks+
                                                           post_Lifetime.Post.consumptions.by.type...photo.view+
                                                           post_Lifetime.Post.consumptions.by.type...link.clicks+
                                                           post_Lifetime.Negative.feedback.from.users.by.type...hide_all_clicks+
                                                           post_Lifetime.Negative.feedback.from.users.by.type...hide_clicks+
                                                           post_Lifetime.Negative.Feedback...hide_all_clicks+
                                                           post_Lifetime.Negative.Feedback...hide_clicks+
                                                           post_Lifetime.Negative.feedback.from.users.by.type...report_spam_clicks+
                                                           post_Lifetime.Negative.Feedback...report_spam_clicks+
                                                           video_Video.Lifetime.Post.Total.Reach+
                                                           video_Lifetime.Post.Total.Impressions.y+
                                                           video_Lifetime.Total.Video.Views+
                                                           video_Lifetime.Unique.Video.Views+
                                                           video_Lifetime.Total.30.second.Views+
                                                           video_Lifetime.Unique.30.second.Views+
                                                           video_Lifetime.Total.watches.at.95.+
                                                           video_Lifetime.Unique.watches.at.95.+
                                                           video_Lifetime.Auto.played.Video.Views+
                                                           video_Lifetime.Clicked.to.play.Video.Views+
                                                           video_Lifetime.Auto.played.30.second.Views+
                                                           video_Lifetime.Clicked.to.play.30.second.Views+
                                                           video_Lifetime.Auto.played.watches.at.95.+
                                                           video_Lifetime.Click.to.play.watches.at.95.+
                                                           video_Lifetime.Post.Paid.Impressions.y+
                                                           video_Lifetime.Post.Organic.Impressions.y+
                                                           video_Lifetime.Paid.Video.Views.y+
                                                           video_Lifetime.Organic.Video.Views.y+
                                                           video_Lifetime.Paid.30.second.Views+
                                                           video_Lifetime.Organic.30.second.Views+
                                                           video_Lifetime.Paid.watches.at.95.+
                                                           video_Lifetime.Organic.watches.at.95.+
                                                           video_Lifetime.Post.Consumptions.by.type...clicks.to.play+
                                                           video_Lifetime.Post.Consumptions.by.type...link.clicks+
                                                           video_Lifetime.Post.Consumptions.by.type...other.clicks+
                                                           video_Lifetime.Post.Consumptions.by.type...photo.view+
                                                           video_Lifetime.Post.consumers.by.type...clicks.to.play+
                                                           video_Lifetime.Post.consumers.by.type...link.clicks.y+
                                                           video_Lifetime.Post.consumers.by.type...other.clicks.y+
                                                           video_Lifetime.Post.consumers.by.type...photo.view.y+
                                                           video_Lifetime.Post.stories.by.action.type...comment.y+
                                                           video_Lifetime.Post.stories.by.action.type...like.y+
                                                           video_Lifetime.Post.stories.by.action.type...share.y+
                                                           video_Lifetime.Talking.About.This..Post..by.action.type...comment+
                                                           video_Lifetime.Talking.About.This..Post..by.action.type...like+
                                                           video_Lifetime.Talking.About.This..Post..by.action.type...share),
                                                  interval="prediction",level=0.95)

#predict using model 3
Model_3_Predict=lm(sales_Social.Media~page_Daily.Paid.Reach+
             sales_Total+
             Adv_Total.Volumes+
             Adv_Social.Media.cost+
             foll_Daily.New.Followers+
             post_Lifetime.Post.consumers.by.type...video.play+
             video_Lifetime.Post.stories.by.action.type...comment.y,data = test_cors_1)


predict_3=predict(Model_3_Predict,test_cors_1=data.frame(sales_Social.Media~page_Daily.Paid.Reach+
                                                           sales_Total+
                                                           Adv_Total.Volumes+
                                                           Adv_Social.Media.cost+
                                                           foll_Daily.New.Followers+
                                                           post_Lifetime.Post.consumers.by.type...video.play+
                                                           video_Lifetime.Post.stories.by.action.type...comment.y),
                  interval="prediction",level=0.95)

#predict using model 9
model_9_Predict=lm(sales_Social.Media~foll_Daily.New.Followers+foll_Total.Followers,data = test_cors_1) 

predict_9=predict(model_9_Predict,test_cors_1=data.frame(sales_Social.Media~foll_Daily.New.Followers+foll_Total.Followers),
                  interval="prediction",level=0.95)

#predict using model 2.1
model_2.1_Predict=lm(sales_Social.Media~page_Daily.Paid.Impressions+
                       sales_Total+
                       page_Daily.Total.Impressions.of.your.posts+
                       page_Daily.New.Likes+
                       foll_Total.Followers+
                       page_Lifetime.Total.Likes+
                       page_Daily.Total.website.click.count.per.Page+
                       page_Daily.Total.Consumers+
                       page_Daily.Negative.Feedback,data = test_cors_1) 

predict_2.1=predict(model_2.1_Predict,test_cors_1=data.frame(sales_Social.Media~page_Daily.Paid.Impressions+
                                                               sales_Total+
                                                               page_Daily.Total.Impressions.of.your.posts+
                                                               page_Daily.New.Likes+
                                                               foll_Total.Followers+
                                                               page_Lifetime.Total.Likes+
                                                               page_Daily.Total.website.click.count.per.Page+
                                                               page_Daily.Total.Consumers+
                                                               page_Daily.Negative.Feedback),
                  interval="prediction",level=0.95)


#predict_1=abs(predict_1) # bit of a trick but ok solution for neg responses, negs due to metrics like daily new folls having big reductions. sol to take log of predictions
#predict_9=abs(predict_9) # no, negs not cos of logs -  is because of lost followers metric
#predict_2=abs(predict_2) 
#predict_4=abs(predict_4) 
dim(predict_1); dim(test_cors_1)
head(predict_1)
head(test_cors_1$sales_Social.Media)
head(predict_1[,1])-head(test_cors_1$sales_Social.Media)
yhat_1=predict_1[,1];yhat_1_log=log(yhat_1);yhat_1_log[is.na(yhat_1_log)]<-0 # prediction
yhat_3=predict_3[,1];yhat_3_log=log(yhat_3);yhat_3_log[is.na(yhat_3_log)]<-0 # prediction 
yhat_9=predict_9[,1];yhat_9_log=log(yhat_9);yhat_9_log[is.na(yhat_9_log)]<-0 # prediction 
yhat_2.1=predict_2.1[,1];yhat_2.1_log=log(yhat_2.1);yhat_2.1_log[is.na(yhat_2.1_log)]<-0 # prediction 

x=test_cors_1$Date
y=subset(test_cors_1,select=c(sales_Social.Media))
y=as.numeric(y[[1]])
y_log=log(y);y_log[is.na(y_log)]<-0 # prediction
nrow(test_cors_1) #df
length(yhat_1) #vec
length(yhat_3) #vec
length(yhat_9) #vec
length(yhat_2.1) #vec
tail(test_cors_1$Date)

par(mfrow=c(1,1))
plot(x,y,type = "l",
     xlab = "2018", ylab = "Social Media Ticket Sales",col="yellow",lwd=5,lty=1,
     main = "Model_1 Actual v Predicted Social Media Sales for 03-Oct-2018 - 29-Jun-2018",cex.main=1.0)
legend("top",legend=c("Actual","Predicted."),fill=c("yellow","dodgerblue"),horiz=FALSE,cex=1.0,xpd=TRUE)
#add model1,2,3 yhat fitted
lines(x,yhat_1,col="dodgerblue",lwd=1,lty=1)

plot(x,y,type = "l",
     xlab = "2018", ylab = "Social Media Ticket Sales",col="yellow",lwd=5,lty=1,
     main = "Model_3 Actual v Predicted Social Media Sales for 03-Oct-2018 - 29-Jun-2018",cex.main=1.0)
legend("top",legend=c("Actual","Predicted."),fill=c("yellow","dodgerblue"),horiz=FALSE,cex=1.0,xpd=TRUE)
#add model1,2,3 yhat fitted
lines(x,yhat_3,col="dodgerblue",lwd=1,lty=1)

plot(x,y,type = "l",
     xlab = "2018", ylab = "Social Media Ticket Sales",col="yellow",lwd=5,lty=1,
     main = "Model_9 Actual v Predicted Social Media Sales for 03-Oct-2018 - 29-Jun-2018",cex.main=1.0)
legend("top",legend=c("Actual","Predicted."),fill=c("yellow","dodgerblue"),horiz=FALSE,cex=1.0,xpd=TRUE)
#add model1,2,3 yhat fitted
lines(x,yhat_9,col="dodgerblue",lwd=1,lty=1)

plot(x,y,type = "l",
     xlab = "2018", ylab = "Social Media Ticket Sales",col="yellow",lwd=5,lty=1,
     main = "Model_2.1 Actual v Predicted Social Media Sales for 03-Oct-2018 - 29-Jun-2018",cex.main=1.0)
legend("top",legend=c("Actual","Predicted."),fill=c("yellow","dodgerblue"),horiz=FALSE,cex=1.0,xpd=TRUE)
#add model1,2,3 yhat fitted
lines(x,yhat_2.1,col="dodgerblue",lwd=1,lty=1)
#check cor of prediction v actual

#plot log
y_log=log(y);y_log[is.na(y_log)]<-0 # prediction
plot(x,y_log,type = "l",
     xlab = "2018", ylab = "Social Media Ticket Sales",col="black",lwd=1,lty=1,
     main = "Actual v Predicted Social Media Sales for 03-Oct-2018 - 29-Jun-2018",cex.main=3.0)
legend("top",legend=c("Actual","Predict Model_1 est.","Predict Model_9 est."),fill=c("black","blue","yellow"),horiz=FALSE,cex=1.5,xpd=TRUE)
#add model1,2,3 yhat fitted
lines(x,yhat_1_log,col="blue",lwd=1,lty=1)
lines(x,yhat_3_log,col="black",lwd=1,lty=1)
lines(x,yhat_9_log,col="darkorange",lwd=1,lty=1)
lines(x,yhat_2.1_log,col="pink",lwd=1,lty=1)

#check cor of prediction v actual
cor(predict_1[,1],test_cors_1$sales_Social.Media)
cor(predict_3[,1],test_cors_1$sales_Social.Media)
cor(predict_9[,1],test_cors_1$sales_Social.Media)
cor(predict_2.1[,1],test_cors_1$sales_Social.Media)

#check sum predict v test (should be zero before abs)
sum(predict_1[,1])-sum(test_cors_1$sales_Social.Media) #var +0.0 due to conv. to abs
sum(predict_3[,1])-sum(test_cors_1$sales_Social.Media) 
sum(predict_9[,1])-sum(test_cors_1$sales_Social.Media) 
sum(predict_2.1[,1])-sum(test_cors_1$sales_Social.Media) 
#so now we have a good p[rediction model, suprising that it captures the strong spike in early July.  Now, identify the driver so we can use in the business.
#also need to look at colinearity between vars, such as paid reach and total reach

#graphs below show if simply strong correlation with total sales, that is SM sales / Total Sales (Tot.Sales not used as x var I think??)
x1=Ticket_Sales$Date
y1=Ticket_Sales$Total
y2=Ticket_Sales$Social.Media
names(video)
par(mfrow=c(2,1))
plot(x1,y1,type = "l",
     xlab = "Date", ylab = "Total Ticket Sales",col="black",lwd=1,lty=1,
     main = "Ticket Sales 1.1.2016 to 30.6.2018",cex.main=0.9)
#legend("top",legend=c("Actual","Estimate model_1 est.","Estimate model_2 est."),fill=c("black","blue","yellow"),horiz=FALSE,cex=0.5,xpd=TRUE)
#add model1,2,3 yhat fitted
plot(x1,y2,type = "l",
     xlab = "Date", ylab = "Social Media Ticket Sales",col="blue",lwd=1,lty=1,
     main = "Social Media Ticket Sales 1.1.2016 to 30.6.2018",cex.main=0.9)
#legend("top",legend=c("Actual","Estimate model_1 est.","Estimate model_2 est."),fill=c("black","blue","yellow"),horiz=FALSE,cex=0.5,xpd=TRUE)

#21/6/18 social media sale analysis-----------------------------------------------------------------

#post_21.6.2018=post[1:50,]
names(post_21.6.2018)
dim(post_21.6.2018)

theme_set(theme_minimal())

ts_page_1=ggplot(data = video, aes(x = Posted, y = video$Lifetime.Talking.About.This..Post..by.action.type...share)) + 
  geom_line(color = "blue", size = 0.25)
ts_page_1+scale_x_date(date_labels = "%b/%Y")

#FOR REGRESSOR TREE - train_cors_2 and test_cors_2 are the files needed I think
library(openxlsx)
write.xlsx(regM, 'regM.xlsx')  #exports to H:\Documents
write.xlsx(cors_1, 'cors_1.xlsx')  #exports to H:\Documents
write.xlsx(train_cors_2, 'train_cors_2.xlsx')  #exports to H:\Documents
write.xlsx(train_cors_1, 'train_cors_1.xlsx')  #exports to H:\Documents
write.xlsx(test_cors_2, 'test_cors_2.xlsx')  #exports to H:\Documents
write.xlsx(test_cors_1, 'test_cors_1.xlsx')  #exports to H:\Documents

#=============================================================================================================================================
#=============================================================================================================================================
# non linear regression option #decision tree
#--------------------------------------------

install.packages("rsample")     
library(rsample)     # data splitting 
set.seed(7)#king of kings

cors_tree=subset(cors_1,select=c(0,2:133))  # make 2:133 to exclude date
boosh_split <- initial_split(cors_tree, prop = 4/5)
boosh_train <- training(boosh_split)
boosh_test  <- testing(boosh_split)

dim(cors_1)
dim(cors_tree)
dim(boosh_test)
dim(boosh_train)
names(boosh_train)


install.packages("rpart")     
library(rpart)  # plotting reg

set.seed(7)#king of kings

rtree1 <- rpart(
  formula = sales_Social.Media  ~ .,
  data    = boosh_train,
  method  = "anova"
)
rtree1

install.packages("rpart.plot")     
library(rpart.plot)  # plotting regression trees
rpart.plot(rtree1)
plotcp(rtree1)
abline(v = 7, lty = "dashed", col="blue")
rtree1$cptable

hyper_grid <- expand.grid(
  minsplit = seq(2, 20, 1),
  maxdepth = seq(2, 20, 1)
)
head(hyper_grid)

# total number of combinations
total_combinations=nrow(hyper_grid)
total_combinations

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # minsplit maxdepth values for i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train model list
  models[[i]] <- rpart(
    formula = sales_Social.Media ~ .,
    data    = boosh_train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# min err
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

install.packages("dplyr")      
library(dplyr)
hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

install.packages("rpart")
library(rpart)
optimal_tree <- rpart(
  formula = sales_Social.Media ~ .,
  data    = boosh_train,
  method  = "anova",
  control = list(minsplit = 11, maxdepth = 8, cp = 0.01)
)

install.packages("ipred")
library(ipred)
pred <- predict(optimal_tree, newdata = boosh_test)
print(pred)

install.packages("caret")
library(caret)
RMSE(pred = pred, obs = boosh_test$sales_Social.Media)
## [1] 34.69303
length(pred)

#random forest
#-------------

install.packages("h2o")
library(h2o)
install.packages("randomForest")
library(randomForest)
install.packages("dplyr")
library(dplyr)
install.packages("ranger")
library(ranger)
install.packages("ipred")
library(ipred)
install.packages("caret")
library(caret)

# for reproduciblity
install.packages("randomForest")
library(randomForest)
set.seed(7) #king of kings

# default RF model
rtree1 <- randomForest(
  formula = sales_Social.Media ~ .,
  data    = boosh_train
)

rtree1
plot(rtree1)

# number of trees with lowest MSE
which.min(rtree1$mse)
## [1] 366

# RMSE of this optimal random forest
sqrt(rtree1$mse[which.min(rtree1$mse)])
## [1] 48.5194

# create training and validation data 
install.packages("rsample")     
library(rsample)     # data splitting 
set.seed(7)
valid_split <- initial_split(boosh_train, .8)

# training data
boosh_train_v2 <- analysis(valid_split)

# validation data
boosh_valid <- assessment(valid_split)
x_test <- boosh_valid[setdiff(names(boosh_valid), "sales_Social.Media")]
y_test <- boosh_valid$sales_Social.Media

rf_oob_comp <- randomForest(
  formula = sales_Social.Media ~ .,
  data    = boosh_train_v2,
  xtest   = x_test,
  ytest   = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

install.packages("ggplot2")
library(ggplot2)
# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")

# nboosh of features
features <- setdiff(names(boosh_train), "sales_Social.Media")

set.seed(7)
rtree2 <- tuneRF(
  x          = boosh_train[features],
  y          = boosh_train$sales_Social.Media,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

# rF speed
system.time(
  boosh_randomForest <- randomForest(
    formula = sales_Social.Media ~ ., 
    data    = boosh_train, 
    ntree   = 500,
    mtry    = floor(length(features) / 3)
  )
)
##  user  system elapsed 
##  3.93   0.00   4.20

# ranger speed
install.packages("ranger")
library(ranger)
system.time(
  boosh_ranger <- ranger(
    formula   = sales_Social.Media ~ ., 
    data      = boosh_train, 
    num.trees = 500,
    mtry      = floor(length(features) / 3)
  )
)
##   user  system elapsed 
##   2.05   0.00   0.70

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 30, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)
## [1] 96

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = sales_Social.Media ~ ., 
    data            = boosh_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 7
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# hyperparameter grid search --> same as above but with increased mtry values
hyper_grid_2 <- expand.grid(
  mtry       = seq(50, 133, by = 25),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE  = 0
)

# perform grid search
for(i in 1:nrow(hyper_grid_2)) {
  
  # train model
  model <- ranger(
    formula         = sales_Social.Media ~ ., 
    data            = boosh_train, 
    num.trees       = 500,
    mtry            = hyper_grid_2$mtry[i],
    min.node.size   = hyper_grid_2$node_size[i],
    sample.fraction = hyper_grid_2$sampe_size[i],
    seed            = 7
  )
  
  # add OOB error to grid
  hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid_2 %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = sales_Social.Media ~ ., 
    data            = boosh_train, 
    num.trees       = 500,
    mtry            = 24,
    min.node.size   = 5,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 10)

optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(50) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 50 important variables")


# start up h2o (I turn off progress bars when creating reports/tutorials)
install.packages("h2o")
library(h2o)
h2o.no_progress()
h2o.init(max_mem_size = "5g")
##  Connection successful!

# create feature names
y <- "sales_Social.Media"
x <- setdiff(names(boosh_train), y)

# turn training set into h2o object
h2o.init()
train.h2o <- as.h2o(boosh_train)

# hyperparameter-grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(20, 30, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# grid-search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# sort results metric select
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(1, 500, by = 50),
  mtries      = seq(15, 35, by = 5),
  max_depth   = seq(1, 40, by = 1),
  min_rows    = seq(1, 50, by = 1),
  nbins       = seq(1, 30, by = 5),
  sample_rate = c(.55, .632, .75)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 30*60
)

# grid search2 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

# results2
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid2", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf2)

# top model_id  by validation error
best_model_id <- grid_perf2@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

best_model_id
best_model

#  model performance test set
boosh_test.h2o <- as.h2o(boosh_test)
best_model_perf <- h2o.performance(model = best_model, newdata = boosh_test.h2o)
best_model_perf

# RMSE of best model
install.packages("dplyr")
library(dplyr)
h2o.mse(best_model_perf) %>% sqrt()
## [1] 23.06842

install.packages("randomForest")
library(randomForest)
# randomForest
pred_randomForest <- predict(boosh_randomForest, boosh_test)
head(pred_randomForest)
#  171      172      187      202      205      206 
# 35.88357 42.16317 45.63373 65.64620 43.85263 46.53123 


install.packages("ranger")
library(ranger)
# ranger
pred_ranger <- predict(boosh_ranger, boosh_test)
head(pred_ranger$predictions)
## [1] 34.75440 41.91300 46.69620 66.94273 43.50350 48.66003

# h2o
install.packages("h2o")
library(h2o)
h2o.init()
pred_h2o <- predict(best_model, boosh_test.h2o)
head(pred_h2o)
tail(pred_h2o)

#> head(pred_h2o)
#predict
#1 33.79143
#2 42.68857
#3 47.19429
#4 59.02286
#5 43.30571
#6 48.97429
#> tail(pred_h2o)
#predict
#1  17.35143
#2  18.65143
#3  40.58571
#4 464.05714
#5  37.79429
#6  10.41714

pred_h2o=as.numeric(pred_h2o)
pred_h2o[1:111]







