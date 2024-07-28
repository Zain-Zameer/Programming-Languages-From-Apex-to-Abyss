# Project: Rise and Fall of programming languages
library("readr")
library("dplyr")
library("ggplot2")
by_tag_year <- read_csv("by_tag_year.csv")
head(by_tag_year)

by_tag_year_fraction <- mutate(by_tag_year,fraction=number/year_total)
by_tag_year_fraction

r_over_time <- filter(by_tag_year_fraction,tag=="r")
r_over_time

#plotting
ggplot(data=r_over_time,aes(x=year,y=fraction))+geom_line()+labs(title="Line Plot of R language",subtitle = "Total questions over time")

ggplot(data=r_over_time,aes(x=year,y=fraction))+geom_bar(stat="identity")+labs(title="Line Plot of R language",subtitle = "Total questions over time")


selected_tags <- c("r","dplyr","ggplot2")
selected_tags_over_time <- filter(by_tag_year_fraction,tag %in% selected_tags)
selected_tags_over_time

ggplot(data=selected_tags_over_time,aes(x=year,y=fraction,color=tag))+geom_line()+labs(title="R Tools Analysis",subtitle="Analyzing the Rise of dplyr, readr, and R Language",x="Years",y="Percentage of questions")

tags <- by_tag_year %>%
            group_by(tag) %>%
            summarize(tag_total=sum(number))
tags

sorted_tags <- arrange(tags,desc(tag_total))
sorted_tags

highest_tags <- c("javascript","java","c#","php")
highest_tags_data <- filter(by_tag_year_fraction,tag %in% highest_tags)
highest_tags_data

ggplot(data=highest_tags_data,aes(x=year,y=fraction,color=tag))+geom_line()+labs(title="Programming Languages",subtitle="Analysis on top highest programming languages",x="Years",y="Percentage of questions")
ggplot(data=highest_tags_data,aes(x=tag,y=fraction))+geom_bar(stat="identity")

collected_data <- c("android","ios","windows-phone")
new_data <- filter(by_tag_year_fraction,tag %in% collected_data)
new_data

ggplot(data=new_data,aes(x=year,y=fraction,color=tag))+geom_line()
