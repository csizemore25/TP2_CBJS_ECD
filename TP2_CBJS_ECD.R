library(tidyverse)
vg_sales=read.csv("vgsales.csv")
vg=head(vg_sales, n=50)

view(vg)
length(vg)
names(vg)

vgsales=subset(vg, select=-c(Rank))
view(vgsales)

vgsales$Year=as.numeric(as.character((vgsales$Year)))
class(vgsales$Year)

#Summary Statistics

paste("Categorical variables are 'Name', 'Platform', 'Genre', and 'Publisher'")
paste("Continuous variables are 'Year', 'NA_Sales', 'EU_Sales, 'JP_Sales', 'Other_Sales', and 'Global_Sales'")

summary(vgsales)

range(vgsales$Year)
IQR(vgsales$Year)
var(vgsales$Year)

range(vgsales$NA_Sales)
IQR(vgsales$NA_Sales)
var(vgsales$NA_Sales)

range(vgsales$Global_Sales)
IQR(vgsales$Global_Sales)
var(vgsales$Global_Sales)

#install.packages("dplyr")
library(dplyr)

name=vgsales %>% 
  count(Name)
view(name)

platform=vgsales %>% 
  count(Platform)
view(platform)

genre=vgsales %>% 
  count(Genre)
view(genre)

publisher=vgsales %>% 
  count(Publisher)
view(publisher)

#Correlation Tables

names(vgsales)
library(reshape2)

heat=vgsales[,c(6:10)]
head(heat)
corr=round(cor(heat), 5) 

melted_corr=melt(corr)
head(melted_corr)
png("heat_map.png")
ggplot(data=melted_corr, aes(Var1, Var2, fill=value))+geom_tile()+
  labs(title="Heat Map")
dev.off() 

#Graphical Exploration

png("year_hist.png")
ggplot(data=vgsales, aes(x=Year))+geom_histogram(fill="red")+
  labs(title="Year Historgram", x="Year", y="Count")
dev.off()

png("genre_bar.png")
ggplot(data=vgsales, aes(x=Genre))+geom_bar(fill="blue")+
  labs(title="Genre Bar Char", x="Genre", y="Count")
dev.off()

png("platform_bar.png")
ggplot(data=vgsales, aes(x=Platform))+geom_bar(fill="green")+
  labs(title="Platform Bar Chart", x="Platform", y="Count")
dev.off()

png("publisher_bar.png")
ggplot(data=vgsales, aes(x=Publisher))+geom_bar(fill='red')+
  labs(title="Publisher Bar Chart", x="Publisher", y="Count")
dev.off()

vgdf=data.frame(vgsales$Year, vgsales$NA_Sales, vgsales$EU_Sales, vgsales$JP_Sales, vgsales$Other_Sales, vgsales$Global_Sales)
png("pair_plot.png")
pairs(vgdf)
dev.off()

reg1=lm(Year~NA_Sales, data=vgsales)
png("reg1.png")
plot(Year~NA_Sales, data=vgsales)+abline(reg1)+
dev.off()

reg2=lm(Global_Sales~NA_Sales, data=vgsales)
png("reg2.png")
plot(Global_Sales~NA_Sales, data=vgsales)+abline(reg2)
dev.off()

png("box1.png")
ggplot(data=vgsales, aes(Platform, Global_Sales))+geom_boxplot(fill='red')+labs(title="Boxplot of Platform vs Global Sales")
dev.off()

png("box2.png")
ggplot(data=vgsales, aes(Platform, NA_Sales))+geom_boxplot(fill='blue')+labs(title="Boxplot of Platform vs North American Sales")
dev.off()

png("scatter_global.png")
ggplot(data=vgsales, aes(Global_Sales, Platform, color=Publisher))+geom_point(size=2)+
labs(title="Global Sales per Platform by Publisher")
dev.off()

png("scatter_na.png")
ggplot(data=vgsales, aes(NA_Sales, Platform, color=Publisher))+geom_point(size=2)+
  labs(title="North American Sales per Platform by Publisher")
dev.off()
