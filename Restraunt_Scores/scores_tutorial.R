#use this file for syracuse

library(ggplot2)
library(ggmap)
library(gridExtra)
library(dplyr)

data = read.csv('Restaurant_Scores.csv')

colnames(data)

unique(data$risk_category)

risk<-function(x){
  if(x==''){
    return('No Risk')
  }
  else return(as.character(x))
}

df = data
df$year<-as.numeric(format(as.Date(df$inspection_date,'%m/%d/%Y'),'%Y'))
df$month<-as.numeric(format(as.Date(df$inspection_date,'%m/%d/%Y'),'%m'))
df$day<-as.numeric(format(as.Date(df$inspection_date,'%m/%d/%Y'),'%g'))
df$risk<-sapply(df$risk_category,risk)

df2 <-  as.data.frame(df %>% group_by(year,month,risk) %>% summarise(number = n()))

perYear = as.data.frame(df2 %>% group_by(risk,year) %>% select(number) %>% summarise(tot=sum(number)))
ggplot(data=perYear,aes(x=year,y=tot, fill=risk)) + geom_bar(stat="identity",position="dodge") + ylab("Number of violations") + geom_text(aes(label=tot), position=position_dodge(width=0.9), vjust=-0.25)



as.data.frame(df2 %>% group_by(year) %>% filter(risk!='No Risk') %>% select(number) %>% summarise(tot_risk = sum(number)))

as.data.frame(df2 %>% group_by(year) %>% filter(risk!='No Risk') %>% select(number))


colnames(df2)

df2$month_name<-month.abb[df2$month]

lev <- unique(df2$month_name)
lev


#ordering according to month
df2$ordered_month <- factor(df2$month_name , levels = lev)

head(df2)


ggplot(data=df2 , aes(year, ordered_month)) + geom_tile(aes(fill=number) , color= 'white') + scale_fill_gradient(low='white' , high='red') + facet_grid(~risk)

tt <- map_data('county' , 'california' , 'San Francisco')

tt<-map_data('county', 'california,san francisco')





highrisk <- filter(df2$risk == 'High Risk')



highrisk <- filter(df, risk=='High Risk')

highrisk <- na.omit(highrisk)


sfMap<-ggplot() + geom_polygon(data=tt, aes(x=long, y=lat, group = group),colour='white',alpha=.5)



sfMap + stat_density2d(aes(x = business_longitude, y = business_latitude, fill = ..level..),size = 1, bins = 10, data = highrisk) + geom_point(data=highrisk, aes(x = business_longitude, y = business_latitude)) + facet_wrap(~year)



test_map <-map_data('county', 'california,san francisco')

?map_data

map_data('state', 'India')
