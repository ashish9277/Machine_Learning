library(dplyr)

library(ggplot2)
library(fpc)

library(caret)
install.packages('glmnet')
library(glmnet)
install.packages('ranger')
library(ranger)

library(e1071)
install.packages('clValid')
library(clValid)

train <- read.csv('train.csv' , sep = ',' , header = TRUE , stringsAsFactors = FALSE)
train$dataset <- 'train'


test <- read.csv('test.csv' , header=TRUE , sep=',' , stringsAsFactors = FALSE)
test$dataset <-  'test'


full <- bind_rows(train , test)

str(full)

summary(full)



factor_variables <-  c('id','color' , 'type' , 'dataset')

full[factor_variables] <- lapply(full[factor_variables], function(x) as.factor(x))

train2 <-  full[full$dataset == 'train' , ]


ggplot(train2 , aes(x=type , y=bone_length, fill=type)) + geom_boxplot()+ guides(fill=FALSE) + xlab('Creature') + ylab('Bone Length') + scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"))



ggplot(train2 , aes(x=type , y=rotting_flesh)) + geom_boxplot() + guides(fill=FALSE) + xlab('Creature') + ylab('Rotting Flesh')



ghost_color <- train2 %>% filter(type == 'Ghost') %>% group_by(color) %>% summarise(count = n())

ggplot(ghost_color , aes(x=color , y=count ,  fill=color)) + geom_bar(stat = 'identity') + xlab('Color') + ylab('Count') + scale_fill_manual(values = c("Black", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#999999")) + guides(fill = FALSE)


pairs(full[,2:5], col = full$type, labels = c("Bone Length", "Rotting Flesh", "Hair Length", "Soul"))


full <- full %>% mutate(hair_soul = hair_length*has_soul)

colnames(full)


full_1 <- full %>% filter(!is.na(type))

ggplot(full_1 , aes(x=type , y=hair_soul , fill=type)) + geom_boxplot() + xlab('Creature') + ylab('Combination of Hair/Soul') + guides(fill= FALSE) + scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"))

full <- full%>% 
  mutate(bone_flesh = bone_length * rotting_flesh,
  bone_hair = bone_length * hair_length,
  bone_soul = bone_length * has_soul,
  flesh_hair = rotting_flesh * hair_length,
  flesh_soul = rotting_flesh * has_soul)


summary(full)


#clustering using kmeans method

set.seed(100)
creature_labels <- full$type
full2 <- full
full2$type <- NULL

full2$id <- NULL
full2$color <- NULL
full2$dataset <- NULL

creature_km_1 <-  kmeans(full2 , 3 , nstart = 30)

summary(creature_km_1)
plotcluster(full2 , clvecd=creature_km_1$cluster)
?plotcluster



dunn_ckm_1 <- dunn(clusters = creature_km_1$cluster, Data = full2)
dunn_ckm_1


table(creature_km_1$cluster, creature_labels)
