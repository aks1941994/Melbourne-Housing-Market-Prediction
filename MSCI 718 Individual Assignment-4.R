#Calling libraries

library(dplyr)
library(tidyverse)
library(ggplot2)

#Reading the data set

setwd('E:/PostGrad/UW Winter Term/MSCI 718/Assignments/Individual Assignment-4')
main_reg_data <- read_csv("melb_data.csv")

#Data Wrangling
model_1_data <- select(main_reg_data, Price, Suburb, SellerG, Method)
model_2_data <- select(main_reg_data, Price, Suburb, Type, Bathroom)
model_3_data <- select(main_reg_data, Price, Suburb, Type, Car)
model_4_data <- select(main_reg_data, Price, Suburb, Type, SellerG)
model_5_data <- select(main_reg_data, Price, Suburb, Rooms, Bedroom2, Bathroom, Propertycount, Regionname, CouncilArea)
model_6_data <- select(main_reg_data, Price, Suburb, Rooms, Bedroom2, Bathroom, Regionname, Car, Landsize)
model_7_data <- select(main_reg_data, Price, Suburb, Rooms, SellerG, Type, Bathroom, Car, Method)
model_8_data <- select(main_reg_data, Price, Suburb, Rooms, SellerG, Type, Bathroom, Landsize, Bedroom2)
model_1_data <- remove_missing(model_1_data)
model_2_data <- remove_missing(model_2_data)
model_3_data <- remove_missing(model_3_data)
model_4_data <- remove_missing(model_4_data)
model_5_data <- remove_missing(model_5_data)
model_6_data <- remove_missing(model_6_data)
model_7_data <- remove_missing(model_7_data)
model_8_data <- remove_missing(model_8_data)

#Models will analyze Melbourne areas only!
model_1_data <- model_1_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_1_data <- model_1_data %>% group_by(Suburb, SellerG)
model_2_data <- model_2_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_2_data <- model_2_data %>% group_by(Suburb, Type)
model_3_data <- model_3_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_3_data <- model_3_data %>% group_by(Suburb, Type)
model_4_data <- model_4_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_4_data <- model_4_data %>% group_by(Suburb, Type)
model_5_data <- model_5_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_5_data <- model_5_data %>% group_by(Suburb, Rooms)
model_6_data <- model_6_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_6_data <- model_6_data %>% group_by(Suburb, Rooms)
model_7_data <- model_7_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_7_data <- model_7_data %>% group_by(Suburb, Rooms)
model_8_data <- model_8_data %>%
  filter(str_detect(Suburb, 'Melbourne'))
model_8_data <- model_8_data %>% group_by(Suburb, Rooms)






#Comparing house price and area categories wise

ggplot(model_1_data, aes(x = Suburb,y= Price)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(model_1_data, aes(x = Suburb,y= Price)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(model_1_data, aes(fill=SellerG, y=Price, x=Suburb)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Suburb Area vs Prices", 
       x="Suburb Area", y = "Price") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplot(model_1_data, aes(fill=Method, y=Price, x=Suburb)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Suburb Area vs Prices", 
       x="Suburb Area", y = "Price") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Modelling

model_1 <- lm(Price ~ Suburb + SellerG + Method, data = model_1_data)
summary(model_1)
model_2 <- lm(Price ~ Suburb + Type +  Bathroom , data = model_2_data)
summary(model_2)
model_3 <- lm(Price ~ Suburb + Type +  Car , data = model_3_data)
summary(model_3)
model_4 <- lm(Price ~ Suburb + Type +  SellerG , data = model_4_data)
summary(model_4)
model_5 <- lm(Price ~ Suburb + Rooms + Bedroom2 + Bathroom + Propertycount + Regionname + CouncilArea, data = model_5_data)
summary(model_5)
model_6 <- lm(Price ~ Suburb + Rooms + Bedroom2 + Bathroom + Regionname + Car + Landsize , data = model_6_data)
summary(model_6)
model_7 <- lm(Price ~ Suburb + Rooms  + Type + Bathroom + Car , data = model_7_data)
summary(model_7)
model_8 <- lm(Price ~ Suburb + Rooms + Type + Bathroom  , data = model_8_data)
summary(model_8)

#Checking assumptions
par(mfrow = c(2, 2))
plot(model_7)

#Detecting Multicollinearity
car::vif(model_7)
