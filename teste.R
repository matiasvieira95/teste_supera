#### teste supera - Matias S. Vieira

## packages
library(readxl)
library(dplyr)
library(ggplot2)
library(rcompanion)
library(caret)
library(e1071)
library(car)

## importing data
setwd('C:\\Users\\matias.vieira\\Downloads\\supera-data-test')
data = read_xlsx('bechdel.xlsx', range = "A2:J1502", na = "NA") 
attach(data)
data <- na.omit(data)

## descriptives
str(data)

data %>% group_by(binary) %>% summarize(mean=mean(budget, na.rm = T))
data %>% group_by(binary) %>% summarize(mean=mean(budget_2013, na.rm = T))


data %>% group_by(binary) %>% summarize(mean=mean(domgross, na.rm = T))
data %>% group_by(binary) %>% summarize(mean=mean(domgross_2013, na.rm = T))

data %>% group_by(binary) %>% summarize(mean=mean(intgross, na.rm = T))
data %>% group_by(binary) %>% summarize(mean=mean(intgross_2013, na.rm = T))



prop.table(table(binary))*100

## 55.2% falham no teste de Bechdel

ggplot(data, aes(x = binary)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)*100), width = 0.5, fill = "#619CFF") + 
  labs(x = "Overall Bechdel Test result", y = "Percentage")

data2 <- data %>% 
  group_by(year) %>% 
  summarize(total=n(),prop=(sum(binary=="PASS")/n())*100)

ggplot(data=data2, aes(x=year, y=prop)) +
  geom_line(color = "#619CFF") + geom_point(color = "#619CFF") +
  labs(x = "Year", y = "% of passing movies")



### modelling
data$binary = ifelse(data$binary == "PASS", 1, 0)
str(data$binary)

n <- nrow(data)
n_train <- round(0.90 * n)
set.seed(2304)
train_indices <- sample(1:n, n_train)
train <- data[train_indices, ]
test <- data[-train_indices, ]

model1 <- glm(binary ~ year + budget + intgross, 
              data = train, family = "binomial"(link = 'logit'))

summary(model1)
nagelkerke(model1)
vif(model1)


## prediction test
pred <- predict(model1, test, type = "response") 
predicted <- round(pred)

tab <- table(Predicted = predicted, Reference = test$binary)
tab


# ## svm
# fit.svm <- train(as.factor(binary) ~ year + budget + intgross, 
#                 data = train, method="svmRadial")
# print(fit.svm)
# 
# pred2 <- predict(fit.svm, test) 
# tab2 <- table(Predicted = pred2, Reference = test$binary)
# tab2

detach(data)
## classificating
films = read.csv2("test.csv", sep = ",", na = "NA")
films <- na.omit(films)
str(films)

films$domgross = as.numeric(films$domgross)
films$intgross = as.numeric(films$intgross)
str(films)

## class

outcome <- predict(model1, films, type = "response") 
predicted <- round(outcome)

export <- data.frame(cbind(films$imdb, predicted))
colnames(export) <- c("imdb", "classification")

write.csv2(export, "result.csv", row.names = FALSE)
