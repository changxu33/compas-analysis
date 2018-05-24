library(dplyr)
library(ggplot2)
raw_data <- read.csv("./compas-scores-two-years.csv")
head(raw_data)
# if compass score <=5, ->0, if >5 , ->1
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, 
                    sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, 
                    two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')

df.compass.low<-df[df$decile_score<=5,]
nrow(df.compass.low)
df.compass.low$pred<-0
head(df.compass.low)
nrow(df.compass.low)
df.compass.high<-df[df$decile_score>5,]
df.compass.high$pred<-1
df.compass<-rbind(df.compass.low,df.compass.high)
head(df.compass)
tail(df.compass)
df.compass.new<-df.compass%>%filter(is_recid==pred)
head(df.compass.new)
tail(df.compass.new)
df.compass.new$mis<-0
head(df.compass.new)
df.compass.new.diff<-df.compass%>%filter(is_recid!=pred)
df.compass.new.diff$mis<-1
df.compass.new<-rbind(df.compass.new,df.compass.new.diff)
head(df.compass.new)
tail(df.compass.new)
nrow(df.compass.new)
df.compass.new <- mutate(df.compass.new, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race)) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))
model <- glm(mis ~ gender_factor + age_factor + race_factor +
               priors_count + crime_factor+ race_factor:age_factor , family="binomial", data=df.compass.new )
#model <- randomForest::randomForest(mis ~ gender_factor + age_factor + race_factor +priors_count + crime_factor, data = df.compass.new)
summary(model)
#plot(model)
#write.csv(df.compass.new, file = "MyData.csv")
#binned residual plot
#install.packages('arm')
library(arm)
binnedplot(model$fitted.values ,model$residuals)
#subrows <- model$fitted.values > 0.68 & model$fitted.values < .72
#mean(df.compass.new$mis[subrows] - model$fitted.values[subrows])
#df.compass.new[subrows,]
#plot(df.compass.new$priors_count, model$fitted.values)
#hist( model$fitted.values)
logLoss <- with(df.compass.new, mis * (model$fitted.values - mean(mis)) - 
                  (1 - mis) * (1 - model$fitted.values - (1 - mean(mis))))
# red , wrong; bule, correct
plot(logLoss, pch = 20, col = ifelse(df.compass.new$mis == 1, "red", "blue"))
rpart::rpart(model$fitted.values ~ gender_factor + age_factor + race_factor +
               priors_count + crime_factor, data = df.compass.new)

model1 <- randomForest::randomForest(mis ~ gender_factor + age_factor + race_factor +priors_count + crime_factor, data = df.compass.new)
binnedplot(model1$predicted, df.compass.new$mis - model1$predicted)
plot(model1$predicted, df.compass.new$mis - model1$predicted)
plot(model1$predicted, df.compass.new$mis - model1$predicted, pch = 20, cex = 2, col = rgb(0,0,0,0.5))
plot(model1$predicted, df.compass.new$mis - model1$predicted, pch = 20, cex = 2, col = rgb(0,0,0,0.5))
plot(model1$predicted, df.compass.new$mis - model1$predicted, pch = 20, cex = 2, col = rgb(0,0,0,0.2))
plot(model1$predicted, runif(length(model1$predicted), -0.05, 0.05) + df.compass.new$mis - model1$predicted, pch = 20, cex = 2, col = rgb(0,0,0,0.2))
dev.new()
#plot(df.compass.new$priors_count, logLoss, pch = 20, col = ifelse(df.compass.new$mis == 1, "red", "blue"))
