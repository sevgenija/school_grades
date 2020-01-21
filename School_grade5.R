## Load packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

student_grade <- read.csv("input/student-por.csv", sep = ";")

View(student_grade) #getting an impression of the dataset
summary(student_grade) 

anyNA(student_grade) #no missing values were found.

student_grade$G1 <- NULL #deleting column 'grade first period'
student_grade$G2 <- NULL #deleting column 'grade second period'
student_grade$school <- NULL #deleting column 'student's school'
student_grade$failures <- NULL #deleting failures

student_grade$G3[student_grade$G3 == 0] <- "Fail"
student_grade$G3[student_grade$G3 == 1] <- "Fail"
student_grade$G3[student_grade$G3 == 2] <- "Fail"
student_grade$G3[student_grade$G3 == 3] <- "Fail"
student_grade$G3[student_grade$G3 == 4] <- "Fail"
student_grade$G3[student_grade$G3 == 5] <- "Fail"
student_grade$G3[student_grade$G3 == 6] <- "Fail"
student_grade$G3[student_grade$G3 == 7] <- "Fail"
student_grade$G3[student_grade$G3 == 8] <- "Fail"
student_grade$G3[student_grade$G3 == 9] <- "Fail"
student_grade$G3[student_grade$G3 == 10] <- "Pass"
student_grade$G3[student_grade$G3 == 11] <- "Pass"
student_grade$G3[student_grade$G3 == 12] <- "Pass"
student_grade$G3[student_grade$G3 == 13] <- "Pass"
student_grade$G3[student_grade$G3 == 14] <- "Pass"
student_grade$G3[student_grade$G3 == 15] <- "Pass"
student_grade$G3[student_grade$G3 == 16] <- "Pass"
student_grade$G3[student_grade$G3 == 17] <- "Pass"
student_grade$G3[student_grade$G3 == 18] <- "Pass"
student_grade$G3[student_grade$G3 == 19] <- "Pass"
student_grade$G3[student_grade$G3 == 20] <- "Pass"

student_grade <- student_grade %>%
   rename(final_grade = G3) #renaming column G3

summary(student_grade)
str(student_grade)

student_grade <- student_grade %>%
  mutate(final_grade = relevel(factor(final_grade), ref = "Fail"))
#changing reference dependent variable and converting into factor. 

str(student_grade)

##########################################################################
#barplot sex
ggplot(data = student_grade, aes(x = final_grade, fill = sex)) + 
  geom_bar(position=position_dodge()) +
  labs(title = "Distribution of gender", y = "Frequency") +
  scale_fill_manual(values = c("F"= "deeppink", "M" = "deepskyblue3"),
                    labels = c("F" = "Women", "M" = "Men")) +
  theme_classic() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, 
                                  margin = unit(c(0, 0, 0.6, 0), "cm")), 
        axis.title.x = element_text(size = 14, 
                                    margin = unit(c(0.3, 0, 0, 0), "cm")),
        axis.title.y = element_text(size = 14, 
                                    margin = unit(c(0, 0.5, 0, 0), "cm")))

##########################################################################
#boxplot age
ggplot(data = student_grade, aes(x = final_grade, y = age, fill = final_grade)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("red3", "green4")) +
  labs(title = "Distribution of age for fail/pass") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                  margin = unit(c(0, 0, 0.3, 0), "cm")), 
        axis.title.x = element_text(size = 10, 
                                    margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.title.y = element_text(size = 10, 
                                    margin = unit(c(0, 0.5, 0, 0), "cm")),
        legend.position="none")

##########################################################################
#boxplot Educational level father
ggplot(data = student_grade, aes(x = final_grade, y = Fedu, fill = final_grade)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("red3", "green4")) +
  labs(title = "Distribution father's education fail/pass", y = "father's education") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                  margin = unit(c(0, 0, 0.3, 0), "cm")), 
        axis.title.x = element_text(size = 10, 
                                    margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.title.y = element_text(size = 10, 
                                    margin = unit(c(0, 0.5, 0, 0), "cm")),
        legend.position="none")
  
##########################################################################
#boxplot educational level mother
ggplot(data = student_grade, aes(x = final_grade, y = Medu, fill = final_grade)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("red3", "green4")) +
  labs(title = "Distribution mother's education fail/pass", y = "mother's education") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                  margin = unit(c(0, 0, 0.3, 0), "cm")), 
        axis.title.x = element_text(size = 10, 
                                    margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.title.y = element_text(size = 10, 
                                    margin = unit(c(0, 0.5, 0, 0), "cm")),
        legend.position="none")

##########################################################################
#boxplot study time
ggplot(data = student_grade, aes(x = final_grade, y = studytime, fill = final_grade)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("red3", "green4")) +
  labs(title = "Distribution studytime fail/pass", y = "studytime in hours") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                  margin = unit(c(0, 0, 0.3, 0), "cm")), 
        axis.title.x = element_text(size = 10, 
                                    margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.title.y = element_text(size = 10, 
                                    margin = unit(c(0, 0.5, 0, 0), "cm")),
        legend.position="none")

##########################################################################
#boxplot health
ggplot(data = student_grade, aes(x = final_grade, y = health, fill = final_grade)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("red3", "green4")) +
  labs(title = "Distribution health fail/pass", y = "health status") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                  margin = unit(c(0, 0, 0.3, 0), "cm")), 
        axis.title.x = element_text(size = 10, 
                                    margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.title.y = element_text(size = 10, 
                                    margin = unit(c(0, 0.5, 0, 0), "cm")),
        legend.position="none")
##########################################################################
#barplot paid
ggplot(data = student_grade, aes(x = final_grade, fill = paid)) + 
  geom_bar(position=position_dodge()) +
  labs(title = "Distribution of extra paid classes", y = "Frequency") +
  scale_fill_manual(values = c("no"= "steelblue2", "yes" = "darkorange2")) +
  theme_classic() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, 
                                  margin = unit(c(0, 0, 0.6, 0), "cm")), 
        axis.title.x = element_text(size = 14, 
                                    margin = unit(c(0.3, 0, 0, 0), "cm")),
        axis.title.y = element_text(size = 14, 
                                    margin = unit(c(0, 0.5, 0, 0), "cm")))

##########################################################################
##creating a training- and testset
##convert variables into factors
names <- c('sex' ,'address', 'famsize', 'Pstatus', 'Medu', 'Fedu','Mjob', 'Fjob', 'reason', 'guardian',
           'schoolsup', 'famsup', 'paid','activities','nursery','internet',
           'romantic', 'famrel', 'freetime', 'goout', 'Dalc','higher', 'Walc', 'health', 'absences' )
student_grade[,names] <- lapply(student_grade[,names] , factor)

set.seed(1)
trn_index = createDataPartition(y = student_grade$final_grade, p = 0.70, list = FALSE)
trn_student_grade = student_grade[trn_index, ]
tst_student_grade = student_grade[-trn_index, ]

table(trn_student_grade$final_grade)
table(tst_student_grade$final_grade)

##up-scaling to improve imbalance in data
up_train <- upSample(x = trn_student_grade [,-ncol(trn_student_grade)],
                         y = trn_student_grade$final_grade)
up_test <- upSample (x= tst_student_grade [,-ncol(tst_student_grade)],
                         y = tst_student_grade$final_grade)

table(up_train$Class)
table(up_test$Class)

##Running the knn model
set.seed(1)

student_grade_knn = train(Class ~., method = "knn", data = up_train,
              trControl = trainControl(method = 'cv', number = 5, classProbs = TRUE),
              preProcess = c("center", "scale"))

student_grade_knn$metric
student_grade_knn
summary(student_grade_knn$results)

##Getting insights into the sensitivity, accuracy and specificity. 
set.seed(1)
predicted_outcomes_knn <- predict(student_grade_knn, up_test)

up_test$Class =as.factor(up_test$Class)
knn_confm <- confusionMatrix(predicted_outcomes_knn, up_test$Class)
knn_confm

##########################################################################
##Logistic regression 


set.seed(1)
student_grade_lgr = train(Class ~., method = "glm", 
                      family = binomial(link = "logit"), 
                      data = up_train,
                      trControl = trainControl(method = 'cv', number = 5,
                                               classProbs = TRUE, 
                                               summaryFunction = prSummary))

summary(student_grade_lgr)


##Confusion matrix. Getting insights into the sensitivity, accuracy and specificity. 

predicted_outcomes_lgr <- predict(student_grade_lgr, up_test)

lgr_confm <- confusionMatrix(predicted_outcomes_lgr, up_test$Class)
lgr_confm

##########################################################################

##Decision tree
set.seed(1)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
student_grade_DT = train(Class ~., method = "rpart", 
                         data = up_train,
                         parms = list(split = "information"),
                         trControl = trctrl,
                         tuneLength = 10)

student_grade_DT$finalModel
summary(student_grade_DT)

##Getting insights into the sensitivity, accuracy and specificity. 

predicted_outcomes_DT <- predict(student_grade_knn, up_test)

up_test$Class =as.factor(up_test$Class)
DT_confm <- confusionMatrix(predicted_outcomes_DT, up_test$Class)

DT_confm

prp(student_grade_DT$finalModel, box.palette = "Reds", tweak = 1.2)