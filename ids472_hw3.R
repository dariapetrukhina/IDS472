

df <- read.csv('/Users/dariapetrukhina/Downloads/covid_19_dataset.csv')
str(df)

#PART 1 
#a) How many people in this dataset tested positive for COVID-19? How many tested negative for
#COVID-19? Offer a possible explanation for the large difference between these numbers.
library(dplyr)
table(df["corona_result"])
# I can assume that the number of negative cases is much larger because people are being tested more often. For example, people who do not work from home are being tested at least one time in two weeks. Moreover, a lot of people are getting vaccine

#b) In preparation for our analysis, create a new dataset which removes any observations which satisfy
#corona_result = other. For the remaining observations, convert corona_result into a numeric
#variable that equals the number 1 if the person tested positive and 0 otherwise. Finally, remove
#any observations with missing values for age_60_and_above and gender.

df2 <-df[!(df$corona_result=="other"),]
lookup <- c("negative" = 0, "positive" = 1)
df2$corona_result <- lookup[df2$corona_result]
unique(df2$corona_result)
df3<-df2

# c) Randomly split the data into a train and test set, with approximately 90% of the data in the train
#set. Make sure that the train and test set preserve the relative ratio of positive to negative cases
#Hint: Use the sample.split() function from the caTools library
dt = sort(sample(nrow(df3), nrow(df3)*0.9))
train<-df3[dt,]
test<-df3[-dt,]

#PART 2
#(a) Build a logistic regression model from the training set using the glm() function to predict whether
# a person is positive for COVID-19
library(dplyr)
library(ISLR)
str(df3)
model <- glm(corona_result ~ test_date+cough+fever+sore_throat+shortness_of_breath+head_ache+age_60_and_above+gender, data = train, family = "binomial")
summary(model)
test.prob <- predict(model, newdata = test, type = "response")
# c)Report the confusion matrix of your logistic regression model on the test set when the threshold
# is set to 0.5. Compute the accuracy, true positive rate, and false positive rate for the model.
test.prediction <- ifelse(test.prob >= 0.5, 1, 0)
table(test$corona_result, test.prediction)
tp = 14633
tn = 154
fp = 14
fn = 336
#accuracy
(tp + tn)/(tp+tn+fp+fn)

# b)Report the confusion matrix of your logistic regression model on the train set when the threshold
# is set to 0.5. Compute the accuracy, true positive rate, and false positive rate for the model.
train.prob <- predict(model, newdata = train, type = "response")
train.prediction <- ifelse(train.prob >= 0.5, 1, 0)
table(train$corona_result, train.prediction)
tp = 132028
tn = 1189
fp = 135
fn = 2874
#accuracy
(tp + tn)/(tp+tn+fp+fn)

# c)In general, we say that a model is overfitting if the accuracy of the model on the train set is
#significantly higher than the accuracy of the model on the test set. Based on your answers to parts
#(b) and (c), do you feel that the logistic regression model is overfitting the data?

# I do not think that the model is overfitiing because the difference between accuracy of model on the 
#train set and the model of the test set is 0.9779117-0.9768778 = 0.0010339


# e) Do you believe that this model would be useful in real life? Answer this question by considering
#your estimates of the true positive rate and false positive rates of the model on unseen data (i.e.,
# the true positive rate and false positive rate that you computed on the test set in part (c)).


#true positive rate 
tp / (tp + fn)

# false positive rate
fp / (fp + tn)

# Yes the model would be useful in real life because the accuracy is high 

# (f) Plot the ROC curve of your logistic regression model on the test set using the ROCR library.
install.packages("ROCR")
library(ROCR)
PredictROC = predict(model, newdata = test)
PredictROC
pred = prediction(PredictROC, test$corona_result)
perf = performance(pred, "tpr", "fpr")
plot(perf)


#(g) Propose a threshold value which would ensure that your logistic regression model has a true positive
#rate of at least 50% on the test set.
#Hint: This can be found by experimenting with the threshold parameter.

test.prediction <- ifelse(test.prob >= 0.1, 1, 0)
table(test$corona_result, test.prediction)
tp / (tp + fn)

#(h) Using the coefficients of your logistic regression model, answer the following questions:
#(i) Holding all other independent variables constant, how much would having shortness of breath
#multiply the odds of testing positive for COVID-19?
coefficients(model)  
exp(3.798691463)  

#(ii) Holding all other independent variables constant, how much would having a headache multiply
#the odds of testing positive for COVID-19?
  

#(iii) Holding all other independent variables constant, how much would being over the age of 60
#multiply the odds of testing positive for COVID-19? Based on this number, what can we
#conclude about the relevance of a person’s age in predicting whether or not they have COVID19?
  


#PART 3
#(a) Use the rpart and rpart.plot libraries to train a classification tree using the train set to predict
#whether a person has COVID-19. Provide the plot of the decision tree.
install.packages("rpart")
library(rpart)
tree_model <- rpart(corona_result ~ test_date+cough+fever+sore_throat+shortness_of_breath+head_ache+age_60_and_above+gender, train)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree_model)

#(b) We recall one of the benefits of classification trees is that they are interpretable. Based on your plot,
#explain (in words) how the tree determines whether someone has COVID-19. What independent
#variables does the tree reveal are most important in accurately predicting whether someone has
#COVID-19?

# The most importnat indepedent variables are head_ache and sore_throat
# The tree determines whether the person has COVID-19 by examining each indepedent variable starting with head_ache

#(c)Report the confusion matrix of your classification tree on the test set when the threshold is set to
#0.5. Compute the accuracy, true positive rate, and false positive rate for the tree.

train.prob<- predict(tree_model, newdata = test)

train.prediction <- ifelse(train.prob >= 0.1, 1, 0)
table(test$corona_result, train.prediction)
tp = 14348
tn = 242
fp = 299
fn = 248
#accuracy
(tp + tn)/(tp+tn+fp+fn)

#(d) Use 5-fold cross validation to find a choice for the complexity parameter (cp) that maximizes the
#accuracy of the model. This can be performed using the Caret library. Report the best choice of
#the cp parameter.
install.packages("caret")
install.packages("lattice")
library(caret)
install.packages("e1071")
library(e1071)
formula = corona_result ~ test_date+cough+fever+sore_throat+shortness_of_breath+head_ache+age_60_and_above+gender
model = rpart(formula, data = train, method="class",  minbucket=5)
rpart.plot(model)
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 
numFolds = trainControl( method = "cv", number = 5 )
result = train(formula, 
               data = df3, 
               method="rpart", 
               trControl = train.control,
               tuneGrid = cpGrid)
result
