setwd("~/Desktop/ACTL3142/Assignment Partb")
library('dplyr')
library('caret')
library("ISLR")
library("class")
library("glmnet")
library("ROSE")
library("e1071")
library("leaps")
library("tree")
library("gbm")
library("randomForest")
set.seed(10)
#### functions ####
conv <- function(a,b,c){
  i <- 1
  while(i < length(a)){
    if(a[i] == b){
      a[i] <- c
    }
    i <- i+1
  }
  return(c(as.numeric(a)))
}
datereset <- function(a){
  dates <- as.character(a)
  i <- 1 
  year <- c()
  month <- c()
  date <- c()
  
  while(i<length(dates)+1){
    if(nchar(dates[i]) < 9){
      year[i] <- as.numeric(strsplit(dates[i], "/")[[1]][3])+2000
      month[i] <- as.numeric(strsplit(dates[i], "/")[[1]][2])
      date[i] <- as.numeric(strsplit(dates[i], "/")[[1]][1])
    }else{
      year[i] <- as.numeric(substring(dates[i], c(1,6,9), c(4, 7, 10))[1])
      month[i] <- as.numeric(substring(dates[i], c(1,6,9), c(4, 7, 10))[2])
      date[i] <- as.numeric(substring(dates[i], c(1,6,9), c(4, 7, 10))[3])
    }
    i <- i+1
  }
  b <- c(as.Date(paste(year,month,date, sep = "-"), "%Y-%m-%d"))
}
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
accuracy <- function(actual, predicted){
  mean(actual == predicted)
}
#### importing data ####
med_out_in_ben <- read.csv(file= "Medicare_Outpatient_Inpatient_Beneficiary_PartB.csv")
med_out_in_ben_eval <- read.csv(file= "Medicare_Outpatient_Inpatient_Beneficiary_Eval_PartB.csv")
providers <- read.csv(file= "Medicare_Provider_PartB.csv")
providers_eval <- read.csv(file= "Medicare_Provider_Eval_PartB.csv")


#### Data cleanup ####
med_out_in_ben_df <- merge(med_out_in_ben,providers, by="ProviderID")


#dates
med_out_in_ben_df$ClaimStartDt <- datereset(med_out_in_ben_df$ClaimStartDt)
med_out_in_ben_df$ClaimEndDt <- datereset(med_out_in_ben_df$ClaimEndDt)

#indicators 
med_out_in_ben_df$RenalDiseaseIndicator <- as.character(med_out_in_ben_df$RenalDiseaseIndicator)
med_out_in_ben_df$RenalDiseaseIndicator <- conv(med_out_in_ben_df$RenalDiseaseIndicator,"Y",1)
med_out_in_ben_df$Gender <- conv(med_out_in_ben_df$Gender, 2, 0)
med_out_in_ben_df <- med_out_in_ben_df%>%
  mutate(Fraud = ifelse(Fraud == "No",0,1))
med_out_in_ben_df$DeductibleAmtPaid[is.na(med_out_in_ben_df$DeductibleAmtPaid)] <-0
j <-40
while(j<51){
  med_out_in_ben_df[,j] <- conv(med_out_in_ben_df[,j],2,0)
  j <- j+1
}
colSums(is.na(med_out_in_ben_df))

#### data manipulation ####
training_df <- med_out_in_ben_df%>%
  mutate(ClmDur = as.numeric(ClaimEndDt- ClaimStartDt))%>%
  select(ProviderID,InscClaimAmtReimbursed,DeductibleAmtPaid,IPAnnualReimbursementAmt,IPAnnualDeductibleAmt,
         OPAnnualDeductibleAmt,OPAnnualReimbursementAmt,Fraud,Gender, RenalDiseaseIndicator,ClmDur,
         ChronicCond_Alzheimer,ChronicCond_Cancer, ChronicCond_Heartfailure,ChronicCond_KidneyDisease,
         ChronicCond_Depression,ChronicCond_stroke,ChronicCond_IschemicHeart,ChronicCond_Diabetes,
         ChronicCond_ObstrPulmonary,ChronicCond_Osteoporasis,ChronicCond_rheumatoidarthritis)%>%
  group_by(ProviderID)%>%
  summarise(InscClaimAmtReimbursed=mean(InscClaimAmtReimbursed),
            DeductibleAmtPaid=mean(DeductibleAmtPaid),
            IPAnnualReimbursementAmt=mean(IPAnnualReimbursementAmt), 
            IPAnnualDeductibleAmt=mean(IPAnnualDeductibleAmt),
            OPAnnualDeductibleAmt=mean(OPAnnualDeductibleAmt),
            OPAnnualReimbursementAmt=mean(OPAnnualReimbursementAmt),
            Gender = mean(Gender), RenalDiseaseIndicator = mean(RenalDiseaseIndicator),
            ChronicCond_Alzheimer = mean(ChronicCond_Alzheimer),
            ChronicCond_Cancer = mean(ChronicCond_Cancer),ChronicCond_Heartfailure = mean(ChronicCond_Heartfailure),
            ChronicCond_KidneyDisease = mean(ChronicCond_KidneyDisease), ChronicCond_Depression = mean(ChronicCond_Depression),
            ChronicCond_stroke = mean(ChronicCond_stroke), ChronicCond_IschemicHeart = mean(ChronicCond_IschemicHeart),
            ChronicCond_Diabetes = mean(ChronicCond_Diabetes), ChronicCond_ObstrPulmonary = mean(ChronicCond_ObstrPulmonary),
            ChronicCond_Osteoporasis = mean(ChronicCond_Osteoporasis), 
            ChronicCond_rheumatoidarthritis = mean(ChronicCond_rheumatoidarthritis),
            ClmDur=mean(ClmDur), totalClaims = n(),Fraud=mean(Fraud))

ggplot(training_df) +
  aes(x = Fraud) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Histogram of Fraudulent Providers") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

set.seed(2322)
train <-  c(createDataPartition(training_df$Fraud, p = 0.7, list = FALSE))
training <- training_df[ train, -1]
test <- training_df[-train,-1]


#unbalanced
x_trn <- training[,-c(23)]%>%as.matrix()
y_trn <- as.factor(training$Fraud)

x_test <- test[,-c(22)]%>%as.matrix()
y_test <- as.factor(test$Fraud)

#balanced
x_trn_under <- ovun.sample(Fraud~., data = training, method = "under", N = 283*2)$data%>%as.matrix()
x_trn_over <- ovun.sample(Fraud~., data = training, method = "over", N = 2823*2)$data%>%as.matrix()
x_trn_under_df <- ovun.sample(Fraud~., data = training, method = "under", N = 283*2)$data
x_trn_over_df <- ovun.sample(Fraud~., data = training, method = "over", N = 2822*2)$data

x_trn_over$Fraud <- as.factor(x_trn_over$Fraud)
x_trn_under$Fraud <- as.factor(x_trn_under$Fraud)

#modelling
library("MASS")
#### Logistic Regression ####
log_reg_over <- glm(Fraud ~., data = x_trn_over_df, family = "binomial")
summary(log_reg_over)
pred_log_reg_over <- log_reg_over%>% predict(test, type = "response")
pred_log_reg_over.class <- as.factor(ifelse(pred_log_reg_over > 0.5, 1, 0))

calc_class_err(y_test,pred_log_reg_over.class)
accuracy(y_test,pred_log_reg_over.class)
confusionMatrix(data = pred_log_reg_over.class,reference = y_test)

#finding the best predictors 
#subset 

#### Knn ####
#underbalanced dataset 
for (i in seq_along(k_n)) {
  pred = knn(train = scale(x_trn_under[,-22]), 
             test  = scale(x_test), 
             cl    = x_trn_under[,7], 
             k     = k_n[i])
  err_k[i] = calc_class_err(y_test, pred)
}

# plot error vs choice of k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors underbalanced")
# add line for min error seen
abline(h = min(err_k), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(y_test == 1), col = "grey", lty = 2)

#k value with min error
which(err_k == min(err_k))

# k = 31
pred_knn_under <- knn(train = scale(x_trn_under[,-22]),
                test = scale(x_test),
                cl = x_trn_under[,22],
                k = max(which(err_k == min(err_k))))

confusionMatrix(data =pred_knn_under, reference =  as.factor(test$Fraud))

#overbalanced data
for (i in seq_along(k_n)) {
  pred = knn(train = scale(x_trn_over[,-22]), 
             test  = scale(x_test), 
             cl    = x_trn_over[,22], 
             k     = k_n[i])
  err_k[i] = calc_class_err(y_test, pred)
}

# plot error vs choice of k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors overbalanced")
# add line for min error seen
abline(h = min(err_k), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = mean(y_test == 1), col = "grey", lty = 2)

#k value with min error
which(err_k == min(err_k))

# k = 31
pred_knn_over <- knn(train = scale(x_trn_over[,-22]),
                      test = scale(x_test),
                      cl = x_trn_over[,22],
                      k = max(which(err_k == min(err_k))))

confusionMatrix(data =pred_knn_over, reference =  y_test)

 ##need to fix

#### Logestic Ridge and Lasso #### 
#ridge
lambda=10^seq(10,-2,length=100)
ridge_model <- glmnet(x_trn_under[,-22], x_trn_under[,22], alpha = 0,family = "binomial")
plot(ridge_model, xvar = "lambda", label = T)
cv_fit_ridge <- cv.glmnet(x_trn_under[,-22], x_trn_under[,22], alpha = 0, family = "binomial")
plot(cv_fit_ridge)

pred_ridge<-predict(ridge_model,s=cv_fit_ridge$lambda.min,newx = x_test)
pred_ridge.class <- as.factor(ifelse(pred_ridge > 0.5, 1, 0))
confusionMatrix(data = pred_ridge.class, reference = y_test)

#lasso
lasso_model <- glmnet(x_trn_under[,-22], x_trn_under[,22], alpha = 1,family = "binomial")
plot(lasso_model, xvar = "lambda", label = T)
cv_fit_lasso <- cv.glmnet(x_trn_under[,-22], x_trn_under[,22], alpha = 0,family = "binomial")
plot(cv_fit_lasso)

pred_lasso <-predict(lasso_model,s=cv_fit_lasso$lambda.min,newx = x_test)
pred_lasso.class <- as.factor(ifelse(pred_lasso > 0.5, 1, 0))
confusionMatrix(data = pred_lasso.class, reference = y_test)

### best parameters


#### Linear Discriminant Analysis ####
lda_fit_under <- lda(Fraud~.,data = x_trn_under_df)
plot(lda_fit_under)
lda.pred_under <- predict(lda_fit_under,test[,-22])
confusionMatrix(data = lda.pred_under$class,y_test)

lda_fit_over <- lda(Fraud~.,data = x_trn_over_df)
plot(lda_fit_over)
lda.pred_over <- predict(lda_fit,test[,-22])
confusionMatrix(data = lda.pred_over$class,y_test)
#### Classification Trees ####

x_trn_over_df$Fraud <- as.factor(x_trn_over_df$Fraud)
x_trn_under_df$Fraud <- as.factor(x_trn_under_df$Fraud)
tree.fraud <- tree(Fraud~., x_trn_under_df,subset = train)
plot(tree.fraud);text(tree.fraud,pretty = 0)
tree.pred <- predict(tree.fraud, test, type = "class")
confusionMatrix(data = tree.pred, reference = as.factor(test$Fraud))

#cross validation
cv.fraud <- cv.tree(tree.fraud,FUN = prune.misclass)
plot(cv.fraud)
prune.fraud <- prune.misclass(tree.fraud,best=11)
plot(prune.fraud);text(tree.fraud,pretty = 0)
tree.pred <- predict(prune.fraud, test, type = "class")
confusionMatrix(data = tree.pred, reference = as.factor(test$Fraud))

### under
tree.fraud <- tree(Fraud~., x_trn_over_df,subset = train)
plot(tree.fraud);text(tree.fraud,pretty = 0)
tree.pred <- predict(tree.fraud, test, type = "class")
confusionMatrix(data = tree.pred, reference = as.factor(test$Fraud))

#cross validation
cv.fraud <- cv.tree(tree.fraud,FUN = prune.misclass)
plot(cv.fraud)
prune.fraud <- prune.misclass(tree.fraud,best=11)
plot(prune.fraud);text(tree.fraud,pretty = 0)
tree.pred <- predict(prune.fraud, test, type = "class")
confusionMatrix(data = tree.pred, reference = as.factor(test$Fraud))

#bagging tree model 
x_trn_under_df$Fraud <- as.factor(x_trn_under_df$Fraud)
bag.fraud_u  <- randomForest(Fraud~., data = x_trn_under_df, mtry = 21)

bag.pred_u <- predict(bag.fraud_u, test)
confusionMatrix(data = bag.pred_u,as.factor(test$Fraud))

x_trn_over_df$Fraud <- as.factor(x_trn_over_df$Fraud)
bag.fraud_o  <- randomForest(Fraud~., data = x_trn_over_df, mtry = 21)

bag.pred_o <- predict(bag.fraud_o, test)
confusionMatrix(data = bag.pred_o,as.factor(test$Fraud))


#random Forest model
x_trn_under_df$Fraud <- as.factor(x_trn_under_df$Fraud)
bag.fraud_u  <- randomForest(Fraud~., data = x_trn_under_df, mtry = 10)

bag.pred_u <- predict(bag.fraud_u, test)
confusionMatrix(data = bag.pred_u,as.factor(test$Fraud))

x_trn_over_df$Fraud <- as.factor(x_trn_over_df$Fraud)
bag.fraud_o  <- randomForest(Fraud~., data = x_trn_over_df, mtry = 10)

bag.pred_o <- predict(bag.fraud_o, test)
confusionMatrix(data = bag.pred_o,as.factor(test$Fraud))

#boosting model 
x_trn_under_df <- ovun.sample(Fraud~., data = training, method = "under", N = 283*2)$data
x_trn_over_df <- ovun.sample(Fraud~., data = training, method = "over", N = 2822*2)$data
x_trn_under_df$Fraud <- as.integer(x_trn_under_df$Fraud)

boost.fraud_u <- gbm(Fraud~., data = x_trn_under_df, n.trees = 2000,
                   shrinkage = 0.01,interaction.depth = 4)
summary(boost.fraud_u)
rel_inf <- data.frame(summary(boost.fraud_u))
ggplot(rel_inf, aes(x=reorder(var, c(21:1)), y=rel.inf)) +
  geom_bar(stat='identity', fill = "blue") + 
  coord_flip() + 
  labs(title= "Variable Importance for Boosting Tree Model", 
       y="Relative Influence", x = "Variables")

boost.pred_u <- predict(boost.fraud_u,test,n.trees = 2000, type = "response")
boost.pred_u.class <- as.factor(ifelse(boost.pred_u > 0.5, 1, 0))

confusionMatrix(data = boost.pred_u.class,reference = as.factor(test$Fraud))
x_trn_over_df$Fraud <- as.integer(x_trn_over_df$Fraud)
boost.fraud_o <- gbm(Fraud~., data = x_trn_over_df, n.trees = 2000,
                   shrinkage = 0.01,interaction.depth = 4)
summary(boost.fraud_o)

boost.pred_o <- predict(boost.fraud_o,test,n.trees = 2000, type = "response")
boost.pred_o.class <- as.factor(ifelse(boost.pred_o > 0.5, 1, 0))

confusionMatrix(data = boost.pred_o.class,reference = as.factor(test$Fraud))



#### SVM models ####
x_trn_over_df$Fraud <- as.factor(x_trn_over_df$Fraud)
x_trn_under_df$Fraud <- as.factor(x_trn_under_df$Fraud)
svm.fraud <- svm(Fraud~., data = x_trn_under_df, kernel = "linear", cost = 0.1, scale = FALSE)
tuned <- tune(svm, Fraud~., data = x_trn_under_df, kernal = "linear",
              ranges = list(epsilon = seq(0,1,0.1)), cost = 2^(2:9))
summary(tuned)
svm.fraud_u <- svm(Fraud~., data = x_trn_under_df, kernel = "linear", cost = 0.175, scale = FALSE)

svm.pred <- predict(svm.fraud_u, test, type="class")
confusionMatrix(data =svm.pred, reference = as.factor(test$Fraud))


#### data graphs ####
totalclm <- training_df%>%
  dplyr::select(Fraud,totalClaims)%>%
  group_by(Fraud)%>%
  summarise(meanclm = mean(totalClaims))

dedclaim <- training_df%>%
  dplyr::select(Fraud,DeductibleAmtPaid)%>%
  group_by(Fraud)%>%
  summarise(meandedclaim = mean(DeductibleAmtPaid))

insclaimrein <- training_df%>%
  dplyr::select(Fraud,InscClaimAmtReimbursed)%>%
  group_by(Fraud)%>%
  summarise(meaninsclaimrei = mean(InscClaimAmtReimbursed))

ggplot(totalclm) +
  aes(x = Fraud, weight = meanclm) +
  geom_bar(fill = "#0c4c8a") +
  labs(y = "Average number of claims", title = "Average number of Total Claims for Fraudulent ") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(dedclaim) +
  aes(x = Fraud, weight = meandedclaim) +
  geom_bar(fill = "#0c4c8a") +
  labs(y = "Deductible Amount", title = "Average Deductible Amount paid") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

ggplot(insclaimrein) +
  aes(x = Fraud, weight = meaninsclaimrei) +
  geom_bar(fill = "#0c4c8a") +
  labs(y = "Reimbursed Amount", title = "Insurance Claim Reimbursed ") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))




#### Prediction ####
med_out_in_ben_eval$RenalDiseaseIndicator <- as.character(med_out_in_ben_eval$RenalDiseaseIndicator)
med_out_in_ben_eval$RenalDiseaseIndicator <- conv(med_out_in_ben_eval$RenalDiseaseIndicator,"Y",1)
med_out_in_ben_eval$Gender <- conv(med_out_in_ben_eval$Gender, 2, 0)
med_out_in_ben_eval$DeductibleAmtPaid[is.na(med_out_in_ben_eval$DeductibleAmtPaid)] <-0
j <-40
while(j<51){
  med_out_in_ben_eval[,j] <- conv(med_out_in_ben_eval[,j],2,0)
  j <- j+1
}
med_out_in_ben_eval$ClaimEndDt <- as.Date(med_out_in_ben_eval$ClaimEndDt)
med_out_in_ben_eval$ClaimStartDt <- as.Date(med_out_in_ben_eval$ClaimStartDt)
#### data manipulation ####
test_eval <- med_out_in_ben_eval%>%
  mutate(ClmDur = as.numeric(ClaimEndDt-ClaimStartDt))%>%
  dplyr::select(ProviderID,InscClaimAmtReimbursed,DeductibleAmtPaid,IPAnnualReimbursementAmt,IPAnnualDeductibleAmt,
         OPAnnualDeductibleAmt,OPAnnualReimbursementAmt,Gender, RenalDiseaseIndicator,ClmDur,
         ChronicCond_Alzheimer,ChronicCond_Cancer, ChronicCond_Heartfailure,ChronicCond_KidneyDisease,
         ChronicCond_Depression,ChronicCond_stroke,ChronicCond_IschemicHeart,ChronicCond_Diabetes,
         ChronicCond_ObstrPulmonary,ChronicCond_Osteoporasis,ChronicCond_rheumatoidarthritis)%>%
  group_by(ProviderID)%>%
  summarise(InscClaimAmtReimbursed=mean(InscClaimAmtReimbursed),
            DeductibleAmtPaid=mean(DeductibleAmtPaid),
            IPAnnualReimbursementAmt=mean(IPAnnualReimbursementAmt), 
            IPAnnualDeductibleAmt=mean(IPAnnualDeductibleAmt),
            OPAnnualDeductibleAmt=mean(OPAnnualDeductibleAmt),
            OPAnnualReimbursementAmt=mean(OPAnnualReimbursementAmt),
            Gender = mean(Gender), RenalDiseaseIndicator = mean(RenalDiseaseIndicator),
            ChronicCond_Alzheimer = mean(ChronicCond_Alzheimer),
            ChronicCond_Cancer = mean(ChronicCond_Cancer),ChronicCond_Heartfailure = mean(ChronicCond_Heartfailure),
            ChronicCond_KidneyDisease = mean(ChronicCond_KidneyDisease), ChronicCond_Depression = mean(ChronicCond_Depression),
            ChronicCond_stroke = mean(ChronicCond_stroke), ChronicCond_IschemicHeart = mean(ChronicCond_IschemicHeart),
            ChronicCond_Diabetes = mean(ChronicCond_Diabetes), ChronicCond_ObstrPulmonary = mean(ChronicCond_ObstrPulmonary),
            ChronicCond_Osteoporasis = mean(ChronicCond_Osteoporasis), 
            ChronicCond_rheumatoidarthritis = mean(ChronicCond_rheumatoidarthritis),
            ClmDur=mean(ClmDur), totalClaims = n())

prediction <- data.frame(fraud = predict(boost.fraud_u,test_eval,n.trees = 2000, type = "response"))
pred <- cbind(test_eval,prediction)%>%
  dplyr::select(ProviderID,fraud)%>%
  dplyr::arrange(desc(fraud))

final_prediction  <- pred[1:350,]
  
final_350 <-final_prediction[,-2]

#write.csv(final_350, row.names = FALSE, file = "final350.csv")
