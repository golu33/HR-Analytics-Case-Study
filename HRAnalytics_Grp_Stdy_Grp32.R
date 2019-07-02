library(plyr)
library(dplyr)
library(tidyr)
library(car)
library(MASS)
library(Hmisc)
library(stringr)
library(ggplot2)
library(lubridate)
library(caret)
library(ROCR)

setwd("C:/PGDDS/HR analytics Case Study")
# Import Data

emp_surv <- read.csv("employee_survey_data.csv")
gen_data <- read.csv("general_data.csv")
mgm_surv <- read.csv("manager_survey_data.csv")
in_time<- read.csv("in_time.csv")
out_time<-read.csv("out_time.csv")

#Understanding the structure and summary
str(emp_surv)
summary(emp_surv)
str(gen_data)
summary(gen_data)
str(mgm_surv)
summary(mgm_surv)
str(in_time)
str(out_time)

#checking for NA's in different datasets
sum(is.na(emp_surv)) # 83 NA values found
sum(is.na(gen_data)) # 28 NA values found
sum(is.na(mgm_surv))  # 0 NA values found
sum(is.na(in_time)) # 109080 values found
sum(is.na(out_time))  # 109080 values found

# Changing column names
colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"

#Checking for duplicate values
which(duplicated(emp_surv$EmployeeID))# no duplicate values
which(duplicated(gen_data$EmployeeID))# no duplicate values
which(duplicated(mgm_surv$EmployeeID))# no duplicate values
which(duplicated(in_time$EmployeeID))# no duplicate values
which(duplicated(out_time$EmployeeID))# no duplicate values

#Deleting columns with all NA in time DF
in_time <- in_time[,colSums(is.na(in_time))<nrow(in_time)]
out_time<-out_time[,colSums(is.na(out_time))<nrow(out_time)]

#Checking NA values
sum(is.na(in_time)) 
sum(is.na(out_time))

#Changing variables for manipulation
in_time1=in_time
out_time1=out_time

in_time1$EmployeeID<-NULL
out_time1$EmployeeID<-NULL

in_time1 <- sapply(in_time1, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
in_time1<-as.data.frame(in_time1)

out_time1 <- sapply(out_time1, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
out_time1<-as.data.frame(out_time1)

# Calculating working hours
diff_time<-out_time1-in_time1

#converting all values to numeric
diff_time<-sapply(diff_time,function(x) as.numeric(x))
diff_time<-as.data.frame(diff_time)

#Calculating mean working hours for each employee
diff_time$Avg_Work_Hrs<-apply(diff_time,1,mean,na.rm=TRUE)

#combining employee ID and Avg Working Hours
Emp_WorkHrs<-cbind(in_time$EmployeeID,diff_time$Avg_Work_Hrs)
Emp_WorkHrs<-as.data.frame(Emp_WorkHrs)
sum(is.na(Emp_WorkHrs)) # No NA values

#Changing the Column name
colnames(Emp_WorkHrs) [1] <- "EmployeeID"

#NA values in other  need to be attended

# NumCompaniesWorked       TotalWorkingYears EnvironmentSatisfaction          
#      19                       9                      25            
# JobSatisfaction          WorkLifeBalance 
#      20                       38

# Imputing Values for NA as the median of the column
gen_data$NumCompaniesWorked<-impute(gen_data$NumCompaniesWorked, median)
gen_data$TotalWorkingYears<-impute(gen_data$TotalWorkingYears, median)

emp_surv$EnvironmentSatisfaction<-impute(emp_surv$EnvironmentSatisfaction, median)
emp_surv$JobSatisfaction<-impute(emp_surv$JobSatisfaction, median)
emp_surv$WorkLifeBalance<-impute(emp_surv$WorkLifeBalance, median)

sum(is.na(emp_surv)) # 0 NA values found
sum(is.na(gen_data)) # 0 NA values found

#Merging data to create Integrated Dataset
intg_emp_data <- merge(x = gen_data, y = emp_surv, by = "EmployeeID", all.x = TRUE)
intg_emp_data <- merge(x= intg_emp_data, y = mgm_surv, by = "EmployeeID", all.x = TRUE)
intg_emp_data <- merge(x= intg_emp_data, y = Emp_WorkHrs, by = "EmployeeID", all.x = TRUE)

#Changing name of column
colnames(intg_emp_data) [30]  <- "AvgWrkHrs"
sum(is.na(intg_emp_data))# No NA values in Integrated dataset

#removing variables which have the same value for all the rows
intg_emp_data$EmployeeCount<-NULL
intg_emp_data$Over18<-NULL
intg_emp_data$StandardHours<-NULL

#Doing Some Outlier treatment on The Monthly income as higher income ppl are managemnt and have different attrition criteria
boxplot(intg_emp_data$MonthlyIncome)
firstquad1 <- quantile(intg_emp_data$MonthlyIncome,0.25)
thirdquad1<-quantile(intg_emp_data$MonthlyIncome,0.75)
upperlmtforout1<-quantile(intg_emp_data$MonthlyIncome,0.75)+1.5*(thirdquad1-firstquad1)
lowerlmtforout1<-quantile(intg_emp_data$MonthlyIncome,0.25)-1.5*(thirdquad1-firstquad1) # since it is negative npt conisdered
intg_emp_data<-subset(intg_emp_data,intg_emp_data$MonthlyIncome<=upperlmtforout1) # Ignoring values above 99 percent quantile


# continuos variables scaling
intg_emp_data$Age<- scale(intg_emp_data$Age) 
intg_emp_data$DistanceFromHome<- scale(intg_emp_data$DistanceFromHome) 
intg_emp_data$JobLevel<- scale(intg_emp_data$JobLevel) 
intg_emp_data$MonthlyIncome<- scale(intg_emp_data$MonthlyIncome) 
intg_emp_data$NumCompaniesWorked<- scale(intg_emp_data$NumCompaniesWorked) 
intg_emp_data$PercentSalaryHike<- scale(intg_emp_data$PercentSalaryHike) 
intg_emp_data$StockOptionLevel<- scale(intg_emp_data$StockOptionLevel) 
intg_emp_data$TotalWorkingYears<- scale(intg_emp_data$TotalWorkingYears) 
intg_emp_data$TrainingTimesLastYear<- scale(intg_emp_data$TrainingTimesLastYear) 
intg_emp_data$YearsAtCompany<- scale(intg_emp_data$YearsAtCompany) 
intg_emp_data$YearsSinceLastPromotion<- scale(intg_emp_data$YearsSinceLastPromotion) 
intg_emp_data$YearsWithCurrManager<- scale(intg_emp_data$YearsWithCurrManager) 
intg_emp_data$AvgWrkHrs<- scale(intg_emp_data$AvgWrkHrs)

#Categorical Variables treatment

#for Gender Female (0), Male(1)
table(factor(intg_emp_data$Gender))
levels(intg_emp_data$Gender) <- c(0,1) 
intg_emp_data$Gender <- as.numeric(levels(intg_emp_data$Gender))[intg_emp_data$Gender]

#for Attrition Yes(1) No (0)

intg_emp_data$Attrition <- ifelse(intg_emp_data$Attrition =="Yes",1,0)

# Creating dummy variables for Multilevel Categorical Variaables

d_bus_travel<-data.frame(model.matrix( ~BusinessTravel, data =intg_emp_data))

d_dept<-data.frame(model.matrix( ~Department, data =intg_emp_data))

d_job_role<-data.frame(model.matrix( ~JobRole, data =intg_emp_data))

d_mar_sts<-data.frame(model.matrix( ~MaritalStatus, data =intg_emp_data))

d_edu_field<-data.frame(model.matrix( ~EducationField, data =intg_emp_data))

#Changing dataset before insertion
intg_emp_data_new<-intg_emp_data

# Inserting dummy var
intg_emp_data_new<-cbind(intg_emp_data_new %>% dplyr::select(-BusinessTravel),d_bus_travel[,-1])

intg_emp_data_new<-cbind(intg_emp_data_new %>% dplyr::select(-Department),d_dept[,-1])

intg_emp_data_new<-cbind(intg_emp_data_new %>% dplyr::select(-JobRole),d_job_role[,-1])

intg_emp_data_new<-cbind(intg_emp_data_new %>% dplyr::select(-MaritalStatus),d_mar_sts[,-1])

intg_emp_data_new<-cbind(intg_emp_data_new %>% dplyr::select(-EducationField),d_edu_field[,-1])


# Divide into training and test data set
#set the seed to 100, let's run it 

set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(intg_emp_data_new), 0.7*nrow(intg_emp_data_new))
# generate the train data set
train = intg_emp_data_new[trainindices,]

test = intg_emp_data_new[-trainindices,]

# Logistic Regression: 

mdl_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(mdl_1)

mdl_2<- stepAIC(mdl_1, direction="both")
summary(mdl_2)
vif(mdl_2)

# removing variables one by one based on VIF and p-value.

mdl_3<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
              JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusMarried + 
              MaritalStatusSingle + EducationFieldLife.Sciences, family = "binomial", 
              data = train)
summary(mdl_3)
vif(mdl_3)

mdl_3<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
              JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusMarried + 
              MaritalStatusSingle + EducationFieldLife.Sciences, family = "binomial", 
            data = train)
summary(mdl_3)
vif(mdl_3)


mdl_4<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
              JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusMarried + 
              MaritalStatusSingle + EducationFieldLife.Sciences, family = "binomial", 
            data = train)
summary(mdl_4)
vif(mdl_4)


mdl_5<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
              JobRoleResearch.Scientist + JobRoleSales.Executive +  
              MaritalStatusSingle + EducationFieldLife.Sciences, family = "binomial", 
            data = train)
summary(mdl_5)
vif(mdl_5)


mdl_6<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
              JobRoleSales.Executive +  
              MaritalStatusSingle + EducationFieldLife.Sciences, family = "binomial", 
            data = train)
summary(mdl_6)
vif(mdl_6)


mdl_7<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
              MaritalStatusSingle + EducationFieldLife.Sciences, family = "binomial", 
            data = train)
summary(mdl_7)
vif(mdl_7)


mdl_8<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + JobRoleResearch.Director + 
              MaritalStatusSingle, family = "binomial", 
            data = train)
summary(mdl_8)
vif(mdl_8)


mdl_9<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + TrainingTimesLastYear + 
              YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + 
              MaritalStatusSingle, family = "binomial", 
            data = train)
summary(mdl_9)
vif(mdl_9)

mdl_10<- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
              TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
              JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
              DepartmentResearch...Development + 
              DepartmentSales + JobRoleManufacturing.Director + 
              MaritalStatusSingle, family = "binomial", 
            data = train)
summary(mdl_10)
vif(mdl_10)

mdl_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
               DepartmentResearch...Development + 
               DepartmentSales + JobRoleManufacturing.Director + 
               MaritalStatusSingle, family = "binomial", 
             data = train)
summary(mdl_11)
vif(mdl_11)

mdl_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
               DepartmentSales + JobRoleManufacturing.Director + 
               MaritalStatusSingle, family = "binomial", 
             data = train)
summary(mdl_12)
vif(mdl_12)

mdl_13<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
               JobRoleManufacturing.Director + 
               MaritalStatusSingle, family = "binomial", 
             data = train)
summary(mdl_13)
vif(mdl_13)

mdl_14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + AvgWrkHrs + BusinessTravelTravel_Frequently + 
               MaritalStatusSingle, family = "binomial", 
             data = train)
summary(mdl_14)
vif(mdl_14)

# Model for Evaluation

eval_mdl<-mdl_14

#predicted probabilities of Attrition for test data

test_pred = predict(eval_mdl, type = "response", 
                    newdata = test[,-1])

# Checking summary

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_act_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

mdl_conf<-confusionMatrix(test_pred_attr,test_act_attr, positive = "Yes")

mdl_acc <- mdl_conf$overall[1]
mdl_sens <- mdl_conf$byClass[1]
mdl_spec <- mdl_conf$byClass[2]

#####################Try with 40 %##################################################
test_pred_attr <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

mdl_conf<-confusionMatrix(test_pred_attr,test_act_attr, positive = "Yes")

mdl_acc <- mdl_conf$overall[1]
mdl_sens <- mdl_conf$byClass[1]
mdl_spec <- mdl_conf$byClass[2]

#######################Try with 30 %################################################

test_pred_attr <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))

mdl_conf<-confusionMatrix(test_pred_attr,test_act_attr, positive = "Yes")

mdl_acc <- mdl_conf$overall[1]
mdl_sens <- mdl_conf$byClass[1]
mdl_spec <- mdl_conf$byClass[2]

#############################Try with 25 %##########################################

test_pred_attr <- factor(ifelse(test_pred >= 0.25, "Yes", "No"))

mdl_conf<-confusionMatrix(test_pred_attr,test_act_attr, positive = "Yes")

mdl_acc <- mdl_conf$overall[1]
mdl_sens <- mdl_conf$byClass[1]
mdl_spec <- mdl_conf$byClass[2]

#########################################################################################
# Need  to Find optimal probability cuttoff 

calcute_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_act_attr, positive = "Yes")
  mdl_acc_t <- conf$overall[1]
  mdl_sens_t <- conf$byClass[1]
  mdl_spec_t <- conf$byClass[2]
  out <- t(as.matrix(c(mdl_sens_t, mdl_spec_t, mdl_acc_t))) 
  colnames(out) <- c("Sensitivity", "Specificity", "Accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

# Preapration of plotting of Curve

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(x in 1:100)
{
  OUT[x,] = calcute_fn(s[x])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1,cex.axis=1,ylim=c(0,1),type="l",lwd=2.5,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1)
lines(s,OUT[,2],col="blue",lwd=2.5)
lines(s,OUT[,3],col="green",lwd=2.5)
box()
legend("bottomright",col=c(2,"blue","green","red"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_val <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#######################################################################

test_pred_attr <- factor(ifelse(test_pred >= cutoff_val, "Yes", "No"))

mdl_conf<-confusionMatrix(test_pred_attr,test_act_attr, positive = "Yes")

mdl_acc <- mdl_conf$overall[1]
mdl_sens <- mdl_conf$byClass[1]
mdl_spec <- mdl_conf$byClass[2]

##***************************************************************************##
### KS -statistic - Test Data ######

# KS statistic is an indicator of how well the model discriminates between 
# the two classes.
# It is equal to 0% for the random model, and 100% for the perfect model

pred_attrition_ks <- ifelse(test_pred_attr == "Yes",1,0)
actual_attrition_ks <- ifelse(test_act_attr == "Yes",1,0)

object_test_ks <- prediction(pred_attrition_ks, actual_attrition_ks)
performance_measures_ks <- performance(object_test_ks, "tpr", "fpr")

# Plotting the Area under curve
auc <- performance(object_test_ks,"auc")
unlist(auc@y.values)
# AUC =0.714

plot(performance_measures_ks,col = "red")
abline(0,1, lty = 8, col = "grey")

test_ks_table <- attr(performance_measures_ks, "y.values")[[1]] - 
  (attr(performance_measures_ks, "x.values")[[1]])
max(test_ks_table)

## Summary 
# KS Static - 42.9%  (KS Static > 40 % indicates a good model)

##***************************************************************************##
# Lift & Gain Chart 

# Gain chart is a popular method to visually inspect model performance 
# in binary prediction. It presents the percentage of captured 
# positive responses as a function of selected percentage of a sample

# Lift basically just tells you the factor by which your model is 
# outperforming a random model, i.e. a model-less situation

lift <- function(labels , predicted_prob,groups=10) {
  if (is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if (is.factor(predicted_prob)) predicted_prob <- 
      as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Decile_attr = lift(actual_attrition_ks, pred_attrition_ks, groups = 10)

ggplot(Decile_attr,aes(x = bucket, y = Gain)) + 
  geom_line() + 
  geom_point(col = 'blue', size = 4) +
  geom_text(aes(label = round(Gain,1.5)),  
            nudge_x = -0.50, nudge_y = -0.50)

ggplot(Decile_attr,aes(x = bucket, y = Cumlift)) + 
  geom_line() + 
  geom_point(col = 'darkgreen', size = 4) +
  geom_text(aes(label = round(Cumlift,1.5)), 
            nudge_x = 0.10, nudge_y = 0.10)

# We can catch 73.86 % ppl within the fourth decile with this model
