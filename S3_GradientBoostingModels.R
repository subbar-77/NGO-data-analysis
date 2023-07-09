# Load packages, functions and data from previous two sessions
# Remember to have all files and scripts in the same folder
#source("S1_ForecastingSales.R")
#source("S2_IntroductionPropensityModels.R")

source("./MIBA 20/SESSIONS/S1/Code and data/SESSION1_Forecasting_Sales.R")
source("./MIBA 20/SESSIONS/S2/Code and data/S2_IntroductionPropensityModels.R")

library(gbm)

# ============================================================================================= #
# SALES FORECASTING MODELS
# ============================================================================================= #
# Data set
sales_data = sales[complete.cases(sales_size,lag_1_sales_size,lag_2_sales_size,lag_4_sales_size,
                                  lag_52_sales_size,temp,unemployment,md,fuel_price,is_holiday,super_bowl),]

sales_data = sales

# 5 fold : Train-test split
cfolds = 5
index = sample(1:cfolds, nrow(sales_data), replace = TRUE)
# To simplify, create a train with index 1:4, test with index 5
train_sales = sales_data[index%in%1:4,]
test_sales = sales_data[index ==5,]

sales_glm = glm(sales_size~0+lag_1_sales_size+lag_2_sales_size+lag_4_sales_size+
           lag_52_sales_size+unemployment+md+fuel_price+is_holiday+super_bowl,
         data = train_sales)
summary(sales_glm)

sales_gbm = gbm(sales_size~lag_1_sales_size+lag_2_sales_size+lag_4_sales_size+
                  lag_52_sales_size+unemployment+temp+md+fuel_price+is_holiday+super_bowl,
                data = train_sales,
                n.trees=1000, 
                shrinkage = 0.02, 
                n.minobsinnode =50)

summary(sales_gbm)


error_glm = mean(abs(predict(sales_glm,newdata = test_sales)-test_sales$sales_size))/mean(test_sales$sales_size)
error_glm
error_gbm = mean(abs(predict(sales_gbm,newdata = test_sales, n.trees = 1000)-test_sales$sales_size))/mean(test_sales$sales_size)
error_gbm



# What happens if we decrease trees to 50?
sales_gbm_3 = gbm(sales_size~lag_1_sales_size+lag_2_sales_size+lag_4_sales_size+
                    lag_52_sales_size+unemployment+temp+md+fuel_price+is_holiday+super_bowl,
                  data = train_sales,n.trees=50, shrinkage = 0.02, n.minobsinnode =50)
error_gbm = mean(abs(predict(sales_gbm_3,newdata = test_sales, n.trees = 50)-test_sales$sales_size))/mean(test_sales$sales_size)
error_gbm
error_gbm = mean(abs(predict(sales_gbm_3,newdata = train_sales, n.trees = 50)-train_sales$sales_size))/mean(train_sales$sales_size)
error_gbm



# ============================================================================================= #
# CREDIT SCORE MODEL
# ============================================================================================= #
# Data set
credit_data = lending_data[,.(default,grade_num,is_emp,home_own_ord,
                             annual_inc,delinq_2yrs_ind, risk_purpose,home_ownership=as.factor(home_ownership),purpose=as.factor(purpose))]


cfolds = 5
index = sample(1:cfolds, nrow(credit_data), replace = TRUE)
# To simplify, create a train with index 1:4, test with index 5
train_credit = credit_data[index%in%1:4,]
test_credit = credit_data[index ==5,]


credit_glm = glm(default~grade_num+is_emp+annual_inc+risk_purpose+home_own_ord, data = train_credit, family = "binomial")

credit_gbm = gbm(default~grade_num+is_emp+annual_inc+risk_purpose+home_own_ord+annual_inc+delinq_2yrs_ind,
                data = train_credit,
                n.trees=500, 
                shrinkage = 0.03,
                n.minobsinnode =300, 
                distribution = "adaboost")

credit_gbm = gbm(default~grade_num+is_emp+annual_inc+purpose+home_ownership+annual_inc+delinq_2yrs_ind,
                 data = train_credit,
                 n.trees=1500,
                 shrinkage = 0.01, 
                 n.minobsinnode =100, 
                 distribution = "bernoulli")

# credit_gbm = gbm(default~grade_num+is_emp+annual_inc+risk_purpose+home_own_ord+annual_inc+delinq_2yrs_ind,
#                  data = train_credit,n.trees=500, shrinkage = 0.05, n.minobsinnode =200, distribution = 'bernoulli')

summary(credit_gbm)

auc_glm = auc(test_credit$default, predict(credit_glm, newdata=test_credit, type= "response"))
auc_glm
auc_gbm = auc(test_credit$default, predict(credit_gbm, newdata=test_credit, n.trees = 500,type= "response"))
auc_gbm = auc(test_credit$default, predict(credit_gbm, newdata=test_credit, n.trees = 1500,type= "response"))
auc_gbm

auc_glm = auc(train_credit$default, predict(credit_glm, newdata=train_credit, type= "response"))
auc_glm
auc_gbm = auc(train_credit$default, predict(credit_gbm, newdata=train_credit, n.trees = 500,type= "response"))
auc_gbm

# ============================================================================================= #
# ENSEMBLE OF MODELS
# ============================================================================================= #
# Predictions are highly correlated but not 100%
# Sales
cor(predict(sales_glm, newdata=train_sales, type= "response"),predict(sales_gbm, newdata=train_sales, n.trees = 1000,type= "response"))
cor(predict(sales_glm, newdata=test_sales, type= "response"),predict(sales_gbm, newdata=test_sales, n.trees = 1000,type= "response"))

# Credit
cor(predict(credit_glm, newdata=train_credit, type= "response"),predict(credit_gbm, newdata=train_credit, n.trees = 500,type= "response"))
cor(predict(credit_glm, newdata=test_credit, type= "response"),predict(credit_gbm, newdata=test_credit, n.trees = 500,type= "response"))


# Averaging predictions can improve goodness of fit
# Sales
ensemble_sales = (predict(sales_glm, newdata=test_sales, type= "response") + predict(sales_gbm, newdata=test_sales, n.trees = 1000,type= "response"))/2
error_ensemble_sales = mean(abs(ensemble_sales-test_sales$sales_size))/mean(test_sales$sales_size)
error_ensemble_sales

# Credit
ensemble_credit = (predict(credit_glm, newdata=test_credit, type= "response") + predict(credit_gbm, newdata=test_credit, n.trees = 500,type= "response"))/4
auc_ensemble_credit = auc(test_credit$default, ensemble_credit)
auc_ensemble_credit
