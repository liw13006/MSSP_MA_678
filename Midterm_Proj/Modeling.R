# read from previous part
source("./Data_Wrangling_&_EDA/EDA.R")

## --------------------------------------------------------------
## right now, there are two tables:
## 1. training set: 70% non-Chargedoff records + 70% Chargedoff records
## 2. Validation set: 30% non-Chargedoff records + 30% Chargedoff records

## load model libs!
pacman::p_loaded()

pacman::p_load(lme4,rstanarm,arm,ggcorrplot,ROCR,ggmosaic)
pacman::p_loaded()


## simple logistic regression on all predictors

set.seed(2019)

fit_simple <- glm(data = lendingclub_model_train%>%dplyr::select(-id), formula = y~.,family = binomial)


for_aic <- step(glm(data = lendingclub_model_train%>%dplyr::select(-id), formula = y~1,family = binomial),direction = "forward",scope = formula(fit_simple),k = 2,trace = 0)
binnedplot(for_aic$fitted.values,for_aic$residuals)


## use glmer
## 
#summary(for_aic)
## what if add purpose as random effect
#formula(for_aic)
ml_fit_aic <- glmer(data = lendingclub_model_train,formula = y ~ fico_10  + inq_last_6mths +open_acc + dti_scale+installment_log + delinq_2yrs + (1|addr_state)+(1|sub_grade)+ (1|term)+(1|purpose)+ (1|emp_length),family = binomial)
#display(ml_fit_aic)
#ranef(ml_fit_aic)
binnedplot(x = fitted(ml_fit_aic),y = resid(ml_fit_aic,type="response"))
y_pred_ml_aic <- predict(ml_fit_aic,newdata = lendingclub_model_val,type = "response")

#sum(y_pred_ml_aic>=0.5&lendingclub_model_val$y>0.5)
#sum(y_pred_ml_aic>=0.5)
set.seed(2019)
y_pred_sim <- rbinom(n=length(y_pred_ml_aic),size = 1,prob = y_pred_ml_aic)

sum(y_pred_sim)
sum(lendingclub_model_val$y)

#y_pred_ml_aic <- predict(ml_fit_aic,newdata = lendingclub_model_val,type = "response")
y_pred_for_aic <- predict(for_aic,newdata = lendingclub_model_val,type = "response")
y_pred_sim_nopool <- rbinom(n=length(y_pred_for_aic),size = 1,prob = y_pred_for_aic)
sum(y_pred_sim_nopool)
#y_pred_sim <- rbinom(n=length(y_pred_ml_aic),size = 1,prob = y_pred_ml_aic)
#sum(y_pred_sim>=0.5)-sum(lendingclub_model_val$y)
#sum(y_pred_sim_nopool>=0.5)-sum(lendingclub_model_val$y)
#sum(lendingclub_model_val$y)
#sum(y_pred_sim == lendingclub_model_train$y)

#binnedplot(for_aic$fitted.values,for_aic$residuals)
#binnedplot(x = fitted(ml_fit_aic),y = resid(ml_fit_aic,type="response"))


## create fake dataset
lendingclub_model_val%<>%dplyr::mutate(sim = y_pred_sim)


## Check distribution!
#ggplot(lendingclub_model_val)+aes(x = addr_state)+geom_bar()+theme(axis.text.x = element_text(angle = 45))
#ggplot(lending_selected_na_drop)+aes(x = cr_length)+geom_histogram(bins = 30)
#p1 <- ggplot(lendingclub_model_val)+geom_mosaic(aes(x = product(factor(y),sub_grade),fill = factor(y)))+theme(axis.text.x = element_text(angle = 45))
#p2 <- ggplot(lendingclub_model_val)+geom_mosaic(aes(x = product(factor(sim),sub_grade),fill = factor(sim)))+theme(axis.text.x = element_text(angle = 45))


#gridExtra::grid.arrange(p1,p2,nrow = 2)

lendingclub_model_2%<>%dplyr::mutate(int_rate_scale = 100*int_rate_scale)
lendingclub_model_train_2 <- dplyr::left_join(lendingclub_model_train,lendingclub_model_2)
lendingclub_model_val_2 <- dplyr::left_join(lendingclub_model_val,lendingclub_model_2)

ml_fit_aic_int <- glmer(data = lendingclub_model_train_2,formula = y ~ int_rate_scale+fico_10  + inq_last_6mths +open_acc + dti_scale+installment_log + delinq_2yrs + (1|addr_state)+(1+int_rate_scale|sub_grade)+ (1|term)+(1|purpose)+ (1|emp_length),family = binomial)


binnedplot(x = fitted(ml_fit_aic_int),y = resid(ml_fit_aic_int,type="response"))
#
y_pred_ml_aic_int <- predict(ml_fit_aic_int,newdata = lendingclub_model_val_2,type = "response")
set.seed(2019)
y_pred_int_sim <- rbinom(n=length(y_pred_ml_aic_int),size = 1,prob = y_pred_ml_aic_int)
sum(y_pred_int_sim)
lendingclub_model_val_2%<>%dplyr::mutate(sim = y_pred_int_sim)
#p1 <- ggplot(lendingclub_model_val_2)+geom_mosaic(aes(x = product(factor(y),sub_grade),fill = factor(y)))+theme(axis.text.x = element_text(angle = 45))
#p2 <- ggplot(lendingclub_model_val_2)+geom_mosaic(aes(x = product(factor(sim),sub_grade),fill = factor(sim)))+theme(axis.text.x = element_text(angle = 45))
#gridExtra::grid.arrange(p1,p2,nrow = 2)

save(for_aic,lendingclub,lendingclub_model,lendingclub_model_2,lendingclub_model_train,lendingclub_model_train_2,lendingclub_model_val,lendingclub_model_val_2,lendingclub_ori,ml_fit_aic,ml_fit_aic_int,y_pred_sim,y_pred_int_sim,y_pred_sim_nopool,y_pred_ml_aic,y_pred_ml_aic_int,file = "./results/results.Rdata")
