source("./Data_Wrangling_&_EDA/Lendingclubreaddata.R")
pacman::p_load(lme4,rstanarm,arm,ggcorrplot,ROCR,ggmosaic)
#library(ggmosaic)

#ggplot(lending_selected_na_drop)+aes(x = addr_state)+geom_bar()+theme(axis.text.x = element_text(angle = 45))
#ggplot(lending_selected_na_drop)+aes(x = cr_length)+geom_histogram(bins = 30)
#ggplot(lending_selected_na_drop)+geom_mosaic(aes(x = product(factor(y),addr_state),fill = factor(y)))+theme(axis.text.x = element_text(angle = 45))
#str(lending_selected_na_drop)

#ggplot(lending_selected_na_drop)+aes(x = inq_last_6mths, y = y)+geom_point()

lendingclub <- lending_selected_na_drop%>%dplyr::select(-funded_amnt,-issue_d,-earliest_cr_line,-fico_range_low,-pub_rec,-last_fico_range_high,-last_fico_range_low)

lendingclub%<>%dplyr::mutate(loan_amnt_log = log(loan_amnt),cr_length_log = log(as.numeric(cr_length)),fico_10 = (fico_range_high-median(fico_range_high))/10,installment_log = log(installment),dti_scale = scale(dti,center = TRUE,scale = TRUE),int_rate_scale = int_rate - min(int_rate),installment_log = installment_log-mean(installment_log))
lendingclub_ori <- lendingclub
lendingclub%<>%dplyr::select(-loan_amnt,-cr_length,-loan_status,-fico_range_high,-installment,-dti,-home_ownership,-int_rate)
#ggplot(lendingclub)+aes(x = cr_length_log)+geom_histogram(bins = 30)

#ggplot(lendingclub)+geom_mosaic(aes(x = product(factor(y),grade),fill = factor(y)))+theme(axis.text.x = element_text(angle = 45))

lendingclub_model <- lendingclub%>%dplyr::select(-int_rate_scale,-delinq_amnt,-chargeoff_within_12_mths)
lendingclub_model_2 <- lendingclub%>%dplyr::select(id,int_rate_scale)
#state_eda <- glm(data = lendingclub_model,formula = y~addr_state-1,family = binomial)
#sum_state <- summary(state_eda)
#summary(lendingclub_model$addr_state)
#substrRight <- function(x){
#  substr(x, nchar(x)-2+1, nchar(x))
#}
#state_name <- lapply(rownames(sum_state$coefficients),substrRight)%>%unlist()%>%factor()
#coef(state_eda)
#se_state <- sqrt(diag(vcov(state_eda)))
#ggplot()+aes(x = state_name,y = coef(state_eda))+geom_point()+geom_errorbar(aes(ymin = coef(state_eda)-2*se_state,ymax = coef(state_eda)+2*se_state),width=.2,position=position_dodge(.9))+ylim(c(-4.5,2.5))+theme(axis.text.x = element_text(angle = 35,size = 6))

#state_eda_glmer <- lme4::glmer(data = lendingclub_model,formula = y~(1|addr_state),family = binomial)
#arm::display(state_eda_glmer)
#state_glmer_sd <- arm::display(state_eda_glmer)


#state_glmer_sd <- lme4::VarCorr(state_eda_glmer)
#state_glmer_sd%<>%as.data.frame()%>%dplyr::select(sdcor)%>%pull()

#lm_state_ranef <- lme4::ranef(state_eda_glmer)$addr_state
#pacman::p_load(tidyverse)

#lm_state_ranef%<>%rownames_to_column(var = "state")

#lm_state_fixef <- lme4::fixef(state_eda_glmer)
#lm_state_ranef$`(Intercept)` <- lm_state_ranef$`(Intercept)`+lm_state_fixef
#ggplot(lm_state_ranef)+aes(x = state,y = `(Intercept)`)+geom_point()+geom_errorbar(aes(ymin = `(Intercept)`-2*state_glmer_sd,ymax = `(Intercept)`+2*state_glmer_sd),width=.2,position=position_dodge(.9))+theme(axis.text.x = element_text(angle = 35,size = 6))
## MFA on lendingclub_model
#pacman::p_load("FactoMineR", "factoextra",psych,GPArotation,irr)

#to_numeric <- function(df){
#  require(magrittr,dplyr)
#  return(df%<>%dplyr::mutate_if(.predicate = is.factor,.funs = as.numeric))
#}

#lapply(lendingclub_model_MFA,levels)
levels(lendingclub_model$emp_length) <- c("n/a","< 1 year","1 year","2 years","3 years","4 years","5 years","6 years","7 years","8 years","9 years","10 years","10+ years")
#levels(lendingclub_model$home_ownership) <- c("NONE","RENT","MORTGAGE","OWN","OTHER")



lendingclub_model_id_0 <- lendingclub_model%>%dplyr::filter(y == 0)%>%dplyr::select(id)%>%dplyr::sample_frac(size = .3)%>%pull()
lendingclub_model_id_1 <- lendingclub_model%>%dplyr::filter(y == 1)%>%dplyr::select(id)%>%dplyr::sample_frac(size = .3)%>%pull()

lendingclub_model_id_val <- c(lendingclub_model_id_0,lendingclub_model_id_1)
lendingclub_model_train <- lendingclub_model%>%filter(!id %in% lendingclub_model_id_val)
lendingclub_model_val <- lendingclub_model%>%filter(id %in% lendingclub_model_id_val)

## Check if the split is valid
(nrow(lendingclub_model_train)+nrow(lendingclub_model_val)) == nrow(lendingclub_model)
rm(lending_selected,lending_selected_na_drop,list_var,lendingclub_model_id_0,lendingclub_model_id_1,lendingclub_model_id_val)


