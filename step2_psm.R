# load library 
library(tidyverse)
library(haven)
library(sjmisc)
library(MatchIt)
library(gridExtra)
library(xlsx)
library(DescTools)
library(cobalt)

# set working directory 
setwd("C:/Users/Xiaoliu/OneDrive - The Ohio State University/iso data 2/final code/raw data processing")

# import dataset 
merge2 <- read.table("merge2.txt") #30004*64

# convert ln_age.w and ln_employee.w to age.w and size.w 
merge2$age.w <-  RoundTo(exp(merge2$ln_age.w),1,ceiling)
merge2$employee.w <-  RoundTo(exp(merge2$ln_employee.w),1,ceiling) #30004 *66

merge2 %>% summarise(across(c("age.w", "age"), ~ mean(.x, na.rm = TRUE)))
merge2 %>% summarise(across(c("employee.w", "size"), ~ mean(.x, na.rm = TRUE)))
summary(merge2$size)
summary(merge2$employee.w)

#propensity score estimation 
# here i didn't eliminate observations with extreme p score in control group (iso dummy = 0)
m_ps<-glm(formula = iso14001 ~ leverage.w + age.w+turnover.w+export_ratio.w+ln_employee.w+rd_ratio.w+HHI.w+log(advertising.expense.w) + public, family=binomial(), data=merge2)
merge2$pr_score <- predict(m_ps, type="response")  #30004 *67

merge2_psm <- merge2

write.table(merge2_psm,"merge2_psm.txt")







##### exploring the dep and indep variables to see if there is imbalance 
#difference-in-means: outcome variable 
merge2%>%
  group_by(iso14001)%>%
  summarise(n = n(), 
            mean_roa.w = mean(roa.w), 
            std_error = sd(roa.w)/sqrt(n))

with(merge2, t.test(roa.w~iso14001))

#difference-in-means: pre-treatment covariates 
covs0 <-subset(merge2,select = c(leverage.w,age.w,turnover.w,export_ratio.w,ln_employee.w,rd_ratio.w,HHI.w,log(advertising.expense.w),public))
merge2_cov<-c('leverage.w','age.w','turnover.w','export_ratio.w','ln_employee.w','rd_ratio.w','HHI.w','advertising.expense.w','public')
merge2%>%
  group_by(iso14001)%>%
  select(one_of(covs0))%>%
  summarise_all(funs(mean(.,na.rm = T)))

lapply(merge2_cov,function(v){
  t.test(merge2[,v]~merge2[,'iso14001'])
})
# conclusion: there are significant difference in the covariates in iso/non-iso groups 

#propensity score estimation 
m_ps<-glm(formula = iso14001 ~ leverage.w + age.w+turnover.w+export_ratio.w+ln_employee.w+rd_ratio.w+HHI.w+log(advertising.expense.w) + public, family=binomial(), data=merge2)
summary(m_ps)

prs_df <- data.frame(pr_score=predict(m_ps, type="response"), 
                     iso14001=m_ps$model$iso14001)

merge2$pr_score <- predict(m_ps, type="response")  # 30004 *67

# eliminate control variables with extreme propensity score 
# do we need this step?
tapply(merge2$pr_score, merge2$iso14001, summary)

test <- subset(merge2, pr_score > 0.03476 & pr_score < 0.66322)  #29983*67

merge2_psm <- merge2
write.table(merge2_psm,"merge2_psm.txt")

