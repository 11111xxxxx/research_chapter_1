library(plyr)
library(dplyr)
library(tidyr)
library(Hmisc)
library(Formula)
library(ggplot2)
library(survival)
library(lattice)
library(psych)
library(fBasics)
library(timeDate)
library(timeSeries)
library(stargazer)
library(parallel)
library(robustbase)
library(perry)
library(robustHD)
library(DescTools)
library(data.table)
library(car)
library(carData)
library(AER)
library(plm)
library(gplots)
library(panelView)
library(dynlm)
library(Matrix)
library(lfe)
library(scales)
library(ggplot2)
library(xtable)
library(reporttools)
# change working directory to import and save dataframe
getwd()
setwd("C:/Users/Xiaoliu/OneDrive - The Ohio State University/iso data 2/final code/raw data processing")

# import merge data set, which includes all other variables 
merge2_psm <- read.table("merge2_psm.txt")
exchange_rate <- read_excel('annual_exchange_rate.xlsx')

mean_rate <- mean(exchange_rate$usd_per_won)

merge2_psm$sales.usd <- merge2_psm$sales.w*mean_rate/1000000
merge2_psm$adv.usd <- merge2_psm$advertising.expense.w*mean_rate/1000
merge2_psm$net.income.usd <- merge2_psm$net.income*mean_rate/1000000
merge2_psm$total.assets.usd <- merge2_psm$total.assets*mean_rate/1000000
merge2_psm$total.equity.usd <- merge2_psm$total.equity*mean_rate/1000000

  
##### explore the unit problem for variable sale and adv expenses 
merge2_psm %>%
  select(c('sales.w','sales_usd','advertising.expense.w','adv_usd')) %>%
  as.data.frame() %>%
  stargazer(covariate.labels = c("winsorized sales","Sales (millions in USD)","winsorized advertising expense","Advertising Expense (thousands in USD)"),type="text")

##### ##### ##### ##### ##### ##### ##### #####  #########  
##### generate summary statistics for overall sample ##### 
##### ##### ##### ##### ##### ##### ##### #####  #########  
merge2_psm %>%
  select(c("sales.usd","roa.w","roe.w","leverage.w","turnover.w","export_ratio.w",
           "employee.w","age.w","rd_ratio.w","adv.usd","public","iso14001")) %>%
  as.data.frame() %>%
  stargazer(covariate.labels = c("Sales (millions in USD)","ROA (%)","ROE (%)",
                                 "Financial Leverage","Assets Turnover ratio","Export Ratio",
                                 "Firm Size","Firm Age (year)","R&D Ratio",
                                 "Advertising Expense (thousands in USD)","Public Dummy","ISO 14001 Dummy"),type="text",digits = 2)


##### ##### ##### ##### ##### ##### ##### #####  ######  
##### generate summary statistics for iso14001=1  #####  
##### ##### ##### ##### ##### ##### ##### #####  ######  
merge2_psm_1 <- as.data.frame(merge2_psm[merge2_psm$iso14001==1,c("sales.usd","roa.w","roe.w","leverage.w","turnover.w","export_ratio.w",
                                                                  "employee.w","age.w","rd_ratio.w","adv.usd","public","iso14001")])
stargazer(merge2_psm_1,
          covariate.labels = c("Sales (millions in USD)","ROA (%)","ROE (%)","Financial Leverage",
                               "Assets Turnover ratio","Export Ratio","Firm Size","Firm Age (year)","R&D Ratio",
                               "Advertising Expense (thousands in USD)","Public Dummy"),type="text",digits = 2)


##### ##### ##### ##### ##### ##### ##### #####  ######  
##### generate summary statistics for iso14001=0  #####  
##### ##### ##### ##### ##### ##### ##### #####  ######  

merge2_psm_0<- as.data.frame(merge2_psm[merge2_psm$iso14001==0,c("sales.usd","roa.w","roe.w","leverage.w","turnover.w","export_ratio.w",
                                                                 "employee.w","age.w","rd_ratio.w","adv.usd","public","iso14001")])
stargazer(merge2_psm_0,
          covariate.labels = c("Sales (millions in USD)","ROA (%)","ROE (%)","Financial Leverage",
                               "Assets Turnover ratio","Export Ratio","Firm Size","Firm Age (year)","R&D Ratio",
                               "Advertising Expense (thousands in USD)","Public Dummy"),type="text",digits = 2)


##### ##### ##### ##### ##### ##### ##### #####  ######  
##### ##### ##### time trend plot   ##### #####  ######  
##### ##### ##### ##### ##### ##### ##### #####  ######  


# test if there is significant time trend 
full_data_ct <- merge2_psm
myvars <- c("ID","year","sales.w","roa.w","roe.w","iso14001","public","ksic","region","leverage.w","turnover.w","export_ratio.w","ln_employee.w","rd_ratio.w","HHI.w","advertising.expense.w","age.w","pr_score") # select specific columns used to generate causal forest 
full_data_ct <- full_data_ct[myvars]
full_data_ct$year2 <- ifelse(full_data_ct$year==2001,1,
                             ifelse(full_data_ct$year==2002,2,
                                    ifelse(full_data_ct$year==2003,3,
                                           ifelse(full_data_ct$year==2004,4, 
                                                  ifelse(full_data_ct$year==2005,5,
                                                         ifelse(full_data_ct$year==2006,6,
                                                                ifelse(full_data_ct$year==2007,7,
                                                                       ifelse(full_data_ct$year==2008,8,
                                                                              ifelse(full_data_ct$year==2009,9,
                                                                                     ifelse(full_data_ct$year==2010,10,
                                                                                            ifelse(full_data_ct$year==2011,11,
                                                                                                   ifelse(full_data_ct$year==2012,12,
                                                                                                          ifelse(full_data_ct$year==2013,13,
                                                                                                                 ifelse(full_data_ct$year==2014,14,NA))))))))))))))
ols_sales <- lm(log(sales.w) ~ year2,
                data = full_data_ct)    # only include time trend 

ols_roa <- lm(roa.w ~ year2,
              data = full_data_ct)    # only include time trend 

ols_roe <- lm(roe.w ~ year2,
              data = full_data_ct)    # only include time trend 


stargazer(ols_sales,ols_roa,ols_roe,covariate.labels = "year",title = "time trend in our data",dep.var.labels = c("Sales","ROA","ROE"), type='text')

## For each year, calculate mean x for sales,roa,roe,
means <- merge2_psm %>% 
  group_by(year) %>%
  dplyr::summarize(sales_mean = mean(sales.usd),roa_mean= mean(roa.w),roe_mean = mean(roe.w),net_income_mean=mean(net.income.usd),
                   total_assets_mean=mean(total.assets.usd),total_equity_mean=mean(total.equity.usd))%>%
  ungroup()

##### a function for the trend plot ##### 
trend_plot <- function(df,count,xlabel,ylabel)  {
  ggplot(df, aes_string(x=names(means)[1],y=names(means)[count])) + 
    geom_line() +
    geom_point() +
    theme_bw() +
    xlab(xlabel) + ylab(ylabel)+
    scale_x_continuous(breaks=seq(2001, 2014, 2))
}

trend_plot_sales<- trend_plot(means,2,"Year","Sales (millions in USD)")
trend_plot_roa <- trend_plot(means,3,"Year","ROA")
trend_plot_roe <- trend_plot(means,4,"Year","ROE")
trend_plot_net_income <- trend_plot(means,5,"Year","Net Income (millions in USD)")
trend_plot_total_assets <- trend_plot(means,6,"Year","Total Assets (millions in USD)")
trend_plot_total_equity <- trend_plot(means,7,"Year","Total Equity (millions in USD)")

library(ggpubr)
ggarrange(trend_plot_net_income, trend_plot_total_assets, trend_plot_roa,
          ncol = 3, nrow = 1)   ###put three plots related to ROA in one figure 

ggarrange(trend_plot_net_income, trend_plot_total_equity, trend_plot_roe,
          ncol = 3, nrow = 1)   ###put three plots related to ROE in one figure 


##### ##### ##### ##### ##### ##### ##### #####  ######  
##### ##### ##### detrend sales     ##### #####  ######  
##### ##### ##### ##### ##### ##### ##### #####  ######  
