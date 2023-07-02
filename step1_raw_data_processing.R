# code copied from data_clean file 

# load library 
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
library(readxl)
library(dplyr)

# set working directory 
setwd("C:/Users/Xiaoliu/OneDrive - The Ohio State University/iso data 2/final code/raw data processing")

# import manufacturing data 
m2001 <- read_excel("2001_manufacturing.xlsx")
m2002 <- read_excel("2002_manufacturing.xlsx")
m2003 <- read_excel("2003_manufacturing.xlsx")
m2004 <- read_excel("2004_manufacturing.xlsx")
m2005 <- read_excel("2005_manufacturing.xlsx")
m2006 <- read_excel("2006_manufacturing.xlsx")
m2007 <- read_excel("2007_manufacturing.xlsx")
m2008 <- read_excel("2008_manufacturing.xlsx")
m2009 <- read_excel("2009_manufacturing.xlsx")
m2010 <- read_excel("2010_manufacturing.xlsx")
m2011 <- read_excel("2011_manufacturing.xlsx")
# drop an extra column called ..22 in 2011 manu df 
# m2011$..22 <- NULL
m2012 <- read_excel("2012_manufacturing.xlsx")
m2013 <- read_excel("2013_manufacturing.xlsx")
m2014 <- read_excel("2014_manufacturing.xlsx") # 5245

# import iso id 
iso_set_ID <- read_excel("iso_set_ID.xlsx", col_types = "text")  #added "col_types = "text" on 4/12/2023 
View(iso_set_ID)
# import regional data 
ID_Region <- read_excel("ID-Region.xlsx")
View(ID_Region)
# import cleaned iso data with info on year, id, ksic, iso dummy, export ratio, ln_employee, ln_age, public, rd_ratio, HHI 
DATA_ISO14_Xs <- read_excel("DATA_ISO14_Xs.xlsx")
View(DATA_ISO14_Xs)
DATA_advrt_cost <- read_excel("DATA-advrt cost.xlsx", sheet = "merge ", col_types = "text")  #added "col_types = "text" on 4/12/2023 
View(DATA_advrt_cost)

# rename DATA_iso14_xs
names(DATA_ISO14_Xs) <- c("year","ID","ksic","iso14001","export_ratio","ln_employee","ln_age","public","rd_ratio","HHI")


##### deal with manufacturing data #####
# assign same column names to each df and assign df back to global enviro
colnames <- c("year","ID","net income","total assets",
              "investment assets","current assets","inventory",
              "tangible assets","assumulated depreciation","total liabilities",
              "current liabilities","non-current liabilities","long-term debt",
              "total equity","capital stock","sales","labor cost","interest expense",
              "rent cost","tax cost","depreciation cost")

dflist <- list(m2001=m2001,m2002=m2002,m2003=m2003,m2004=m2004,
               m2005=m2005,m2006=m2006,m2007=m2007,m2008=m2008,
               m2009=m2009,m2010=m2010,m2011=m2011,m2012=m2012,
               m2013=m2013,m2014=m2014)
list2env(lapply(dflist, setNames, colnames), .GlobalEnv)

# drop the 22nd column in m2011
# m2011 <- m2011[ -c(22) ]
# merge all df by rows 
merged <- rbind(m2001,m2002,m2003,m2004,m2005,m2006,m2007,m2008,m2009,m2010,m2011,m2012,m2013,m2014)  #73430 *21

View(merged)

# reomve obs with NA in total assets 
merged<-merged[!is.na(merged$`total assets`),]  #69883*21

# generate dependent variable: roa, roe and ros   
merged <- merged %>%
  mutate(roa = `net income` / `total assets`) %>%
  mutate(roe = `net income` / `total equity`) %>%
  mutate(ros = `net income` / sales)  #69883*24

# generate independent variable 
# financial risk 
merged <- merged %>%
  mutate(leverage = `total liabilities` / `total equity`)

# turnover 
merged <- merged %>%
  mutate(turnover = sales / `total assets`)  # 69883*26

##### merge 'merged' and 'iso_set_id' #####
# merge two data sets, include industry, start year ...
merged2 <-  merge(merged, iso_set_ID,all.merged = TRUE)  #69883*31

##### merge 'merged2' with cleaned iso data #####
# rename DATA_iso14_xs
names(DATA_ISO14_Xs) <- c("year","ID","ksic","iso14001","export_ratio","ln_employee","ln_age","public","rd_ratio","HHI")

# merge two data sets, include age, size, exportrotio ....
merged3 <- merge(merged2,DATA_ISO14_Xs, all.merged = TRUE )  #30004 *38

##### merge 'merged3' with advertising data #####
# rename cols of adv data 
names(DATA_advrt_cost) <- c("ID","stock","advertising expense","year")
merged4 <- merge(merged3, DATA_advrt_cost, all.merged = TRUE)  #30004 * 40 

##### merge 'merged4' with regional data #####
merged5 <- merge(merged4, ID_Region, all.merged = TRUE)   #30004*41

#reorder data frame   
merge3 <- merged5[c(1,2,3,41,31,32,36,23,24,25,17,26,27,33,34,35,37,38,40,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,28,29,30,39)]  #30004 *41

merge2 <-merge3
merge2[is.na(merge2)] <- 0   # 30004 *41


# multiply depend variable *100 
merge2$roa <- merge2$roa*100
merge2$roe <- merge2$roe*100
merge2$ros <- merge2$ros*100

#### winsorize #####
merge2$roa.w <- winsor(merge2$roa)
merge2$roe.w <- winsor(merge2$roe)
merge2$ros.w <- winsor(merge2$ros)
merge2$sales.w <- winsor(merge2$sales)
merge2$leverage.w <- winsor(merge2$leverage)
merge2$turnover.w <- winsor(merge2$turnover)
merge2$export_ratio.w <- winsor(merge2$export_ratio)
merge2$ln_employee.w <- winsor(merge2$ln_employee)
merge2$ln_age.w <- winsor(merge2$ln_age)
merge2$rd_ratio.w <- winsor(merge2$rd_ratio)
merge2$HHI.w <- winsor(merge2$HHI)
merge2$`advertising expense` <- as.numeric(merge2$`advertising expense`)  # I have adv as character in this dataframe, therefore I have to convert adv to a numerical column
merge2$advertising.expense.w <- winsor(merge2$`advertising expense`)

# check is roa is less than roe
roa <- data.table(merge2$year,merge2$ID,merge2$roa,merge2$roa.w)
View(roa)
summary(roa)
roe <- data.table(merge2$year,merge2$ID,merge2$roe,merge2$roe.w)
summary(roe)

# generate age and number of employee
merge2$year <- as.numeric(merge2$year)
merge2$startyear <- as.numeric(merge2$startyear)
merge3 <- merge2 %>%
  mutate(age = year - startyear + 1) %>%
  mutate(size = round(exp(ln_employee),digits=0)) #30004 *55

# so far, I have winsorized financial variables, number of employees, firm age should keep the same (no winsorized)


##### create IV #####
# create IV 
merge2$one <- 1
merge2 <- merge2 %>%
  ungroup()

merge2<- merge2 %>%
  group_by(year,ksic) %>%
  mutate(yr_ksic_iso_sum = sum(iso14001)) %>%
  mutate(yr_ksic_sum = sum(one)) %>%
  mutate(yr_ksic_iso_sum_1 = yr_ksic_iso_sum- iso14001 ) %>%
  mutate(yr_ksic_sum_1 = yr_ksic_sum -1)%>%
  mutate(iv_in = yr_ksic_iso_sum_1/yr_ksic_sum_1) %>%
  ungroup()

merge2 <- merge2 %>%
  group_by(year) %>%
  mutate(yr_sum_iso = sum(iso14001) -yr_ksic_iso_sum) %>%
  mutate(yr_sum = sum(one) - yr_ksic_sum) %>%
  mutate(iv_out = yr_sum_iso/yr_sum) %>%
  ungroup()   #30004 *62

# correlation test, kendall, pearson, spearman and it's no more highly correlated 

iv_na <- merge2[is.na(merge2$iv_in),]
View(iv_na)

ksic12 <- subset(merge2, ksic == 12)
View(ksic12)

# replace na in iv_in with 0 
merge2$iv_in[is.na(merge2$iv_in)] <- 0

cor(merge2$iv_in,merge2$iv_out,method = c("kendall"))
cor(merge2$iv_in,merge2$iv_out,method = c("spearman"))
cor(merge2$iv_in,merge2$iv_out,method = c("pearson"))

merge2 <- merge2 %>%
  mutate(age = year - startyear + 1) %>%
  mutate(size = round(exp(ln_employee),digits=0)) #30004 *64


##### write the data 
write.table(merge2,"merge2.txt")  
# This final dataframe has 62 columns 
