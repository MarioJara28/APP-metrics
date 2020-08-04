##As discussed please use the attached documents for the below mentioned questions. 
#Please make sure to submit the case study by Thursday, June 18th at 4pm Dubai time.


#Excel reading
install.packages("readxl")
library(readxl)

#working with dates
install.packages("lubridate")
library(lubridate)

##Data wrangling
install.packages("tidyverse")
library(tidyverse)

##SQL manipulation package
install.packages("sqldf")
library(sqldf)

#Graphics package
install.packages("ggplot2")
library(ggplot2)


#############################################Datasets sent from txxxxt
example <- read_xlsx("C:/Users/mario/Desktop/Downloads/assignment_example_order_data.xlsx")
example <- as.data.frame(example)
example[,1] <- as_date(example[,1])
example[,1]<- round_date(example[,1], "months")

casedata <- read_xlsx("C:/Users/mario/Desktop/Downloads/Case study data - Cohorts.xlsx")
casedata <- as.data.frame(casedata)
casedata[,2] <- as_date(casedata[,2])

casedata[,2]<- round_date(casedata[,2], "months")

#OBJECTIVE 1-  Using the Cohort Analysis Technique, please analyse the growth of orders and clients on a monthly basis.
######Also, analyse seasonality of the orders for this dataset. 
#(Seasonality is defined by an event occurring at a specific regular interval less than a year, such as weekly, monthly, or quarterly). 
#Please use sql, tableau/any visualization tool and use the workbook 'case study data - cohorts'


#In order to prepare the date im going to use an R package called SQLDF; basically this package mymics
#sql syntax to manipulate and mainly group the date accordingly to the objetive.

############################################SQL queries##############################################

##Inner join of both datasets
inner <- sqldf("select example.*,casedata.*   from example
inner join
casedata
on
iso_date=Date
")



#Grouping of casedata
clientgrouping <- sqldf("select  Date, count(distinct(Client)) as clients from casedata group by 1")
clientmin <- sqldf("select  min(Date) as date , Client as clients from casedata group by 2")


clientmin[,1] <- as_date(clientmin[,1])

ordersgrouping <- sqldf("select  Date, count(distinct(Order_id)) as orders from casedata group by 1")
#######################################################################################################



#Technique used: Cohort analysis
#Client growth monthly

growth_rate = clientgrouping %>%
  # first sort by year
  arrange(Date) %>%
  mutate(Diff_month = month(Date) - lag(month(Date)),  # Difference in time (just in case there are gaps)
         Diff_growth = clients - lag(clients), # Difference in route between years
         Rate_percent = Diff_growth/lag(clients) * 100
         ) # growth rate in percent
Average_growth = mean(growth_rate$Rate_percent, na.rm = TRUE)


print(growth_rate)
print(Average_growth)
write.csv(growth_rate, "growth_rate_clients.csv")
write.csv(Average_growth, "average_growth_clients.csv")


#Orders growth monthly

growth_rate = ordersgrouping %>%
  # first sort by year
  arrange(Date) %>%
  mutate(Diff_month = month(Date) - lag(month(Date)),  # Difference in time (just in case there are gaps)
         Diff_growth = orders - lag(orders), # Difference in route between years
         Rate_percent = Diff_growth/lag(orders) * 100
  ) # growth rate in percent
Average_growth = mean(growth_rate$Rate_percent, na.rm = TRUE)


print(growth_rate)
print(Average_growth)


write.csv(growth_rate, "growth_rate_orders.csv")
write.csv(Average_growth, "average_growth_orders.csv")



###################################################Cohort analysis
appname <- "Talabat"


dbdata <- as.data.frame(cbind(casedata[,3],substr(casedata[,2], 1, 7))) 


z <- list("user_id", "yw")
colnames(dbdata)<- z
colnames(dbdata)
dbdata <- sqldf("select * from dbdata group by 1,2")


cohort <- dbdata %>%           
  group_by(user_id) %>%        
  mutate(first = min(yw)) %>%  
  group_by(first, yw) %>%      
  summarise(users = n()) %>%   
  spread(yw, users)            



shiftrow <- function(v) {
  # put a vector in, strip off leading NA values, and place that amount at the end
  first_na_index <- min( which(!is.na(v)) )
  
  
  c(v[first_na_index:length(v)], rep(NA, first_na_index-1))
}

# create a new dataframe, with shifted rows (and keep the first one)
shifted <- data.frame(
  cohort = cohort$first,
  t(apply( select(as.data.frame(cohort), 2:ncol(cohort)), # 2nd column to the end
           1, # for every row
           shiftrow ))
)

# and make column names readable
# first should be "cohort" and the rest month.<number>
colnames(shifted) <- c("cohort", sub("","month.", str_pad(1:(ncol(shifted)-1),2,pad = "0")))

# percentages
shifted_pct <- data.frame(
  cohort = shifted$cohort, # first column
  shifted[,2:nrow(shifted)+1] / shifted[["month.01"]] # rest: divide by month.01
)

######### 4: prepare plot data

plotdata_abs <- gather(shifted,     "cohort_age", "people"  ,2:ncol(shifted    ))
plotdata_pct <- gather(shifted_pct, "cohort_age", "percent" ,2:ncol(shifted_pct))


labelnames <- c( plotdata_abs$people[1:(ncol(shifted)-1)],
                 plotdata_pct$percent[(ncol(shifted)):(nrow(plotdata_pct))])

#Labels
pretty_print <- function(n) {
  case_when( n <= 1  ~ sprintf("%1.0f %%", n*100),
             n >  1  ~ as.character(n),
             TRUE    ~ " ") # for NA values, skip the label
}

# create the plot data
plotdata <- data.frame(
  cohort     = plotdata_pct$cohort,
  cohort_age = plotdata_pct$cohort_age,
  percentage = plotdata_pct$percent,
  label      = pretty_print(labelnames)
)

######### 5: plot
# plot (with reordered y axis, oldest group on top)


plotdata[which(plotdata$percentage == 1), "percentage"] <- 0

ggplot(plotdata, aes(x = cohort_age, y = reorder(cohort, desc(cohort)))) +
  geom_raster(aes(fill = percentage)) +
  scale_fill_continuous(guide = FALSE) + # no legend
  geom_text(aes(label = label), color = "white") +
  xlab("cohort age") + ylab("cohort") + 
  ggtitle(paste("Retention table (cohort) for",appname, "app"))


#################################################################################################



#Orders seasonality
#Required package for time series
install.packages("forecast")
library(forecast)

casedata <- read_xlsx("C:/Users/mario/Desktop/Downloads/Case study data - Cohorts.xlsx")
casedata <- as.data.frame(casedata)




x <- casedata %>%           # store in cohort table, get from dbdata
  select(Order_id, Date)%>%
  group_by(Date) %>%        # group all users together
  summarise(n = n())

timeseries <- ts(x$n, start=c(2017, 10,01), end=c(2018, 10,01), frequency=365)

tim <- plot.ts(timeseries)


stl(timeseries)
decompose(timeseries)
##Results: there is no seasonality, although strong trend



#OBJECTIVE 2- 6-month, monthly forecast results for orders; you are free to use any methodology but please specify the method along with all assumptions taken during the exercise. Please use the workbook 'assignment example order data' for this exercise.
#Technique used: time series model (probably arima), taking into account seasonality and ramadan period.


example <- read_xlsx("C:/Users/mario/Desktop/Downloads/assignment_example_order_data.xlsx")
example <- as.data.frame(example)



y <- sqldf("select iso_date, avg(is_ramadan), avg(is_important_day), avg(is_holiday), sum(succ_orders) from example group by 1")
y[,1]<- as_date(y[,1])




timeseries <- ts(y$`sum(succ_orders)`, start=c(2017, 10,01), end=c(2018, 10,01), frequency=365)

tim <- plot.ts(timeseries)


stl(timeseries)
decompose(timeseries)

##Results of relevant variables for predicting orders
m <- summary(lm(y$`sum(succ_orders)`~ y$`avg(is_ramadan)`+ y$`avg(is_important_day)`+ y$`avg(is_holiday)`))
print(m)






# double exponential - models level and trend. No seasonality, as we discovered there is none
fit <- HoltWinters(timeseries, gamma=FALSE)
forecast(fit, 183)
plot(forecast(fit, 183))




#A stationary time series is one whose properties do not depend on the time at which the series is observed. As is the case, thus arima advised

##ARIMA
fit <- auto.arima(timeseries)
forecast(fit, 183)
plot(forecast(fit, 183))

#As seeen, arima provides a quite strightforward prediction

#Key insights to take from the results

##1: different types of customers regarding retention rate (fluctuates); probably a customer segmentation of active users makes sense
##2: growth in new customers have been stalled
##3: growth in new orders is negative
##4: no seasonality of orders; however strong trend upwards. White noise quite present
##5 arima models are prescribed since no seasonality
##6 ramadan and holidays DO have an impact in sales; not so important days (not significant predictor)
##7 Ramadan do have a negative impact in sales; thus prescribed marketing actions for night time (or less couriers during this period of time)
##Holidays DO have a strong positive effect in sales; thus marketing actions in these days are not advised






