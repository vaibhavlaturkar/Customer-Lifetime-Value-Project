
################################ RFM Analysis ##########################################

#############################   Data Cleaning ##########################################

Online <- read.csv("G:/CLV Project/Project/online_retail.csv")

str(Online)

names(Online)
summary(Online)

## There are 135080 NA is Customer Id we will remove them 

library(sqldf)
library(ggplot2)
library(outliers)

## For RFM analysis we need only 4 colunms CustomerID,InvoiceDate,Quantity,UnitPrice


data_clean <- sqldf("Select CustomerID,InvoiceDate,Quantity,UnitPrice from Online where CustomerID is not null")

summary(data_clean)

## We have removed the NA rows from the data of Customer ID colunm

## We will check the outliers in the data
## we will remove the negative value and 0 value from quantity and unit price
library(dplyr)
data_clean <- data_clean %>%
  filter(Quantity >=1 ) %>%
  filter(UnitPrice >=0.5)

summary(data_clean)

# We have removed the Negative and 0 value 

# Now we will remove Outliers on basis of IQR Rule

removeOutliers = function(x) { 
  # Get Q1 and Q3
  qnt = quantile(x, probs=c(.25, .75))
  
  # Get the interquartile range time 1.5
  iqt = 1.5 * IQR(x)
  
  # Apply on a copy of the original data
  y = x 
  y = which(x >(qnt[2] + iqt))
  
  # Remove incomplete cases and return the resulted variable
  return(y)
}

(a <- removeOutliers(data_clean$Quantity))

# We got Index numbers of Outliers from Quantity

data_clean <- data_clean[-a,]
summary(data_clean)
hist(data_clean$Quantity)
boxplot(data_clean$Quantity)


# We will check for Unit price

(b <- removeOutliers(data_clean$UnitPrice))

data_clean <- data_clean[-b,]
summary(data_clean)
hist(data_clean$UnitPrice)
boxplot(data_clean$UnitPrice)


############################################################################################################################# Data Modification ##################################


##################### Add Total Amount Column ######################

# We need to add total amount of colunm by multiplying quantity * Unit price

data_clean$Total_Amount <- data_clean$Quantity * data_clean$UnitPrice

head(data_clean)
summary(data_clean)



# We will check the class of date 
class(data_clean$InvoiceDate)
# class of date is factor we need to change it to date

data_clean$InvoiceDate <- as.character(data_clean$InvoiceDate)
data_clean$InvoiceDate <- as.Date(data_clean$InvoiceDate,"%d-%m-%Y")

summary(data_clean)





################## Filter the data into four period ##################

# we have data from Dec 2010 to Dec 2011 
# we will divide the data in three period i.e 
# From DEc 2010- MARCH 2011 as Q1 of 4 Months 
# From APRIL 2011 - JUNE 2011 as Q2 of 3 Months
# From JULY 2011 -SEP 2011 as Q3 of 3 Months
# From OCT 2011 - DEC 2011 as Q4 of 3 Months



# We will add new Month and Year columns by using lubridate package

library(lubridate)

data_clean$Year <- year(data_clean$InvoiceDate)
data_clean$Month <- month(data_clean$InvoiceDate)

head(data_clean)
summary(data_clean)



############################################################################################################################# Divide data ######################################3

# keeping this in mind we will put range for our next data sets which are quarter wise

library(dplyr)

## QUARTER 1
# We will take the data from Dec 2010 to March 2011 as data_Q1
data_q1 <- filter(data_clean, InvoiceDate < '2011-03-01')
summary(data_q1)

## QUARTER 2
# And from April 2011 to June 2011 as data_Q2
data_q2 <- filter(data_clean, Year == '2011' & Month == c(4,5,6) )
summary(data_q2)

## QUARTER 3
# And from July 2011 to Sep 2011 as data_Q3
data_q3 <- filter(data_clean, Year == '2011' & Month == c(7,8,9) )
summary(data_q3)

## QUARTER 4
# And from Oct 2011 to Dec 2011 as data_Q4
data_q4 <- filter(data_clean, Year == '2011' & Month == c(10,11,12) )
summary(data_q4)

########################## Add Days_Since colunm ###################

## QUARTER 1
data_q1$Days_Since <- as.integer(difftime(time1 = "2011-04-02",time2 = data_q1$InvoiceDate,units = "days"))
summary(data_q1)


data_q2$Days_Since <- as.integer(difftime(time1 = "2011-07-02",time2 = data_q2$InvoiceDate,units = "days"))
summary(data_q2)

data_q3$Days_Since <- as.integer(difftime(time1 = "2011-10-02",time2 = data_q3$InvoiceDate,units = "days"))
summary(data_q3)

data_q4$Days_Since <- as.integer(difftime(time1 = "2012-01-02",time2 = data_q4$InvoiceDate,units = "days"))
summary(data_q4)


