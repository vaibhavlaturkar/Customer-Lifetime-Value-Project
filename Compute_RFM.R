
##################### COMPUTE Recency,Frequency,Monetary Value ##########################
#########################################################################################


library(sqldf)


## We have created a function to generate R,F,M

RFM<- function(data,Customerid,Days_Since,Total_Amount){
  library(sqldf)
  rfm  <- sqldf("SELECT CustomerID,MIN(Days_Since) AS 'Recency',COUNT(CustomerID) AS 'Frequency',AVG(Total_Amount) AS 'Monetary_Value' FROM data GROUP BY 1")
  return(rfm)
}

## Quarter 1 Customers
customers_q1<- RFM(data_q1,data_q1$CustomerID,data_q1$Days_Since,data_q1$Total_Amount)
head(customers_q1)
summary(customers_q1)

## Quarter 2 Customers
customers_q2<- RFM(data_q2,data_q2$CustomerID,data_q2$Days_Since,data_q2$Total_Amount)
head(customers_q2)
summary(customers_q2)

## Quarter 3 Customers
customers_q3<- RFM(data_q3,data_q3$CustomerID,data_q3$Days_Since,data_q3$Total_Amount)
head(customers_q3)
summary(customers_q3)

## Quarter 4 Customers
customers_q4<- RFM(data_q4,data_q4$CustomerID,data_q4$Days_Since,data_q4$Total_Amount)
head(customers_q4)
summary(customers_q4)




##########################################################################################################################// Scoring R,F,M // ################################################################################################################################


# Do Scoring for Q1 data

summary(customers_q1)

### We will remove the Outliers on IQR Rule

# Frequency
d <- removeOutliers(customers_q1$Frequency)
customers_q1 <- customers_q1[-d,]
summary(customers_q1)
hist(customers_q1$Frequency)


# Monetary Value

hist(customers_q1$Monetary_Value)

c <- removeOutliers(customers_q1$Monetary_Value)
customers_q1 <- customers_q1[-c,]
summary(customers_q1)
hist(customers_q1$Monetary_Value)


## All the Outliers are removed 

############################ We will Score R,F,M #########################################

# We will score them on quartiles

# Recency Scoring

customers_q1$Rec_Score <- ifelse(customers_q1$Recency <= 46,5,ifelse(customers_q1$Recency > 46 & customers_q1$Recency <= 66,4,ifelse(customers_q1$Recency > 66 & customers_q1$Recency <= 72,3,ifelse(customers_q1$Recency > 72 & customers_q1$Recency <= 106,2,1))))

head(customers_q1)
tail(customers_q1)


# Frequency Scoring

customers_q1$Freq_Score <- ifelse(customers_q1$Frequency <= 10,1,ifelse(customers_q1$Frequency > 10 & customers_q1$Frequency <= 19,2,ifelse(customers_q1$Frequency > 19 & customers_q1$Frequency <= 25,3,ifelse(customers_q1$Frequency > 25 & customers_q1$Frequency <= 36,4,5))))

head(customers_q1)
tail(customers_q1)



# Monetary Scoring

36/5

customers_q1$Mon_Score <- ifelse(customers_q1$Monetary_Value <= 7,1,ifelse(customers_q1$Monetary_Value > 7 & customers_q1$Monetary_Value <= 14,2,ifelse(customers_q1$Monetary_Value > 14 & customers_q1$Monetary_Value <= 21,3,ifelse(customers_q1$Monetary_Value > 21 & customers_q1$Monetary_Value <= 28,4,5))))

head(customers_q1)
tail(customers_q1)



### concatenate RFM SCORE 

#We will concatenate Rec_score,Freq_score,Mon_score into RFM_Score by using paste function

customers_q1$RFM_Value<- paste(customers_q1$Rec_Score,customers_q1$Freq_Score,customers_q1$Mon_Score)

# We will check RFM_score
head(customers_q1)


## Remove the Spcae between the numbers from RFM Value Colunm using Stringr package

library(stringr)

class(customers_q1$RFM_Value)

customers_q1$RFM_Value <- str_replace_all(customers_q1$RFM_Value, fixed(" "), "")

head(customers_q1)

customers_q1$RFM_Value <- as.integer(customers_q1$RFM_Value)

head(customers_q1)
summary(customers_q1)
#Next Segmentation
#########################################################################################