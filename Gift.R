#interested in invoice no, cust id, quant, unit price
library(rfm)
setwd("/Users/janicegunawan/Desktop/Marketing Analytics")
DT = read.csv("UKGift.csv")
View(head(DT))

#Clean Data
DT[, "Date"] = as.Date(as.character(DT[, "Date"]),"%m/%d/%Y") 
DT = DT[DT$Cancel == 0, ]
DT$Amount = DT$Quantity * DT$UnitPrice

#Historical Period and Forecast Period
summary(DT$Date)
DT = DT[order(DT$Date,decreasing = TRUE), ] 
History_endDate = as.Date("2011-11-09") 
Forecast_endDate = as.Date("2011-12-09")
HistDT = DT[DT$Date <= History_endDate, ] 
ForecastDT = DT[DT$Date > History_endDate, ]

#Generate RFM Scores
Hist_RFM = rfm_table_order(data = HistDT, 
                           customer_id = CustomerID,
                           order_date = Date,
                           revenue = Amount,
                           analysis_date = History_endDate)
Hist_RFMDT = Hist_RFM$rfm

# Maintain the unique ID in the forecast data
ForecastID = ForecastDT$CustomerID[!duplicated(ForecastDT$CustomerID)]
# Generate the indicator whether the customer buy anything from the website.
Hist_RFMDT$Buy = ifelse(Hist_RFMDT$customer_id %in% ForecastID, 1, 0)

Reg = lm(Buy ~ recency_score + frequency_score + monetary_score, data = Hist_RFMDT)
summary(Reg)

#Generating the revenue in forecast period
Fore_RFM = rfm_table_order(data = ForecastDT, 
                           customer_id = CustomerID,
                           order_date = Date,
                           revenue = Amount,
                           analysis_date = max(DT$Date))
Fore_RFMDT = Fore_RFM$rfm
Hist_RFMDT$Revenue = ifelse(Hist_RFMDT$customer_id %in% Fore_RFMDT$customer_id,
                            Fore_RFMDT$amount, 0)

RegRev = lm(Revenue ~ recency_score + frequency_score + monetary_score, data = Hist_RFMDT)
summary(RegRev)

Input = data.frame(recency_score = c(4,5,3,2,2), 
                   frequency_score = c(2,1,1,2,3), 
                   monetary_score = c(2,1,1,3,4))
predict(Reg, Input, interval = "confidence")
predict(RegRev, Input, interval = "confidence")

Input2 = data.frame(recency_score = c(5,1,1), 
                   frequency_score = c(1,5,1), 
                   monetary_score = c(1,1,5))
predict(Reg, Input2, interval = "confidence")
predict(RegRev, Input2, interval = "confidence")
