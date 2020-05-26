install.packages("rfm")
library(rfm)

setwd("/Users/janicegunawan/Desktop/Marketing Analytics")
DT = read.csv("OnlineRetail.csv")
head(DT)
DT[, 2] = as.Date(as.character(DT[,2]),"%Y%m%d")
head(DT)

#Generate RFM Score
RFMobj = rfm_table_order(data = DT, 
                         customer_id = ID,
                         order_date = Date,
                         revenue = Amount, 
                         analysis_date = max(DT$Date))
RFMDT = RFMobj$rfm   #to extract the RFM data with RFM scores
View(RFMDT)

dim(RFMDT) #find total number of observations

rfm_rm_plot(RFMobj) #Recency vs Monetary Value
rfm_fm_plot(RFMobj) #Frequency vs Monetary Value
rfm_rf_plot(RFMobj) #Recency vs Frequency
rfm_heatmap(RFMobj) #average monetary value for different categories of recency and frequency scores
rfm_bar_chart(RFMobj) #generates the distribution of monetary scores for the different combinations of frequency and recency scores.

DT = DT[order(DT$Date,decreasing = TRUE), ] 
History_endDate = as.Date("2018-05-31") 
Forecast_endDate = as.Date("2018-06-30") 
HistDT = DT[DT$Date <= History_endDate, ] #retain the purchase records before the end of history data
ForecastDT = DT[DT$Date > History_endDate, ]

#Generate RFM Score
Hist_RFM = rfm_table_order(data = HistDT, 
                           customer_id = ID,
                           order_date = Date,
                           revenue = Amount,
                           analysis_date = History_endDate)
Hist_RFMDT = Hist_RFM$rfm

# Maintain the unique ID in the forecast data
ForecastID = ForecastDT$ID[!duplicated(ForecastDT$ID)]
# Generate the indicator whether the customer buy anything from the website.
Hist_RFMDT$Buy = ifelse(Hist_RFMDT$customer_id %in% ForecastID, 1, 0)

Reg = lm(Buy~ recency_score + frequency_score + monetary_score, data = Hist_RFMDT)
summary(Reg)

Input = data.frame(recency_score = c(5,1,1), 
                   frequency_score = c(1,5,1), 
                   monetary_score = c(1,1,5))
predict(Reg, Input)
predict(Reg, Input, interval = "confidence")

Input = data.frame(recency_score = c(1),
                   frequency_score = c(1),
                   monetary_score = c(1))
predict(Reg, Input, interval = "confidence")

#Logit function transforms all the values in the range between 0 and 1
Z = c(-1000, -200, -10, -2, 0, 1, 5, 10, 50, 500)
Logit = exp(Z) / (1 + exp(Z))
Logit

LogitReg = glm(Buy~recency_score+frequency_score+ monetary_score, 
               family=quasibinomial(link='logit'),
               data=Hist_RFMDT) 
summary(LogitReg)
predict(LogitReg, Input, type="response")
#linear is faster and easier than logit
#linear might exceed boundary, logit will be between 0 and 1 (logistic regression)
#use logit if the probabilities that the model is extreme
#use linear if it will be average or moderate 

Input = data.frame(recency_score = c(1), 
                   frequency_score = c(1), 
                   monetary_score = c(1))
predict(LogitReg, Input)

Input = data.frame(recency_score = c(5,1,1), 
                   frequency_score = c(1,5,1), 
                   monetary_score = c(1,1,5))
predict(LogitReg, Input, type="response")

#Generating the revenue in forecast period
Fore_RFM = rfm_table_order(data = ForecastDT, 
                           customer_id = ID,
                           order_date = Date,
                           revenue = Amount,
                           analysis_date = max(DT$Date))
Fore_RFMDT = Fore_RFM$rfm

#If the customer appeared in the forecast period, the revenue is the sales in that period, otherwise it is 0
Hist_RFMDT$Revenue = ifelse(Hist_RFMDT$customer_id %in% Fore_RFMDT$customer_id,
                            Fore_RFMDT$amount, 0)

RegRev = lm(Revenue ~ recency_score + frequency_score + monetary_score, data = Hist_RFMDT)
summary(RegRev)

Input = data.frame(recency_score = c(5,1,1), 
                   frequency_score = c(1,5,1), 
                   monetary_score = c(1,1,5))
predict(RegRev, Input, interval = "confidence")
