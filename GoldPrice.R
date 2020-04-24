library(readxl)
library(tidyverse)

gold_data <- read_excel("C:\\Users\\rashjain\\Documents\\Gold_Historical_Prices.xlsx")
summary(gold_data)


#Checking NA's in all Columns'
for (col in colnames(gold_data)){
  print(sum(is.na(gold_data[[col]])))
}

#Since there are no empty cell in the columns so we can proceed without dropping NA's

gold_data_temp <- 0
Yearly_Avg <- vector()
Lower_CI <- vector()
Upper_CI <- vector()
Year <- vector()
j <- 1

#Loop between years 2000-2018
for (i in 2000:2018){
  Year[j] = paste(i,sep="")
  gold_data_temp <- subset(gold_data, Date >= paste(i,"-01-01",sep="") & Date <= paste(i,"-12-31",sep=""))
  
  #finding the yearly avg
  Yearly_Avg[j] = mean(gold_data_temp$Price)
  
  #finding the SD, Upper CI limit and lower CI limit 
  SD = sd(gold_data_temp$Price)
  n = nrow(gold_data_temp)
  Lower_CI[j] = Yearly_Avg[j]-(1.96*(SD/sqrt(n)))
  Upper_CI[j] = Yearly_Avg[j]+(1.96*(SD/sqrt(n)))
  j <- j+1
}

print(Year)
print(Yearly_Avg)
print(Lower_CI)
print(Upper_CI)

Gold_Price_Avg <- cbind(Year,Yearly_Avg,Lower_CI,Upper_CI )
View(Gold_Price_Avg)

#Plotting the graph between years and yearly_average
plot(Year, Yearly_Avg )

#writing to csv file
write.csv(Gold_Price_Avg, file = "Gold_Prices_Summary.csv")

