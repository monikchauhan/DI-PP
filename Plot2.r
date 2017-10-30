library(dplyr)
library(ggplot2)
salary <- read.csv("MApayroll.csv")
names(salary)

#Extracting selected columns
salaryOT<-select(salary,1,2,3,4,6,8,9,10,11,12)
names(salaryOT)

#Grouping salary by year
salaryOT<-salaryOT%>%group_by(Year)

#Extracting overtime salaries
OTsal<-dplyr::summarize(salaryOT,
                        salTot=sum(pay_total_actual), 
                        salOT=sum(pay_overtime_actual[pay_overtime_actual!=0]), 
                        salOTFTE=sum(pay_overtime_actual[pay_overtime_actual!=0 & position_type=="Full Time Employee"]),
                        salOTFTC=sum(pay_overtime_actual[pay_overtime_actual!=0 & position_type=="Full Time Contractor"]),
                        salOTPTE=sum(pay_overtime_actual[pay_overtime_actual!=0 & position_type=="Part Time Employee"]),
                        salOTPTC=sum(pay_overtime_actual[pay_overtime_actual!=0 & position_type=="Part Time Contractor"]))
OTsal

#percentage salaries
percentOT<-OTsal %>% mutate(OT=(salOT/salTot)*100,
                            OTFTE=(salOTFTE/salTot)*100,
                            OTFTC=(salOTFTC/salTot)*100,
                            OTPTE=(salOTPTE/salTot)*100,
                            OTPTC=(salOTPTC/salTot)*100)
percentOT

# Plotting Salaries

#Plot 1
ggplot(percentOT, aes(x = Year, y = OT)) +
  geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label=paste(round(salOT/1e6,2), "M")), position=position_dodge(width=0.9), size=2, vjust=0, hjust=0) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +   
  xlab("Year") + ylab("Total Over Time Payout in %") + 
  ggtitle("Total Over Time Payouts for all position types in MA")+coord_flip()

#Plot 2
ggplot(percentOT, aes(x = Year, y = OTFTE)) +
  geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label=paste(round(salOTFTE/1e6,2),"M")), position=position_dodge(width=0.9), size=2, vjust=0, hjust=0) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +   
  xlab("Year") + ylab("Total OT Payout to Full time Employees in %") + 
  ggtitle("Total Over Time payouts to Full Time Employees in MA")+coord_flip()

#Plot 3
ggplot(percentOT, aes(x = Year, y = OTFTC)) +
  geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label=paste(round(salOTFTC/1e3,2),"K")), position=position_dodge(width=0.9), size=2, vjust=0, hjust=0) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +   
  xlab("Year") + ylab("Total OT Payout to Full time Contractors in %") + 
  ggtitle("Total Over Time payouts to Full Time Contractors in MA")+coord_flip()

#Plot 4
ggplot(percentOT, aes(x = Year, y = OTPTE)) +
  geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label=paste(round(salOTPTE/1e6,2),"M")), position=position_dodge(width=0.9),size=2, vjust=0, hjust=0) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +   
  xlab("Year") + ylab("Total OT Payout to Part time Employees in %") + 
  ggtitle("Total Over Time payouts to Part Time Employees in MA")+coord_flip()

#Plot 5
ggplot(percentOT, aes(x = Year, y = OTPTC)) +
  geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label=paste(round(salOTPTC/1e3,2), "K")), position=position_dodge(width=0.9),size=2, vjust=0, hjust=0) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +   
  xlab("Year") + ylab("Total OT Payout to Part time Contractors in %") + 
  ggtitle("Total Over Time payouts to Part Time Contractors in MA")+coord_flip()