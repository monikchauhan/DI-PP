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
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=salOT), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("Over Time Salary OF MA in %") + 
  ggtitle("Over Time Salaries in MA over the Years")+coord_flip()

#Plot 2
ggplot(percentOT, aes(x = Year, y = OTFTE)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=salOTFTE), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("OT for FTE OF MA in %") + 
  ggtitle("Over Time Salaries for Full Time Employees in MA over the Years")+coord_flip()

#Plot 3
ggplot(percentOT, aes(x = Year, y = OTFTC)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=salOTFTC), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("OT for FTC OF MA in %") + 
  ggtitle("Over Time Salaries for Full Time Contractors in MA over the Years")+coord_flip()

#Plot 4
ggplot(percentOT, aes(x = Year, y = OTPTE)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=salOTPTE), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("OT for PTE OF MA in %") + 
  ggtitle("Over Time Salaries for Part Time Employees in MA over the Years")+coord_flip()

#Plot 5
ggplot(percentOT, aes(x = Year, y = OTPTC)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=salOTPTC), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("OT for PTC OF MA in %") + 
  ggtitle("Over Time Salaries for Part Time Contractors in MA over the Years")+coord_flip()