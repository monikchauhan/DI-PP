library(dplyr)
library(ggplot2)
salary <- read.csv("MApayroll.csv")
names(salary)
#Extracting selected columns
salaryOT<-select(salary,1,2,3,4,6,8,9,11)
names(salaryOT)
levels(salaryOT$position_type)
#grouping yearwise entire salary
salaryOT<-salaryOT%>%group_by(Year)
#getting counts in single dataframe
salaryOTyear<-dplyr::summarize(salaryOT,total=n(),FTE=sum(position_type=="Full Time Employee"),PTE=sum(position_type=="Part Time Employee"),FTC=sum(position_type=="Full Time Contractor"),PTC=sum(position_type=="Part Time Contractor"))
salaryOTyear
#percentage of different categories
salaryOTyear$FTEpercent<-with(salaryOTyear,(FTE/total)*100)
salaryOTyear
salaryPercent<-salaryOTyear%>%mutate(PTEpercent=(PTE/total)*100,FTCpercent=(FTC/total)*100,PTCpercent=(PTC/total)*100)
salaryPercent
#Plot 1

ggplot(salaryPercent, aes(x = Year, y = FTEpercent)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=FTE), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("Full Time Employees OF MA in %") + 
  ggtitle("Full Time Employees in MA over the Years")+coord_flip()
#Plot 2
ggplot(salaryPercent, aes(x = Year, y = FTCpercent)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=FTC), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("Full Time Contractors OF MA in %") + 
  ggtitle("Full Time Contractors in MA over the Years")+coord_flip()
#Plot 3
ggplot(salaryPercent, aes(x = Year, y = PTEpercent)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=PTE), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("Part Time Employees OF MA in %") + 
  ggtitle("Part Time Employees in MA over the Years")+coord_flip()
#Plot 4
ggplot(salaryPercent, aes(x = Year, y = PTCpercent)) +
  geom_bar(stat = "identity", fill = "red") + 
  geom_text(aes(label=PTC), position=position_dodge(width=0.9), vjust=0) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   
  xlab("Year") + ylab("Part Time Contractors OF MA in %") + 
  ggtitle("Part Time Contractors in MA over the Years")+coord_flip()
