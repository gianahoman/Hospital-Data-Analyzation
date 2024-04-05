library(dplyr)
library(ggplot2)


hospitals = read.csv("hospitals.csv")
#1)Exploratory Analysis

#a) How big is the dataset?
hospitaldatasize <- dim(hospitals)
hospitaldatasize


#b)What are the names of the columns?
hospitalattributes<- colnames(hospitals)
hospitalattributes

#c) What data types are each column?
datatypes<- sapply(hospitals,class)
datatypes

#d) Are there missing values? - no missing values
missingvalues<- colSums(is.na(hospitals))
missingvalues

#e) Which hospital has the lowest number of beds?
lowestbeds = hospitals%>% filter(Beds==min(Beds))
lowestbeds

#f) Which hospital has the lowest expense?
lowestexpense = hospitals%>% filter(Total.Expense==min(Total.Expense))
lowestexpense

#g) How many hospitals deliver babies?
deliveries= hospitals%>%filter(Births.or.Not==1)
deliveries

#h) Using ggplot, scatterplot number of beds vs Total Expense
ggplot(hospitals,aes(x=Beds, y = Total.Expense)) + geom_point() +
  xlab("Number of Beds") +
  ylab("Total Expense") +
  ggtitle("Number of Beds vs Total Expense")

#i) Using ggplot, scatterplot Admissions vs Total Expense
ggplot(hospitals,aes(x=Admissions, y = Total.Expense)) + geom_point() +
  xlab("Admission") +
  ylab("Total Expense") +
  ggtitle("Admissions vs Total Expense")

#j) Using dplyr and ggplot, scatterplot beds vs Total Expense
#but only for hospitals that deliver babies'

babyhospitals<-hospitals %>% filter(Births > 0)
ggplot(babyhospitals,aes(x=Beds, y = Total.Expense)) + geom_point() +
  xlab("Beds") +
  ylab("Total Expenses") +
  ggtitle("Delivery Hospitals vs Total Expense")

#k) One more question that you believe would be useful.
#What is the distribution of admissions based on payroll expense?
adminexp <- hospitals %>%
  select(Admissions, Payroll.Expense)

ggplot(adminexp, aes(x = Payroll.Expense, y = Admissions)) +
  geom_point() +
  xlab("Payroll Expense") +
  ylab("Admissions") +
  ggtitle("Admissions vs Payroll Expense")

#2) Descriptive Analytics

#I)PieChart

totaloutpatientvisits <- sum(hospitals$Outpatient.Visits)
totaladmissions <- sum(hospitals$Admissions)

piedf <- data.frame(
  Attribute = c("Admissions", "Outpatient Visits"),
  Value = c(totaladmissions, totaloutpatientvisits)
)

piedf %>%
  ggplot(aes(x = "", y = Value, fill = Attribute)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("skyblue", "orange")) +
  labs(fill = "Attribute", y = NULL) +
  ggtitle("Admissions and Outpatient Visits in Hospitals")

#There are many more outpatient visists in comaprison to admissions, meaning that maybe there should be a bigger facility to meet
#outpatient demands.

#II)BarChart


totalpersonnel <- sum(hospitals$Personnel)

bardf <- data.frame(
  Attribute = c("Admissions", "Personnel"),
  Total = c(totaladmissions, totalpersonnel)
)

bardf %>%
  ggplot(aes(x = Attribute, y = Total, fill = Attribute)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Total", fill = NULL) +
  ggtitle("Total Admissions vs Personnel")

#III)LineChart

expenses <- hospitals %>%
  group_by(Beds) %>%
  summarise(totalexpense = sum(Total.Expense),
            payrollexpense = sum(Payroll.Expense))

ggplot(expenses, aes(x = Beds)) +
  geom_line(aes(y = totalexpense, color = "Total Expense")) +
  geom_line(aes(y = payrollexpense, color = "Payroll Expense")) +
  labs(x = "Beds", y = "Expenses", color = "Expense Type", 
       title = "Total Expense and Payroll Expense by Beds")


#3)Simple Regression 
#ii)
ggplot(hospitals, aes(x = Beds , y = Total.Expense)) +
geom_point() 

reg1 <- lm(Total.Expense ~ Beds, hospitals)
summary(reg1)


#iii) What is the value of the R^2?
#The value of R^2 is  0.6043.

#iv) What does the R^2 measure in this case?
#The R^2 measures the percentage of variability(approximately 60.43%) of Total Expense that can be explained by the number of Beds.

#v) What are the pvalues ? How many pvalues are reported, why ?What does each pvalue mean?
#The pvalue is <2.2e-16(less than 0.05, reject null hypothesis and Beta is not equal to zero),as there is only one reported value(shown 
#twice but they are both the same) as this is simple regression as there is only one independent variable. 
#The low p-values obtained from the regression analysis suggests that the relationship between the number of beds and total expenses observed in the sample 
# hospital dataset is likely to generalize to the broader population of hospitals, aiding in making inferences about hospital populations based on the sample data.


#vi)Explain R square, pvalues.
#R^2 measure how well a regression model speaks to the fit of a dataset as it is representing the percentage of variation of the total expense
# explained by beds. The pvalues always measures the significance of the regression model through hypothesis tests, as it represents the probability of observing the given result,
#or a more extreme result,under the null hypothesis. Here, we observe of the pvalue is smaller or larger than 0.05, so we can determine significance.


#vii)What would be the right attribute size (independent variable) that seems most appropriate to lead you in the expense range of $55–$75 million?
#The proper attribute size to fall within the expense range of 55-75 million dollars ranges from 50720.37 beds and 69201.77 beds.

#55,000,000 = 1084.56x -16060.93 
#   x= 50720.37       
#75,000,000= 1084.56x -16060.93 
#   x = 69201.77      


#4) Multivariate Regression


#i)


ggplot(hospitals, aes(x = Beds, y = Total.Expense, color = Personnel)) +
  geom_point() + labs(title = "Beds and Personnel on Total Expense")

reg2 = lm(Total.Expense ~ Personnel + Beds, hospitals)
summary(reg2)

#ii) What is the value of the R^2?
#The value of R^2 is  0.9121.

#iii) What does the R^2 measure in this case?
#The R^2 measures the percentage of variability(approximately 91.21%) in the dependent variable (Total Expense) that is explained by the change 
#in the independent variables (Personnel and Beds) included in the regression model

#iv) What are the pvalues ? How many pvalues are reported, why ?What does each pvalue mean?
#There are two pvalues(not including the overall pvalue), as this is multivariate regression in which multiple independent variables are included in 
#regression (in this case two : Beds and Personnel).The coefficient for Personnel reveals the pvalue < 2e-16 (less than 0.05), indicating
#that there is a significant relationship between personnel and total expenses in hospitals.
#The coefficient for Beds reveals the pvalue of 1.05e-11(less than 0.05) also suggesting a highly significant relationship 
#between the number of Beds and Total Expenses. The overall pvalue < 2.2e-16, which communicates that there is a significant relationship between 
#beds and personnel, with total expenses, as we reject the null and accept the alternative hypothesis. We can conclude both Beta1 and Beta2 are not equal to zero
#The obtained low p-values provide insights into how the relationships among personnel, number of beds, and total expenses observed in the sample data are indicative 
#of trends that likely extend to the broader population of hospitals.


#v) Explain R square, pvalues.
#R^2 measure how well a regression model speaks to the fit of a dataset as it is representing the percentage of variation of the total expense
# explained by beds and personnel.In this case the R^2 represents the model very well as it is very close to 1. The pvalues always measures the significance 
#of the regression model through hypothesis tests, as it represents the probability of observing the given result, or a more extreme result,under the null 
#hypothesis. Here, we observe of the pvalue is smaller or larger than 0.05, so we can determine significance and the overall and individual pvalues confirm
#that the model is significant.


#5)
#                                 I choose Option A to be better based on the regressions performed.
# - The multivariate regression revealed a stronger R^2(=0.9121) in comparison to the R^2(=0.6043) in our simple regression, hence our indicators or independent 
#   variables(Beds and Personnel) explain the change in total expense better together than beds does alone.
# - As stated in class, this is to be expected as the more columns or attributes added to a regression the higher the R^2 becomes, 
#   which is also why there is an adjusted R^2.

#Based on the regressions and plots, the recommendation leans towards building a smaller hospital facility. This decision is supported by the fact that the
#multivariate model with fewer beds and fewer personnel explains the variation in total expenses effectively, and the range of expenses aligns well 
#with the needs of a smaller facility.
# The range of expense (55 - 75 million),  the range of beds(90-100) of a smaller facility fits best within our expense range,
#hence one again based on all of my regression and plots Option A would be my choice.

