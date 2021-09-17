rm(list=ls())
library(ggplot2)
library(ggExtra)
library(cowplot)

###plot1###
#https://academic.oup.com/jid/article/222/1/17/5820656#
#N=12,628, surveillance (all ages)#
#Detection of Seasonal Coronaviruses at General Practitioner Visits from 2005-2017#
#West Scotland, United Kingdom (Nickbaksh et al, 2020)#

agegroup1 <- c(rep("<1" , 3) , rep("1-5" , 3) , rep("6-16" , 3) , rep("17-46" , 3) , rep("47-64" , 3) , rep("65+" , 3))
virus1 <- rep(c("229E" , "OC43" , "NL63") , 6)
value1 <- c(0.42,	2.94,	2.1,	0.56,	3.07,	2.09, 1.18,	2.35,	1.24, 1.39,	2.16,	1.29, 1.15,	2.58,	1.15, 1.24,	2.91,	0.95)
data1 <- data.frame(agegroup1,virus1,value1)
data1$virus1 <- factor(data1$virus1, levels = c("229E" , "OC43" , "NL63"))
data1$agegroup1 <- factor(data1$agegroup1, levels = c("<1", "1-5", "6-16", "17-46", "47-64", "65+"))

plot1 <- ggplot(data1, aes(fill=virus1, y=value1, x=agegroup1)) + geom_bar(width=0.7, position=position_dodge(width=0.8), stat="identity", color="black") + scale_fill_brewer(palette="Set1") + xlab("Age (Years)") + ylab("Percent Positive (%)") + ggtitle("Detection of Seasonal Coronaviruses at General Practitioner Visits from 2005-2017\nWest Scotland, United Kingdom (Nickbaksh et al, 2020)") + theme_bw() + removeGrid(x = TRUE, y = TRUE) + theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Strain")
plot1

###plot2###
#https://academic.oup.com/jid/article/222/1/9/5815743?guestAccessKey=b19eb499-007a-4ebf-a4a8-bb6a7481ec0c3#
#HIVE Cohort - Michigan - Incidence per 100 person-years#
#Incidence (per 100 Person-Years) of Seasonal Coronaviruses in the HIVE Cohort from 2010-2018#
#Michigan, United States (Monto et al, 2020)#

agegroup2 <- c(rep("<5" , 4) , rep("5-11" , 4) , rep("12-17" , 4) , rep("18-49" , 4) , rep("50+" , 4))
virus2 <- rep(c("229E" , "OC43" , "NL63", "HKU1") , 5)
value2 <- c(1.35, 8.37, 5.77, 3.67, 1.18, 3.52, 3.62, 1.68, 1.53, 2.65, 1.99, 0.765, 1.86, 3.65, 3.16, 2.37, 2.86, 2.83, 1.53, 0.765)
data2 <- data.frame(agegroup2,virus2,value2)
data2$virus2 <- factor(data2$virus2, levels = c("229E" , "OC43" , "NL63", "HKU1"))
data2$agegroup2 <- factor(data2$agegroup2, levels = c("<5", "5-11", "12-17", "18-49", "50+"))

plot2 <- ggplot(data2, aes(fill=virus2, y=value2, x=agegroup2)) + geom_bar(width=0.7, position=position_dodge(width=0.8), stat="identity", color="black") + scale_fill_brewer(palette="Set1") + ylab("Incidence Per 100 Person-Years") + xlab("Age (Years)") + ggtitle("Incidence (per 100 Person-Years) of Seasonal Coronaviruses in the HIVE Cohort from 2010-2018\nMichigan, United States (Monto et al, 2020)") + theme_bw() + removeGrid(x = TRUE, y = TRUE) + theme(plot.title = element_text(hjust = 0.5)) + labs(fill="Strain")
plot2

plot_grid(plot1, plot2, ncol=1, nrow=2)


