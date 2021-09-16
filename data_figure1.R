rm(list=ls())
library(cdcfluview)
library(dplyr)
library(writexl)
library(imputeTS)
library(MMWRweek)
library(ggplot2)
library(zoo)
library(tidyverse)
library(smooth)
library(Mcomp)
library(readxl)
library(egg)
library(lubridate)
library(cowplot)

###plot 1### 
#before continuing, download excel (nyc_mortality_1911-1924) from lecohen/SARS-CoV-2_pandemic-to-endemic on github#
nyc_mortality <- read_excel("~/Desktop/nyc_mortality_1911-1924.xlsx")
nyc_mortality$Date_date <- as.Date(nyc_mortality$Date)

nyc_mortality_from_1916 <- nyc_mortality[261:678,,]

nyc_mortality_from_1916 = mutate(nyc_mortality_from_1916, threeweek_avg_flu= rollmean(total_mortality, 3, align="center", fill=0))

nyc_mortality_from_1916$threeweek_avg_flu[1] <- 1831.6667
nyc_mortality_from_1916$threeweek_avg_flu[418] <- 1283.3333

colors <- c("Three Week Rolling Average"="red","Total Mortality"="pink")

plot_flu <-ggplot(data=nyc_mortality_from_1916)+
  theme_bw()+ geom_col(aes(x=Date_date, y=total_mortality), fill="pink") +
  geom_line(aes(x=Date_date, y = threeweek_avg_flu, color = "Three Week Rolling Average"), size = .75) +
  ggtitle("Total Mortality in New York City, 1916-1924") + 
  xlab("Date") + ylab("Deaths") + 
  theme(plot.title = element_text(hjust = 0.5, size = 11)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_x_date(breaks = as.Date(c("1917-01-06", "1923-12-29")), date_breaks = "1 year", date_labels = "%Y") + labs(color="Legend") + scale_color_manual(values = colors)
plot_flu

###plot 2###
flu <- who_nrevss(region = "national", years = NULL )

#extract the 3 databases from flu list and make variables comparable#
dat1 <- as.data.frame(flu$combined_prior_to_2015_16) %>% 
  filter(year>1998) %>% 
  mutate(total_a = a_h1 + a_h3 + a_2009_h1n1 + a_subtyping_not_performed + a_unable_to_subtype,
         total_b = b,
         total_flu = total_a + total_b) %>% 
  select(wk_date, total_specimens, total_flu, total_a, a_h1, a_h3, a_2009_h1n1, total_b)
dat2 <- as.data.frame(flu$public_health_labs) %>% 
  mutate(total_a = a_2009_h1n1 + a_h3 + a_subtyping_not_performed,
         total_b = b+bvic+byam,
         total_flu = total_a + total_b,
         a_h1 = 0) %>% 
  select(wk_date, total_specimens, total_flu, total_a, a_h1, a_h3, a_2009_h1n1, total_b)
dat3 <- as.data.frame(flu$clinical_labs) %>% 
  mutate(total_flu = total_a + total_b,
         a_h3 = 0,
         a_2009_h1n1 = 0,
         a_h1 = 0) %>% 
  select(wk_date, total_specimens, total_flu, total_a, a_h1, a_h3, a_2009_h1n1, total_b)


##Extrapolating percents for A subtypes##
#calculating propotion postive for each flu subtype#

dat <- rbind(dat1, dat2, dat3) %>% group_by(wk_date) %>% summarize_all(sum) %>% ungroup %>% 
  arrange(wk_date) %>% 
  mutate(propa.h1 = a_h1/(a_2009_h1n1+a_h1+a_h3),
         propa.h1 = na_interpolation(propa.h1),
         propa.2009h1n1 = a_2009_h1n1/(a_2009_h1n1+a_h1+a_h3),
         propa.2009h1n1 = na_interpolation(propa.2009h1n1),
         propa.h3 = a_h3/(a_2009_h1n1+a_h1+a_h3),
         propa.h3 = na_interpolation(propa.h3),
         ah1.int = total_a*propa.h1,
         a2009h1n1.int = total_a*propa.2009h1n1,
         ah3.int = total_a*propa.h3,
         prop.pos = total_flu/total_specimens,
         prop.pos = ifelse(is.nan(prop.pos),0,prop.pos),
         prop.b = total_b/total_specimens,
         prop.b = ifelse(is.nan(prop.b),0,prop.b),
         prop.ah1 = ah1.int/total_specimens,
         prop.ah1 = ifelse(is.nan(prop.ah1),0, prop.ah1),
         prop.a2009 = a2009h1n1.int/total_specimens,
         prop.a2009 = ifelse(is.nan(prop.a2009),0,prop.a2009),
         prop.ah3 = ah3.int/total_specimens,
         prop.ah3 = ifelse(is.nan(prop.ah3),0,prop.ah3)) %>% 
  select(wk_date, total_specimens, total_flu, total_a, ah1.int, a2009h1n1.int, ah3.int, propa.h1, propa.2009h1n1, propa.h3, total_b,prop.pos, prop.b, prop.ah1, prop.ah3, prop.a2009)
write.csv(dat, "weekly flu subtypes.csv")

#calculating rolling average#
dat = mutate(dat, threeweek_avg_prop.a2009= rollmean(prop.a2009, 3, align="center", fill=0))

#H1N1 plot#
dat_from_2009 <- dat[523:1164,,]

dat_from_2009$yr_date <- year(dat_from_2009$wk_date)

colors2 <- c("Three Week Rolling Average"="red","Confirmed H1N1 * 100 / Total Specimens Tested (Percent Positive)"="pink")

plot_h1n1_2009 <- ggplot(data=dat_from_2009)+
  theme_bw()+
  geom_col(aes(x=wk_date, y=prop.a2009*100), fill="pink") + 
  ggtitle("Seasons of Pandemic H1N1 in the US (CDC FluView)") + 
  ylab("Percent Positive (%)") + xlab("Date") + 
  theme(plot.title = element_text(hjust = 0.5, size = 11)) +
  geom_line(aes(x=wk_date, y = threeweek_avg_prop.a2009*100, color = "Three Week Rolling Average"), size = .75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(color="Legend") + scale_color_manual(values = colors2)
plot_h1n1_2009 

#y axis = Confirmed H1N1 * 100 / Total Specimens Tested (Percent Positive)#

###plot 3###
options(scipen=999)

#before continuing, download excel (owid-covid-data) from lecohen/SARS-CoV-2_pandemic-to-endemic on github#
covid <- read_excel("~/Desktop/owid-covid-data.xlsx")
##

#usa data#
covid_usa = covid[110946:111497,,]
covid_usa$tests_per_case_input <- c(16.7,	18.6,	18.2,	16.2,	17.5,	17.5,	17.8,	21.8,	20.5,	20.9,	20.6,	18,	15.8,	14.9,	12.7,	11,	10.4,	9.8,	9,	8.4,	7.7,	7.3,	7,	6.5,	5.9,	5.6,	5.3,	5.1,	5,	4.9,	4.9,	5,	5,	5,	5.3,	5.3,	5.2,	5.2,	5.4,	5.6,	5.8,	5.6,	5.7,	5.9,	6.2,	6.5,	6.6,	6.9,	7.1,	7.2,	7.8,	8.1,	8.5,	9.1,	9.4,	9.8,	10.1,	10.3,	10.5,	11,	11.3,	12.2,	12.7,	13.3,	14.1,	14.9,	15.8,	16.4,	16.9,	17.2,	17.6,	17.5,	17.8,	17.6,	17.8,	18.2,	18.7,	18.7,	19,	19.4,	20.6,	21.6,	21.8,	21.9,	22.2,	23.2,	23.3,	23.3,	24,	24,	24.5,	24.9,	24.9,	25.7,	25.8,	25.5,	25.9,	25.7,	25.6,	25.6,	25.4,	25,	24.7,	24.2,	23.6,	22.8,	21.6,	20.4,	19.9,	19.2,	18.5,	18.3,	17.8,	17.6,	17.6,	17,	16.4,	16.2,	15.7,	15.2,	15.2,	14.8,	14.7,	14.9,	14.8,	14.8,	15,	14.7,	14.7,	14.6,	14.2,	14.1,	14.2,	14.2,	14.2,	14.4,	14.4,	14.8,	14.8,	14.7,	14.8,	14.7,	14.5,	14.3,	14.2,	14.2,	14.3,	14.4,	14.8,	14.8,	15.3,	15.5,	16,	16.1,	16.1,	16,	16.6,	16.7,	17,	16.6,	16.8,	16.9,	17.2,	17.3,	17.7,	18.1,	19.1,	19.4,	19.7,	19.7,	20,	20.2,	20.2,	20.4,	20.5,	20.6,	20.7,	20.6,	20.9,	21,	20.8,	20.9,	20.9,	20.9,	21.1,	21.5,	22.4,	23.1,	23.5,	23.4,	23.6,	23.6,	23.8,	23.3,	23.7,	23.8,	23.5,	22.3,	22.4,	22.7,	22.7,	22.4,	22.2,	22.2,	23.4,	22.9,	22.9,	23.1,	22.8,	22.6,	22.9,	22.9,	23.5,	23.3,	23,	23.6,	23.5,	23.1,	23,	22.1,	21.1,	20.4,	19.5,	19.5,	19.3,	18.3,	18.4,	18.8,	18.8,	18.6,	17.7,	17.3,	17.5,	17.1,	16.6,	16.1,	15.7,	15.5,	14.6,	14.3,	13.3,	13,	12.6,	12.4,	12.1,	12.1,	11.8,	11.8,	11.4,	11.2,	10.8,	10.5,	10.5,	10.3,	10.5,	10.5,	10.4,	10.5,	10.6,	10.7,	10.7,	10.7,	10.7,	10.9,	10.3,	10.1,	9.9,	9.8,	9.6,	9.3,	9,	9.3,	9.2,	9,	8.8,	8.7,	8.6,	8.6,	8.5,	8.5,	8.4,	8.4,	8.5,	8.3,	8.2,	8.1,	8.2,	8.3,	8.3,	8.3,	8.6,	8.7,	9,	8.3,	8.2,	8.2,	8.1,	8,	7.8,	7.6,	7.3,	7.1,	7.1,	7,	7,	7,	7,	7.5,	7.5,	7.5,	7.5,	7.5,	7.6,	7.8,	8,	8.1,	8.3,	8.5,	8.8,	9.1,	9.3,	9.5,	9.9,	9.7,	10,	10.2,	10.3,	10.4,	10.6,	10.7,	10.7,	10.9,	11.1,	11.7,	12.1,	12.5,	12.7,	13.4,	13.9,	14.1,	13.7,	13.8,	13.8,	14.1,	14.5,	14.6,	14.9,	16,	16.5,	17,	17.3,	17.7,	18.2,	18.5,	18.6,	18.9,	19.2,	19.4,	19.2,	19.7,	20.2,	20.5,	20.9,	21.2,	21.7,	22.3,	22.1,	22.3,	22.5,	22.7,	22.9,	23,	22.2,	22.3,	22.3,	22.3,	22.2,	21.9,	22.3,	22.6,	22.7,	21.3,	20.8,	20.1,	19.8,	19.4,	18.7,	18.4,	19.1,	18.6,	18.8,	18.8,	19,	18.5,	18.6,	18.4,	18.5,	18.3,	18.2,	18,	18.4,	17.9,	17.9,	18.1,	18.2,	18.7,	18.8,	18.8,	19.4,	19.9,	20.3,	21,	20.9,	21.4,	22.5,	23.1,	23.4,	23.8,	23.8,	24.1,	24.1,	23.6,	23.8,	24.5,	24.9,	25.3,	26.1,	26.7,	27.9,	28.3,	28.6,	29.2,	29.5,	29.8,	30.3,	31.1,	31.4,	31.6,	31.9,	33.8,	35.1,	35.6,	35.7,	35.9,	36.3,	36.4,	36.9,	38,	39.3,	42.6,	39.5,	42.4,	46.4,	49,	48.5,	49.2,	47.9,	54.5,	51.3,	51.7,	47,	49.2,	49.3,	50.3,	50.6,	53.1,	54.9,	56.7,	55.7,	56.1,	55.9,	55.4,	55.1,	53,	50.9,	51.6,	51.6,	49.7,	48.6,	47.2,	45.7,	41.9,	42.6,	41.8,	43.7,	36.6,	34.2,	33.2,	29,	28.4,	28.5,	25.1,	25.7,	24.3,	23.4,	20.3,	20.2,	19.9,	18.8,	18.2,	17.5,	16.3,	14.9,	14.6,	14.9,	13.8,	13.2,	13.1,	12.9,	11.4,	11.5,	11.4,	11,	10.7,	10.6,	10.5,	10.1,	10.2,	10.4,	9.9,	9.9,	9.4,	9.3,	9.7,	9.8,	9.8,	9.5,	9.4,	9.6,	9.5,	9.4,	9.3,	9.4,	9.2,	9.3,	9.4,	9.4,	9.7,	10,	10.1,	9.9,	10,	9.7,	9.8,	9.9,	9.8,	9.8,	11.1,	9.2,	9,	8.6,	7.8)

covid_usa_streamlined <- select(covid_usa, "date", "new_cases", "tests_per_case_input")
covid_usa_streamlined$totalspecimenstested <- covid_usa_streamlined$new_cases * covid_usa_streamlined$tests_per_case_input  
covid_usa_streamlined$prop_covid <- covid_usa_streamlined$new_cases / covid_usa_streamlined$totalspecimenstested
covid_usa_streamlined <- mutate(covid_usa_streamlined, threeweek_avg_covid= rollmean(prop_covid, 21, align="center", fill=0)) 
covid_usa_streamlined$date <- as.Date(covid_usa_streamlined$date, "%Y-%m-%d")

percent_covid <- covid_usa_streamlined$prop_covid * 100
coeff <- 10000

colors3 <- c("Confirmed SARS-CoV-2 * 100 / Total Specimens Tested (Percent Positive)"="red","New Cases"="pink")

plot_cov_2020 <-ggplot(data=covid_usa_streamlined)+
  theme_bw()+ geom_col(aes(x=date, y=new_cases/coeff), fill="pink") + geom_line(aes(x=date, y = percent_covid, color = "Confirmed SARS-CoV-2 * 100 / Total Specimens Tested (Percent Positive)"), size = .75) +
  ggtitle("SARS-CoV-2 Waves in the US, 2020-2021 (Our World in Data)") + 
  xlab("Date") + 
  theme(plot.title = element_text(hjust = 0.5, size = 11)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_y_continuous(name = "Percent Positive (%)", limits=c(0,40), breaks=c(0,10,20,30,40), sec.axis = sec_axis(~.*coeff, name="New Cases of SARS-CoV-2"))+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + theme(axis.text.x = element_text(angle = 90)) + labs(color="Legend") + scale_color_manual(values = colors3)
plot_cov_2020

#y axis = Confirmed SARS-CoV-2 * 100 / Total Specimens Tested (Percent Positive)#

#plot the three plots 

plot_grid(plot_flu + theme(legend.justification = c(0,1)), plot_h1n1_2009 + theme(legend.justification = c(0,1)), plot_cov_2020 + theme(legend.justification = c(0,1)), nrow=3, ncol=1, align="v", rel_heights = c(1,1,1.25))

