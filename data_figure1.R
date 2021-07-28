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
library(gridExtra)
library(grid)

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

plot_h1n1_2009 <- ggplot(data=dat_from_2009)+
  theme_bw()+
  geom_col(aes(x=wk_date, y=prop.a2009*100), fill="pink") + 
  ggtitle("Seasons of Pandemic H1N1 in the US (CDC FluView)") + 
  ylab("Percent Positive (%)") + xlab("Date") + 
  theme(plot.title = element_text(hjust = 0.5, size = 11)) +
  geom_line(aes(x=wk_date, y = threeweek_avg_prop.a2009*100), color = "red", size = .75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
plot_h1n1_2009 

#y axis = Confirmed H1N1/Total Specimens * 100 (% Positive)#

#covid plot#
options(scipen=999)

#before continuing, download excel from lecohen/SARS-CoV-2_pandemic-to-endemic from github#
covid <- read_excel("~/Downloads/owid-covid-data.xlsx")
##

#usa data#
covid_usa = covid[94995:95359,,]
covid_usa$tests_per_case_input <- c(6,	5.8,	5.2,	5.2,	5.2,	5.8,	6.5,	6.7,	6.2,	7.1,	8,	8.2,	8.3,	8.5,	8.3,	8.1,	7.8,	7.8,	7.8,	7.5,	7.1,	6.8,	6.6,	6.4,	6.2,	5.9,	5.7,	5.5,	5.3,	5.3,	5.2,	5.2,	5.2,	5.1,	5.2,	5.4,	5.3,	5.3,	5.3,	5.5,	5.6,	5.8,	5.7,	5.8,	6.1,	6.4,	6.5,	6.7,	6.9,	7,	7.2,	7.6,	7.9,	8.3,	8.7,	9.2,	9.7,	9.9,	10.1,	10.4,	11,	11.2,	11.6,	12.2,	12.8,	13.3,	13.9,	14.5,	15.2,	15.6,	15.6,	16.1,	15.9,	16.1,	16.1,	16.4,	16.7,	17.2,	17.5,	17.9,	18.9,	19.6,	20.8,	21.3,	21.3,	21.7,	22.7,	22.2,	22.2,	22.7,	22.7,	23.3,	23.8,	23.3,	23.8,	23.8,	23.8,	23.8,	23.8,	23.3,	23.3,	22.7,	22.7,	22.2,	21.7,	20.8,	20,	19.6,	18.9,	18.2,	17.2,	16.7,	16.4,	15.9,	15.9,	15.9,	15.4,	15.2,	14.7,	14.3,	14.1,	14.1,	13.9,	13.7,	13.9,	13.9,	13.9,	13.9,	13.7,	13.9,	13.9,	13.7,	13.5,	13.7,	13.9,	13.9,	13.9,	14.1,	14.5,	14.7,	14.9,	14.9,	14.9,	14.9,	14.9,	14.9,	15.2,	14.9,	14.9,	15.2,	15.2,	15.4,	15.6,	15.9,	16.4,	16.7,	16.9,	16.9,	17.2,	17.5,	17.5,	17.9,	18.2,	17.9,	18.2,	18.5,	18.9,	19.2,	19.6,	20,	20,	20.4,	20.4,	20.4,	20.4,	20.4,	20.8,	20.8,	20.4,	20.8,	21.3,	20.8,	20.8,	20.8,	20.8,	21.3,	21.3,	21.7,	22.7,	23.3,	23.3,	23.3,	23.8,	23.8,	23.8,	23.8,	23.8,	23.8,	23.3,	22.2,	22.7,	22.7,	22.2,	22.2,	22.2,	22.2,	22.7,	22.2,	22.2,	22.2,	22.2,	22.2,	22.2,	22.7,	22.7,	22.7,	23.3,	23.3,	22.7,	22.7,	21.3,	20.8,	20,	19.2,	18.9,	18.9,	18.5,	18.9,	18.9,	18.9,	18.9,	18.2,	17.5,	17.2,	16.9,	16.4,	16.1,	15.4,	15.2,	14.9,	14.5,	14.1,	13.7,	13.2,	12.7,	13,	12.5,	12.2,	11.8,	11.4,	11.1,	10.8,	10.3,	10.1,	10.2,	10.3,	10.4,	10.3,	10.4,	10.4,	10.5,	10.6,	10.6,	10.5,	10.4,	10.2,	10.1,	9.9,	9.8,	9.4,	9.2,	9.1,	9,	8.8,	8.7,	8.5,	8.5,	8.5,	8.6,	8.5,	8.5,	8.4,	8.3,	8.5,	8.5,	8.3,	8.3,	8.4,	8.5,	8.5,	8.6,	8.7,	8.9,	8.8,	8.5,	8.6,	8.3,	8.1,	7.8,	7.4,	7.2,	7,	6.8,	6.9,	6.8,	6.8,	7,	7,	7.4,	7.4,	7.4,	7.6,	7.8,	7.9,	8.3,	8.3,	8.5,	8.8,	8.8,	9.2,	9.3,	9.6,	9.9,	10.1,	9.9,	10.1,	10.4,	10.6,	11.1,	11.1,	11.4,	11.2,	11.4,	11.8,	12.3,	12.5,	13.2,	13.3,	14.1,	14.7,	14.7,	14.5,	14.5,	14.5,	14.7,	14.9,	15.2,	15.6,	16.7,	17.5,	18.2,	18.5,	19.2,	19.2,	19.6,	20,	20.4,	20.4,	20,	20.4,	20.8,	21.3,	21.7,	21.7,	22.2)

covid_usa_streamlined <- select(covid_usa, "date", "new_cases", "tests_per_case_input")
covid_usa_streamlined$totalspecimenstested <- covid_usa_streamlined$new_cases * covid_usa_streamlined$tests_per_case_input  
covid_usa_streamlined$prop_covid <- covid_usa_streamlined$new_cases / covid_usa_streamlined$totalspecimenstested
covid_usa_streamlined <- mutate(covid_usa_streamlined, threeweek_avg_covid= rollmean(prop_covid, 21, align="center", fill=0)) 

covid_usa_streamlined_2020 <- covid_usa_streamlined[1:285,,]
percent_covid <- covid_usa_streamlined$prop_covid * 100
coeff <- 10000

plot_cov_2020 <-ggplot(data=covid_usa_streamlined)+
  theme_bw()+ geom_col(aes(x=date, y=new_cases/coeff), fill="pink") + geom_line(aes(x=date, y = percent_covid), color = "red", size = .75) +
  ggtitle("SARS-CoV-2 Waves in the US, 2020-2021 (Our World in Data)") + 
  xlab("Date") + 
  theme(plot.title = element_text(hjust = 0.5, size = 11)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_y_continuous(name = "Percent Positive (%)", limits=c(0,40), breaks=c(0,10,20,30,40), sec.axis = sec_axis(~.*coeff, name="New Cases of SARS-CoV-2")) 
plot_cov_2020

#y axis = New Cases/Total Specimens * 100 (% Positive)#

g1 <- ggplotGrob(plot_h1n1_2009)  
g2 <- ggplotGrob(plot_cov_2020)

g <- rbind(g1, g2) 

set_panel_widths <- function(g, widths){
  g$widths <- grid:::unit.list(g$widths) 
  id_panels <- unique(g$layout[g$layout$name=="panel", "t"])
  g$widths[id_panels] <- widths
  g
}

g <- set_panel_widths(g, lapply(1:2, grid::unit, "null")) 
grid::grid.draw(g)


                   
                   
                   
                   
                   
                   
                   
                   
