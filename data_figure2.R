rm(list=ls())
library(ggplot2)
library(readxl)
library(ggExtra)
library(facetscales)
library(scales)
library(patchwork)
library(cowplot)

###plot1-h1n1###
#source= https://gis.cdc.gov/grasp/fluview/flu_by_age_virus.html#
#before continuing, download excel from lecohen/SARS-CoV-2_pandemic-to-endemic github#
age_h1n1 <- read_excel("~/Downloads/age_h1n1_cdc.xlsx")
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=11)
  )

age_h1n1$age <- factor(age_h1n1$age, levels=c("0-4", "5-24", "25-64", "65+"))
age_h1n1_bp <- ggplot(data=age_h1n1, aes(x="", y=percentage, fill=age)) + geom_bar(width = 1, stat = "identity") + facet_grid(~season)
pie_chart <- age_h1n1_bp + coord_polar("y", start=0)
pie_chart1 <- pie_chart + blank_theme + theme(axis.text.x=element_blank()) + ggtitle("Age Distribution of Pandemic H1N1 Over Four US Flu Seasons (CDC)") + theme(plot.title = element_text(hjust = 0.75))


###plot2-DHF###
#source= https://pubmed.ncbi.nlm.nih.gov/25064368/ (Karyanti et al, 2014)#
#before continuing, download excel from lecohen/SARS-CoV-2_pandemic-to-endemic github#
dengue_cases_age <- read_excel("~/Downloads/age_dengue_karyanti.xlsx")
dengue_cases_age$age <- factor(dengue_cases_age$age, levels=c("<1", "1-4", "5-14", "15+"))
dengue_cases_age_bp <- ggplot(data=dengue_cases_age, aes(x="", y=percentage, fill=age)) + geom_bar(width = 1, stat = "identity") + facet_grid(~year)
pie_chart2 <- dengue_cases_age_bp + coord_polar("y", start=0)
pie_chart3 <- pie_chart2 + blank_theme + theme(axis.text.x=element_blank()) + ggtitle("Age Distribution of Indonesia Dengue Hemorrhagic Fever Incidence per 100,000 Population (Karyanti, 2014)") + theme(plot.title = element_text(size = 10.5))

###plot3-COVID###
#source= CDC (as of March, 2021)#
#before continuing, download excel from lecohen/SARS-CoV-2_pandemic-to-endemic github#
covid_cases_age <- read_excel("~/Downloads/age_covid_cdc.xlsx")
covid_cases_age$age <- factor(covid_cases_age$age, levels=c("0-13", "14-24", "25-64", "65+"))
covid_cases_age_bp <- ggplot(data=covid_cases_age, aes(x="", y=Percentage, fill=age)) + geom_bar(width = 1, stat = "identity") + facet_grid(~Month)
pie_chart4 <- covid_cases_age_bp + coord_polar("y", start=0)
pie_chart5 <- pie_chart4 + blank_theme + theme(axis.text.x=element_blank()) + ggtitle("Age Distribution of US COVID-19 Incidence per 100,000 Population (CDC)") + theme(plot.title = element_text(hjust = 0.75))

plot_grid(pie_chart1, pie_chart3, pie_chart5, ncol=1, nrow=3)



