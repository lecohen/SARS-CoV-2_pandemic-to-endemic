rm(list=ls())
library(ggplot2)
library(readxl)
library(ggExtra)
library(scales)
library(patchwork)
library(cowplot)

###plot1-1918flu###
#before continuing, download excel (nyc_mortality_age_and_season) from lecohen/SARS-CoV-2_pandemic-to-endemic on github#
nyc_mortality <- read_excel("~/Downloads/nyc_mortality_age_and_season.xlsx")

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=11)
  )

nyc_mortality$age <- factor(nyc_mortality$age, levels=c("0-5", "5-25", "25-65", "65+"))
nyc_mortality_bp <- ggplot(data=nyc_mortality, aes(x="", y=percentage, fill=age)) + geom_bar(width = 1, stat = "identity") + facet_grid(~season) + labs(fill="Age (Years)")
pie_chart <- nyc_mortality_bp + coord_polar("y", start=0)
pie_chart1 <- pie_chart + blank_theme + theme(axis.text.x=element_blank()) + ggtitle("Age Distribution of Total Mortality in NYC Pre and Post 1918 Influenza Pandemic") + theme(plot.title = element_text(hjust = 0.5))

###plot2-2009h1n1###
#source= https://gis.cdc.gov/grasp/fluview/flu_by_age_virus.html#
#before continuing, download excel (age_h1n1_cdc) from lecohen/SARS-CoV-2_pandemic-to-endemic on github#
age_h1n1 <- read_excel("~/Downloads/age_h1n1_cdc.xlsx")
age_h1n1$age <- factor(age_h1n1$age, levels=c("0-4", "5-24", "25-64", "65+"))
age_h1n1_bp <- ggplot(data=age_h1n1, aes(x="", y=percentage, fill=age)) + geom_bar(width = 1, stat = "identity") + facet_grid(~season) + labs(fill="Age (Years)") 
pie_chart2 <- age_h1n1_bp + coord_polar("y", start=0)
pie_chart3 <- pie_chart2 + blank_theme + theme(axis.text.x=element_blank()) + ggtitle("Age Distribution of Pandemic H1N1 Over Four US Flu Seasons") + theme(plot.title = element_text(hjust = 0.5))


###plot3-DHF###
#source= https://pubmed.ncbi.nlm.nih.gov/25064368/ (Karyanti et al, 2014)#
#before continuing, download excel (age_dengue_karyanti) from lecohen/SARS-CoV-2_pandemic-to-endemic on github#
colors5 <- c("<1"="hotpink","1-4"="darkgreen","5-14"="royalblue1","15+"="mediumorchid4")
dengue_cases_age <- read_excel("~/Downloads/age_dengue_karyanti.xlsx")
dengue_cases_age$age <- factor(dengue_cases_age$age, levels=c("<1", "1-4", "5-14", "15+"))
dengue_cases_age_bp <- ggplot(data=dengue_cases_age, aes(x="", y=percentage, fill=age)) + geom_bar(width = 1, stat = "identity") + facet_grid(~year) + labs(fill="Age (Years)") + scale_fill_manual(values = colors5)
pie_chart4 <- dengue_cases_age_bp + coord_polar("y", start=0)
pie_chart5 <- pie_chart4 + blank_theme + theme(axis.text.x=element_blank()) + ggtitle("Age Distribution of Indonesia Dengue Hemorrhagic Fever Incidence per 100,000 Population") + theme(plot.title = element_text(hjust = 0.5))

###plot4-COVID###
#source= CDC (as of March, 2021)#
#before continuing, download excel (age_covid_cdc) from lecohen/SARS-CoV-2_pandemic-to-endemic on github#
covid_cases_age <- read_excel("~/Downloads/age_covid_cdc.xlsx")
covid_cases_age$age <- factor(covid_cases_age$age, levels=c("0-13", "14-24", "25-64", "65+"))
covid_cases_age_bp <- ggplot(data=covid_cases_age, aes(x="", y=Percentage, fill=age)) + geom_bar(width = 1, stat = "identity") + facet_grid(~Month) + labs(fill="Age (Years)")
pie_chart6 <- covid_cases_age_bp + coord_polar("y", start=0)
pie_chart7 <- pie_chart6 + blank_theme + theme(axis.text.x=element_blank()) + ggtitle("Age Distribution of US COVID-19 Incidence per 100,000 Population") + theme(plot.title = element_text(hjust = 0.5))

plot_grid(pie_chart1, pie_chart3, pie_chart5, pie_chart7, ncol=1, nrow=4, align="v", labels = c('A','B','C','D'), label_size = 12, hjust = -16)



