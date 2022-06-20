
rm(list = ls())  #clear the workspace

library(gets)
library(dplyr)
library(tidyr)
library(ggplot2)

source("isatpanel_v9.R")
source("breakmap.R")

set.seed(123)


ltitle=paste0("Results_CO2Drivers_Systematic Analysis 1",
              format(Sys.time(), "%m-%d"),".txt")

data <- read.csv("CO2DriversEU_dataset_v1.csv")
data$lgdp_sq <- data$lgdp^2

EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")
EU28 <- c(EU15, "Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia", 
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", 
          "Slovak Republic", "Slovenia")
EU31 <- c(EU28, "Switzerland", "Iceland", "Norway")


###### Analysis:

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 CO2 DRIVERS EU - ANALYSIS 1                      # \n",
    "#                                                                  # \n",
    "# Started: ", format(Sys.time(), "%d.%m.%y %H-%M"), 
    "                                          # \n",
    "#################################################################### \n",
    "\n \n \n \n \n"),
  file = ltitle
)

# Specify parameters:
for(syear in c(1970, 1980, 1995)){
  for(sample in list(EU15, EU28, EU31)){
    runit <- "Austria"
    
    dat <- filter(data, country %in% sample, year>=syear)
    
    for(ar.lag in c(T, F)){
      
      for(p.value in c(0.05, 0.01, 0.005)){
        
        # Specify control variables:
        controls <- cbind(dat$lgdp, dat$lgdp_sq, dat$lpop)
        if(ar.lag){controls <- cbind(controls, dat$L1.ltransport.emissions)}
        
        # Break analysis:
        is <- isatpanel(
          y=dat$ltransport.emissions, 
          id=dat$country, 
          time=dat$year, 
          mxreg=controls, 
          mxbreak=c(dat$const), 
          break.method="both", 
          effect="twoways", 
          iis=TRUE, 
          t.pval=p.value)
        
        # Create and output graph:
        map <- breakmap(
          model = is,
          start.time = syear,
          reference.unit = runit,
          plot.iis = T
        )
        
        mtitle <- paste0("Analysis1/from", syear, 
                         "/EU", length(sample),
                         "_", ifelse(ar.lag, "with", "without"), "_ar-lag",
                         "_p", p.value, ".png")
        
      ggsave(filename=mtitle, plot =  map, width=30, height=15, units="cm")
        
        # Output analysis results:
        cat(
          paste0(
            " \n ############################### \n # Sample: EU", 
            length(sample),
            " \n # Startyear: ", syear,
            " \n # Autogressive lag: ", ar.lag,
            " \n # p-value: ", p.value,
            " \n # graph location: ", mtitle,
            " \n \n "), 
          file = ltitle, 
          append = T)
        
        sink(ltitle, append=T)
        print(is)
        sink()
        
        cat(" \n \n \n \n \n", 
            file = ltitle, 
            append = T)
        
      }
    }
  }
}


