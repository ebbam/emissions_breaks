
rm(list = ls())  #clear the workspace

library(gets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(data.table)

source("isatpanel_v9.R")
source("breakmap_v2.R")

set.seed(123)


ltitle=paste0("Results_CO2Drivers_Analysis 1d_",
              format(Sys.time(), "%m-%d"),".txt")

data <- read.csv("CO2DriversEU_dataset_v1.csv")
data$lgdp_sq <- data$lgdp^2

data <- as.data.table(data)

data$transport.emissions_pc <- data$transport.emissions/data$pop
data$ltransport.emissions_pc <- log(data$transport.emissions_pc)
data[, L1.ltransport.emissions_pc:=c(NA, ltransport.emissions_pc[-.N]), by="country"]

data$growthtransport.emissions_pc <- log(data$transport.emissions_pc/exp(data$L1.ltransport.emissions_pc))
data[, L1.growthtransport.emissions_pc:=c(NA, growthtransport.emissions_pc[-.N]), by="country"]


EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")
EU31 <- c(EU15, "Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia", 
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", 
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland", 
          "Norway")


###### Analysis:

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 CO2 DRIVERS EU - ANALYSIS 1d                     # \n",
    "#                                                                  # \n",
    "# Started: ", format(Sys.time(), "%d.%m.%y %H-%M"), 
    "                                          # \n",
    "#################################################################### \n",
    "\n \n \n \n \n"),
  file = ltitle
)

# Specify parameters:
syear <- 1995
plot.count <- 1
runit <- "Austria"


for(sample in list(EU15, EU31)){
  dat <- filter(data, country %in% sample, year>=syear)
  
  maps <- list()
  
  cat(
    paste0(
      "############################## \n",
      "#  SAMPLE = EU", length(sample), " \n",
      "############################## \n",
      "\n \n \n \n \n"),
    file = ltitle,
    append = T
  )

  for(dv in list(dat$ltransport.emissions, dat$ltransport.emissions_pc, dat$growthtransport.emissions_pc)){
    
    for(p.value in c(0.1, 0.05, 0.01, 0.005)){
      
      # Specify control variables:
      controls <- cbind(dat$lgdp, dat$lgdp_sq)
      
      
      if(plot.count %in% c(1:4, 13:16)){
        dv.name <- "log(emissions)"
        controls <- cbind(controls, dat$lpop)
        }
      if(plot.count %in% c(5:8, 17:20)){dv.name <- "log(emissions p.c.)"}
      if(plot.count %in% c(9:12, 21:24)){dv.name <- "growth(emissions p.c.)"}
      
      # Break analysis:
      is <- isatpanel(
        y=dv, 
        id=dat$country, 
        time=dat$year, 
        mxreg=controls, 
        mxbreak=c(dat$const), 
        break.method="both", 
        effect="twoways", 
        iis=TRUE, 
        t.pval=p.value)

      
      # Create and output graph:
      graph <- breakmap(
        model = is,
        start.time = syear,
        reference.unit = runit,
        plot.iis = T
        ) +
        ggtitle(paste0(dv.name, " at p=",p.value))  + 
        theme(legend.position = "none")
      
      ggsave(
        paste0("Analysis1d/EU", length(sample), "_", dv.name, "_p", 
               p.value, ".png"),
        graph,
        width=25,
        height=length(sample),
        units="cm"
      )

      maps[[plot.count]] <- ggplot_build(graph)
      plot.count <- plot.count+1
      
      # Output analysis results:
      cat(
        paste0(
          " \n ############################### \n #### DV = ", 
          dv.name,
          " \n # Startyear: ", syear,
          " \n # Autogressive lag: FALSE ",
          " \n # p-value: ", p.value,
          " \n \n "), 
        file = ltitle, 
        append = T)
      
      sink(ltitle, append=T)
      print(is)
      sink()
      
      cat(" \n \n \n \n \n", 
          file = ltitle, 
          append = T)
      
      cat(
        paste0(
          " \n \n \n EU", length(sample), ": Plot ", plot.count-1, "/24 finished. \n \n \n "
        )
      )
    }
  }
}
