
rm(list = ls())  #clear the workspace

library(gets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(data.table)
library(getspanel)

set.seed(123)


ltitle=paste0("Results_CO2Drivers_Analysis 1d_",
              format(Sys.time(), "%m-%d"),".txt")

data <- read.csv("CO2DriversEU_dataset_v1_final.csv")
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
  
    for(p.value in c(0.05, 0.01, 0.001)){
      # Break analysis:
      is <- isatpanel(dat,
        formula = "ltransport.emissions ~ lgdp + lgdp_sq + lpop", 
        index = c("country", "year"),
        effect="twoways", 
      # I think the issue is the choice of fesis, iis, sis! Once this is right the results should be identical...
      # For the EU-31 test case could also be the non-differentiation between EU_15 and EU_31 countries...
        fesis = TRUE,
        t.pval= p.value)
      
      
      # Create and output graph:
      graph <- plot_grid(is) +
          ggtitle(paste0("EU",length(sample), "log_emissions at p=",p.value))  + 
         theme(legend.position = "none")
      
      ggsave(
           paste0("EU", length(sample), "_", "_p", 
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
          " \n ############################### \n ", 
          "Log Emissions",
          " \n # Sample: EU", length(sample),
          " \n # Startyear: ", syear,
          " \n # Autogressive lag: FALSE ",
          " \n # p-value: ", p.value,
          " \n \n "), 
        file = ltitle, 
        append = T)
      
      sink(ltitle, append=T)
      print(is)
      sink()
      
      cat(
        paste0(
          " \n \n \n EU", length(sample), ": Plot ", plot.count-1, "/8 finished. \n \n \n "
        )
      )
    }
  }
