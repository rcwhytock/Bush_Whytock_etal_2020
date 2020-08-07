# Read all the tables into a list
RScripts <- list.files(
  recursive = FALSE,
  full = TRUE,
  pattern = "\\.R$"
)

RScripts <- RScripts[-which(RScripts %in% c("./bodyCondition_loadData_ v.2019.R", "./runAll.R", "./regressionTables.R", "./bodyCondition_annualChange_panelPlot.R",  "./examplePhotos.R","./checkEvents.R"))]

# Run all scripts 
for(k in 1:length(RScripts)){
  
  source(RScripts[k])
  rm(list=setdiff(ls(),c("RScripts", "k"))) 

}

source("./regressionTables.R")
