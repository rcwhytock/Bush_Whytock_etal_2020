library(officer)

# Load the empty word document
wordDoc <- read_docx("../results/regressionTables.docx")

# Read all the tables into a list
regressionTabs <- list.files(
  "../results/",
  recursive = TRUE,
  full = TRUE,
  pattern = "\\.csv$"
)

# Creat empty list
tabList <- vector("list", length = length(regressionTabs))

# Load the tables into the list
for (i in 1:length(tabList)) {
  tabList[[i]] <- read.csv(regressionTabs[i])
  tabList[[i]]$filename <- regressionTabs[i]
  
}

# Add the tables to the word document, round numbers to two digits
for (i in 1:length(tabList)) {
  for (j in 1:length(tabList[[i]])) {
    if (is.numeric(tabList[[i]][, j])) {
      tabList[[i]][, j] <- round(tabList[[i]][, j], digits = 2)
      
    }
    
  }
  
  wordDoc <- body_add_table(wordDoc, tabList[[i]])
  body_add_break(wordDoc, pos = "after") # add each table to new page
  
}

# Save results
print(wordDoc, target = "../results/regressionTables_out.docx")
