## Long-term collapse in fruit availability threatens Central African forest megafauna #### 


This repository contains all of the code used in Bush and Whytock et al. 2020 [citation TBC].
Data used in the analyses are available in .csv format from the University of Stirling's data repository (DataSTORRE: Stirling Online Repository for Research Data) at this link http://hdl.handle.net/11667/159

### The directory structure is:

```
fruitPhenology # Analysis of tree reproduction and fruit availability
  |-figures # Figures output from the R scripts
  |-R # R scripts
  
bodyCondition # Analysis of trends in forest elephant body condition over time
  |-figures # Figures output from the R scripts
  |-R # R scripts
  
testScoring # Comparison between scorers using a standardised database of 200 photos
  |-data # Data in .csv format
  |-figures # Figures output from the R scripts
  |-R # R scripts
  
 shinyApp # The R Shiny App used to score elephant body condition. Minimum working version.
  |-data # Data in .csv format
  |-pages # Server and ui R scripts
    |-server
    |-ui
  |-www # Contains images to run the app locally
    |-refImages # Reference photos. b,d,f,h,j are from https://doi.org/10.1371/journal.pone.0093802
    |-sampleImages # 10 example photos
      |-complete # Contains image to show the user has scored all example photos
```
