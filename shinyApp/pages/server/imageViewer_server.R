observeEvent(input$submitOKbutton, {
    
  if (! input$imageOK == "jeter") {
    
    removeUI(selector = "#removeButton", immediate = TRUE)
    
    # Source the reference image viewer
    source("./pages/server/scoreSlider_server.R",
                   local = TRUE)$value
    
  } else {
    removeUI(selector = "#removeButton", immediate = TRUE)
    
    # Add result to the csv
    res <-
      c(imageList()[imageToView()],
        input$userID,
        input$imageOK,
        "NA",
        "NA",
        "NA",
        "NA",
        "NA",
        "NA",
        as.character(ymd_hms(Sys.time())),
        0)
    
    # read old data, merge and write
    analysedData <- read.csv(paste0("./data/sampleResults.csv"), stringsAsFactors = FALSE)
    analysedData <- rbind(res, analysedData)
    write.csv(analysedData, paste0("./data/sampleResults.csv"), row.names = F)
    
    # Reset 'quality' button
    reset("imageOK")
    
    # Take out images from imageList that have already been seen by user
    imageList(imageList()[! imageList() %in% analysedData$uniqueName])
    output$numberComplete <- renderText({
      
      paste("Image number:", 200 - length(imageList()))
    })
    
    
    # Choose a new random image to score
    if(length(imageList()) > 0){
    imageToView(sample(length(imageList()), size = 1, replace = F))
   
    # Source the image
    src <-
      paste0(imageURL,
             imageList()[imageToView()])
    output$imageOut <-
      renderText({
        c('<img src = "', src, '" style="max-width:100%;">')
      }) }
  else {
        
        output$imageOut <-
          renderText({
            c('<img src = "', "./sampleImages/complete/complete.jpg", '" style="max-width:100%;">')
          })
        
      }
    
    # Add the submit button back in
    output$decisionButton <- renderUI({
      
      div(id = "removeButton", 
        actionButton(
          inputId = "submitOKbutton",
          label = "soumettre",
          width = "100%",
          style = "color: #fff; background-color: #4f4f4f; border-color: #141414"
        ))
      })
    
  }
  
  
})

observeEvent(input$scoreButton, {
  
  # Remove bodyscore slider
  removeUI(selector = "#scoreSlider", immediate = TRUE)

  # Add the submit button back in
  output$decisionButton <- renderUI({
    
    div(id = "removeButton", 
        actionButton(
          inputId = "submitOKbutton",
          label = "soumettre",
          width = "100%",
          style = "color: #fff; background-color: #4f4f4f; border-color: #141414"
        ))
  })
  
  # Add result to csv
  res <-
    c(
      imageList()[imageToView()],
      input$userID,
      input$imageOK,
      input$angle,
      input$sliderScore,
      input$sex,
      input$age,
      input$sick,
      as.character(ymd_hms(Sys.time())),
      input$nEles
    )
  
  analysedData <- read.csv(paste0("./data/sampleResults.csv"), stringsAsFactors = FALSE)
  analysedData <- rbind(res, analysedData)
  write.csv(analysedData, paste0("./data/sampleResults.csv"), row.names = F)
  
  reset("imageOK")
  
  # New image
  imageList(imageList()[! imageList() %in% analysedData$uniqueName])
  output$numberComplete <- renderText({
    
    paste("Image number:", 200 - length(imageList()))
  })
  
  
  if(length(imageList()) > 0){
    imageToView(sample(length(imageList()), size = 1, replace = F))
    
    # Source the image
    src <-
      paste0(imageURL,
             imageList()[imageToView()])
    output$imageOut <-
      renderText({
        c('<img src = "', src, '" style="max-width:100%;">')
      }) } else {
        
        output$imageOut <-
          renderText({
            c('<img src = "', "./sampleImages/complete/complete.jpg", '" style="max-width:100%;">')
          })
        
      }
  
}, autoDestroy = TRUE)

# Undo
observeEvent(input$undoButton, {
  
  # Add the submit button back in
  output$decisionButton <- renderUI({
    
    div(id = "removeButton", 
        actionButton(
          inputId = "submitOKbutton",
          label = "soumettre",
          width = "100%",
          style = "color: #fff; background-color: #4f4f4f; border-color: #141414"
        ))
  })
  
  # Remove bodyscore slider
  removeUI(selector = "#scoreSlider")
  
  reset("imageOK")
  
  
}, autoDestroy = TRUE)