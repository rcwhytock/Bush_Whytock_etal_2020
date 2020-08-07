observeEvent(input$submitUser, {

  # Create success message
  output$userRegistered <- renderText({
    
    paste("<font color=\"#4f4f4f\"><b>",
          "enregistre",
          "</b></font>")
  })
  
  # Load the data
  analysedData <- read.csv(paste0("./data/sampleResults.csv"), stringsAsFactors = FALSE)
  names(analysedData)[1] <- "uniqueName"
  imageList(imageList()[! imageList() %in% analysedData$uniqueName])
  
  output$numberComplete <- renderText({
    
    paste("Image number:", 200 - length(imageList()))
  })

  # Get initial image to view
  imageToView(sample(length(imageList()), size = 1, replace = F))
  src <- paste0(imageURL, imageList()[imageToView()])
  output$imageOut <- renderText({c('<img src="', src,'" style="max-width:100%;">')})
  
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
      }) } else {
        
        output$imageOut <-
          renderText({
            c('<img src = "', "./sampleImages/complete/complete.jpg", '" style="max-width:100%;">')
          })
        
      }

})