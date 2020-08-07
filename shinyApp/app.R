source("./global.R")

# Define UI
ui <- div(id = "mainPage",
          
          useShinyjs(),
          source("./pages/ui/dash_ui.R",
                 local = TRUE)$value
          
          )

# Define server logic
server <- function(input, output) {

  # Load the list of all images
  images <- list.files("./www/sampleImages", pattern = "\\.jpg$")
  imageList <- reactiveVal(images)
  
  # Initial image to view
  imageToView <- reactiveVal() 
  
  # URL for images (local in example app)
  imageURL <- "./sampleImages/"
  
  source("./pages/server/username_server.R",
         local = TRUE)$value
  source("./pages/server/imageViewer_server.R",
         local = TRUE)$value
}

# Run the application
shinyApp(ui = ui, server = server)
