tabItem(tabName = "scoreImageTab",
          fluidPage(column(
            width = 7,
            box(status = "warning",
                width = 12,
          htmlOutput("imageOut"),
          br(),
          selectInput(
            inputId = "imageOK",
            label = "qualite",
            choices = c("bon",
                        "modere",
                        "mauvais",
                        "jeter"),
            selectize = FALSE,
            width = "100%"
          ),
          uiOutput("decisionButton"),
          div(id = "removeButton", 
              actionButton(
            inputId = "submitOKbutton",
            label = "soumettre",
            width = "100%",
            style = "color: #fff; background-color: #4f4f4f; border-color: #141414"
          ))
        )),
        column(
          width = 5,
          
          uiOutput("sliderBox")
            
            
          
        )))