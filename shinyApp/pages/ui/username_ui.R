tabItem(tabName = "userTab",
        column(width = 6,
          box(status = "warning",
              title = "Nom d'utilisateur",
              width = 12,
              selectizeInput(inputId = "userID",
                           label = "selectionnez utilisateur",
                           choices = userList,
                           width = "100%"),
              
              actionButton(inputId = "submitUser",
                             label = "soumettre",
                             width = "100%",
                           style = "color: #fff; background-color: #4f4f4f; border-color: #141414"),
              br(),
              htmlOutput("userRegistered")
              )))