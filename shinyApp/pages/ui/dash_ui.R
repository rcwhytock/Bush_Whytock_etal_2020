#### Header ####
header <- dashboardHeader(title = "ANPN : ecoHealth",
                          titleWidth = 300)
#### Sidebar ####
sidebar <- dashboardSidebar(width = 200, sidebarMenu(
  
  menuItem("User info",
           tabName = "userTab",
           icon = icon("users")),
  menuItem("Marquer",
           tabName = "scoreImageTab",
           icon = icon("calculator"))

))

#### Body #####
body <-   dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  tabItems(
    source("./pages/ui/username_ui.R", local = TRUE)$value,
    source("./pages/ui/imageViewer_ui.R", local = TRUE)$value
    )
 
)

#### Create dashboard ####
dashboardPage(header,
              sidebar,
              body,
              skin = "yellow")
