header <- shinydashboardPlus::dashboardHeader(
  
  title = tags$img(
    src = "https://raw.githubusercontent.com/MariosCh1/Interactive-Web-Application-to-Deploy-Machine-Learning-Models-using-R-Shiny/main/www/logo_white.png",
    height = '40',
    width = '114',
    tags$style(HTML(" .main-header .logo {padding: 0 8px;}"))
  ),
  userOutput("user"),
  controlbarIcon = icon("bars")
  #,fixed = TRUE
  
)
