header <- shinydashboardPlus::dashboardHeader(title = tags$img(src="https://github.com/MariosCh1/Interactive-Web-Application-to-Deploy-Machine-Learning-Models-using-R-Shiny/blob/a8f98150e93fb256786aa4dd9b527a9f11dba9b4/www/logo.png?raw=true", 
                                                               height = '40', width='101',
                                                               tags$style(
                                                                 HTML(
                                                                   " .main-header .logo {padding: 0 8px;}"
                                                                 )
                                                               )), userOutput("user"))
