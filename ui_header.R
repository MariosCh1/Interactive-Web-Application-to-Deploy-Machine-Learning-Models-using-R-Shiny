header <- shinydashboardPlus::dashboardHeader(title = tags$img(src="https://github.com/MariosCh1/Interactive-Web-Application-to-Deploy-Machine-Learning-Models-using-R-Shiny/blob/main/www/logo.png?raw=true", 
                                                               height = '40', width='114',
                                                               tags$style(
                                                                 HTML(
                                                                   " .main-header .logo {padding: 0 7px;}"
                                                                 )
                                                               )), userOutput("user"))
