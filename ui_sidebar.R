sidebar <- shinydashboardPlus::dashboardSidebar(
sidebarMenu(
  
  id = "sidebar_menu",
  
  menuItem(
    "Data Manager",
    tabName = "data_manager",
    icon = icon("database"),
    menuSubItem("File Uploader", tabName = "upload_files"),
    menuSubItem("Storage", tabName = "dataset_storage")
  ),
  
  menuItem(
    "Exploratory Data Analysis",
    tabName = "EDA",
    icon = icon("chart-area")
    
  ),
  
  
  menuItem(
    "Prediction Model",
    tabName = "Prediction",
    icon = icon("dashboard")
    
  )
  
))

