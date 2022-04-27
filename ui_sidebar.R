sidebar <- shinydashboardPlus::dashboardSidebar(
sidebarMenu(
  
  id = "sidebar_menu",
  
  menuItem(
    "Data Manager",
    tabName = "data_manager",
    icon = icon("database"),
    menuSubItem("File Uploader", tabName = "upload_files"),
    menuSubItem("Storage/Data Transformation", tabName = "dataset_storage")
  ),
  
  menuItem(
    "Exploratory Data Analysis",
    tabName = "EDA",
    icon = icon("chart-area")
    
  )
  
))

