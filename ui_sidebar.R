sidebar <- shinydashboardPlus::dashboardSidebar(
  
  shinyjs::useShinyjs(),
  minified = TRUE, 
  collapsed = TRUE,
  
  sidebarMenu(
    
    id = "sidebar_menu",
    
    menuItem(
      "Data Manager",
      tabName = "data_manager",
      icon = icon("database"),
      startExpanded = TRUE,
      menuSubItem("File Uploader", tabName = "upload_files", selected = TRUE),
      menuSubItem("Datasets' Storage", tabName = "dataset_storage")
    ),
    
    menuItem(
      "Exploratory Data Analysis",
      tabName = "EDA",
      icon = icon("chart-area")
      
    ),
    
    menuItem(
      "ML Prediction Modeling",
      tabName = "Prediction",
      icon = icon("tachometer-alt"),
      menuSubItem("Supervised Learning", tabName = "supervised"),
      menuSubItem("Unsupervised Learning", tabName = "unsupervised"),
      menuSubItem("ML Models' Storage", tabName = "ML_models_storage")
    )
    
  )

)

