sidebar <- shinydashboardPlus::dashboardSidebar(
  
  shinyjs::useShinyjs(),
  minified = TRUE, 
  collapsed = TRUE,
  
  sidebarMenu(
    
    id = "sidebar_menu",
    
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = shiny::icon("house")
      
    ),
    
    menuItem(
      "Data Manager",
      tabName = "data_manager",
      icon = shiny::icon("database"),
      menuSubItem("File Uploader", tabName = "upload_files", icon = shiny::icon("angles-right")),
      menuSubItem("Datasets' Storage", tabName = "dataset_storage", icon = shiny::icon("angles-right"))
    ),
    
    menuItem(
      "Exploratory Data Analysis",
      tabName = "EDA",
      icon = shiny::icon("chart-area")
      
    ),
    
    menuItem(
      "ML Prediction Modeling",
      tabName = "Prediction",
      icon = shiny::icon("gauge-high"),
      menuSubItem("Supervised ML", tabName = "supervised", icon = shiny::icon("angles-right")),
      #menuSubItem("Unsupervised ML", tabName = "unsupervised", icon = shiny::icon("angles-right")),
      menuSubItem("ML Models' Storage", tabName = "ML_models_storage", icon = shiny::icon("angles-right"))
    )
    
  )

)
