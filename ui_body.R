

body <- shinydashboard::dashboardBody( useShinyjs(), # show/hide
      
  shinydashboard::tabItems(

    #-------------UploadFiles---------------------------------------------------
    
    shinydashboard::tabItem(
      
      tabName = "upload_files",
      titlePanel("File Uploader"),
      
      # Sidebar Datasets
      sidebarLayout(
        
        sidebarPanel(
          
          fluidPage(
          
            fileInput(
              
              "uploaded_file",
              "A. Choose a File",
              multiple = TRUE,
              accept = c(".csv", ".tsv", ".sav", ".xls",".xlsx")),
            
            DT::dataTableOutput("file_to_save"),
          
            br(),
          
            shinyWidgets::pickerInput("show_vars",
                        "B. Select Columns to Save",
                        choices = NULL,
                        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE), 
                        multiple = TRUE),
          
            br(),
            
            #useShinyalert(),
            
            actionButton("Save_to_DB", "Save Columns to DB")
            
            )
          
        ),
        
        # Main panel
        mainPanel(
          
          fluidPage(

            uiOutput("dataset_tabs_upload")
            
          )
          
        )
        
      )
      
    ),
    
    #------------Storage--------------------------------------------------------
    
    shinydashboard::tabItem(
      
     
      tabName = "dataset_storage",
      titlePanel("Storage"),
      
      # Sidebar Datasets
      sidebarLayout(
        
        sidebarPanel(
          
          fluidPage(

            DT::dataTableOutput("Storage_DB"),
            
            br(),
            
            #useShinyalert(),
            
            actionButton("dataset_delete", "Delete")
          
          )
          
        ),
        
        # Main panel
        mainPanel(
          
          fluidPage(

            uiOutput("dataset_preview")
            
          )
          
        )
        
      )
      
    ),
    
    #------EDA------------------------------------------------------------------
    
    shinydashboard::tabItem(

      tabName = "EDA",
      titlePanel("Exploratory Data Analysis"),

      # Sidebar Datasets
      sidebarLayout(

        sidebarPanel(

          fluidPage(

            
            shinyWidgets::checkboxGroupButtons("plot_types",
                                               "A. Choose One or More Plot Types",
                                               choices = c("Dimension",
                                                           "Basic Info",
                                                           "Missing Values",
                                                           "Histogram - Continuous",
                                                           "Density",
                                                           "Multivariate Analysis",
                                                           "Barplots - Categorical",
                                                           "Q-Q Plot",
                                                           "Box Plots",
                                                           "Scatter Plots",
                                                           "Principal Component Analysis")),
            
            br(),

            shinyWidgets::pickerInput("select_dataset",
                        "B. Select Dataset from Storage",
                        choices = NULL,
                        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
            
            br(),
            
            shinyWidgets::pickerInput("selected_vars_EDA",
                                      label = "C. Select Variables from Dataset for EDA",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("selected_vars_EDA_continuous",
                                      label = "C. Select Continuous Variables from Dataset for EDA",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("selected_vars_EDA_discrete",
                                      label = "C. Select Discrete Variables from Dataset for EDA",
                                      choices = NULL,
                                      multiple = TRUE),
            
            br(),
            
            shinyWidgets::pickerInput("selected_vars_EDA_grouped_discrete",
                                      label = "D. Select Discrete Variables from Dataset for grouping",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("selected_vars_EDA_grouped_continuous",
                                      label = "D. Select Continuous Variables from Dataset for grouping",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("select_corr_calc_type",
                                      label = "D. Select variables' type for correlation",
                                      choices = c("all", "discrete", "continuous"))

          )

        ),

        mainPanel(

          fluidPage(
            #<div class="user-select-none svg-container" style="position: relative; width: 788px; height: 400px;">
            
            
            htmlOutput("plot_tabs")
            
            

          )

        )

      )

    )
    
  )
  
)

