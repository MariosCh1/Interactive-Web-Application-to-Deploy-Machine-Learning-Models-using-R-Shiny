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
            
            actionButton("Save_to_DB", "Save")
            
            )
            , width = 3
          
        ),
        
        # Main panel
        mainPanel(
          
          #fluidPage(

            uiOutput("dataset_tabs_upload")
            
          #)
          , width = 9
          
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
          
          , width = 3
          
        ),
        
        # Main panel
        mainPanel(
          
          fluidPage(

            uiOutput("dataset_preview")
            
          )
          , width = 9
          
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
        , width = 3

        ),

        mainPanel(
          
            htmlOutput("plot_tabs")

          , width = 9

        )

      )

    ),
    
    #---------Prediction Model--------------------------------------------------
    
    shinydashboard::tabItem(
      
      tabName = "supervised",
      titlePanel("Supervised Learning"),
                            
      # Main panel
      mainPanel(
        
        fluidPage(
          
        align = "center",
            
          glide(
              
            id="SupPredictionGLide",
            
            custom_controls = glideControls(

              next_content = list(
                nextButton(),
                tags$button(id="ML_Submit_Button",type="button",class="btn action-button btn-primary btn-success last-screen",HTML(paste("Start Learning", shiny::icon("play", lib = "glyphicon"))))

              )
              
            ),
              
            #height = "560px",
              
              screen(
                  
                br(),
                h2("Step 1: Dataset & Partitions"),
                br(),
                br(),
                h3("Let's Start!"),
                br(),
                br(),
                shinyWidgets::pickerInput("select_train_dataset",
                                          label = "A. Please choose the train dataset from storage list:",
                                          choices = NULL,
                                          multiple = FALSE),
                br(),
                br(),
                sliderInput("select_data_partition", label = "B. Please select the percentage of train dataset partition:", min = 70, max = 100, value = 80, post = "%"),
                next_condition = "input.select_train_dataset.length > 0",
                next_label = paste("Next: Step 2 ", shiny::icon("chevron-right", lib = "glyphicon"))
                  
                  
                ), 
            
              screen(
                
                br(),
                h2("Step 2: Prediction Type - Regression or Classification"),
                br(),
                br(),
                h3("One step closer, please choose the type of prediction..."),
                br(),
                br(),
                fluidPage(
                  fluidRow(
                    box(title="Regression", 
                        img(src = "https://raw.githubusercontent.com/MariosCh1/Interactive-Web-Application-to-Deploy-Machine-Learning-Models-using-R-Shiny/main/www/regression_example.png",
                            width=300,
                            height=192),
                        footer=shinyWidgets::awesomeCheckbox("checkbox_regression_choice", label = "I would like get a amount as prediction")),
                    
                    box(title="Classification",
                        img(src = "https://raw.githubusercontent.com/MariosCh1/Interactive-Web-Application-to-Deploy-Machine-Learning-Models-using-R-Shiny/main/www/classification_example.png",
                            width=300,
                            height=192),
                        footer=shinyWidgets::awesomeCheckbox("checkbox_classification_choice", label = "I would like get a class as prediction"))
                    
                  )
                ),
                next_condition = "input.checkbox_classification_choice | input.checkbox_regression_choice",
                next_label = paste("Next: Step 3 ", shiny::icon("chevron-right", lib = "glyphicon"))
                
                
              ),
              
              screen(
                  
                  br(),
                  h2("Step 3: Independed & Depended Variables"),
                  br(),
                  br(),
                  h3("Now, we define the model's components..."),
                  br(),
                  br(),
                  shinyWidgets::pickerInput("select_dependent_variable",
                                            label = "C. Select the Depended Variable that you would like to predict:",
                                            choices = NULL,
                                            multiple = FALSE),
                  br(),
                  br(),
                  shinyWidgets::pickerInput("select_independent_variables",
                                            label = "D. Please choose the Independed Variables:",
                                            choices = NULL,
                                            multiple = TRUE),
                  next_condition = "input.select_independent_variables.length > 0",
                  next_label = paste("Next: Step 4 ", shiny::icon("chevron-right", lib = "glyphicon"))

              ),
              
            
              screen(
                  
                br(),
                h2("Step 4: Cross Validation & Hypertuning"),
                br(),
                br()


              )
            

               
            
            )
        
          
          
        ), width = 12
      
      
        
      )
    
    ),
    
    shinydashboard::tabItem(
      
      tabName = "unsupervised",
      titlePanel("Unsupervised Learning"),
      mainPanel(        
        
        fluidPage(
        
        align = "center",
        
        #glide()
        
        ),width=12)
      
      
    )
    
  )
  
)

