body <- shinydashboard::dashboardBody( useShinyjs(), # show/hide                                       
                                   
  shinydashboard::tabItems(
    
    #-------------Dashboard-----------------------------------------------------
    
    shinydashboard::tabItem(
      
      tabName = "dashboard",
      
      titlePanel("Dashboard"),
      
      br(),
      
      valueBoxOutput("DatasetCounter"),
      
      valueBoxOutput("MLModelsCounter"),
      
    ),

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
              "A. Choose a File:",
              multiple = TRUE,
              accept = c(".csv", ".tsv", ".sav", ".xls",".xlsx", ".json")),
            
            DT::dataTableOutput("file_to_save"),
          
            br(),
          
            shinyWidgets::pickerInput("show_vars",
                        "B. Select Columns to Save:",
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
      titlePanel("Datasets' Storage"),
      
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
                                               "A. Choose One or More Plot Types:",
                                               choices = c("Dimension",
                                                           "Basic Info",
                                                           "Summary Statistics",
                                                           "Descriptive Statistics",
                                                           "Missing Values",
                                                           "Histogram - Continuous",
                                                           "Density",
                                                           "Multivariate Analysis",
                                                           "Barplots - Categorical",
                                                           "Q-Q Plots",
                                                           "Box Plots",
                                                           "Scatter Plots",
                                                           "PCA")),
            
            br(),

            shinyWidgets::pickerInput("select_dataset",
                        "B. Select Dataset from Storage",
                        choices = NULL,
                        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
            
            br(),
            
            shinyWidgets::pickerInput("selected_vars_EDA",
                                      label = "C. Select Variables:",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("selected_vars_EDA_continuous",
                                      label = "C. Select Continuous Variables:",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("selected_vars_EDA_discrete",
                                      label = "C. Select Discrete Variables:",
                                      choices = NULL,
                                      multiple = TRUE),
            
            br(),
            
            shinyWidgets::pickerInput("selected_vars_EDA_grouped_discrete",
                                      label = "D. Select Discrete Variable for grouping:",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("selected_vars_EDA_grouped_continuous",
                                      label = "D. Select Continuous Variable for grouping:",
                                      choices = NULL,
                                      multiple = TRUE),
            
            shinyWidgets::pickerInput("select_corr_calc_type",
                                      label = "D. Select variables' type for correlation:",
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
      titlePanel("Supervised Machine Learning"),
                            
      # Main panel
      mainPanel(
        
        fluidPage(
          
        align = "center",
            
          glide(
              
            id="SupPredictionGLide",
            
            custom_controls = glideControls(

              next_content = list(
                
                nextButton(),
                textOutput("RunningTime", inline = TRUE),
                tags$button(id="ML_Submit_Button",type="button",class="btn action-button btn-primary btn-success last-screen",HTML(paste("Start Learning", shiny::icon("play", lib = "font-awesome")))),
                tags$button(id="ML_Stop_Button",type="button",class="btn action-button btn-primary btn-danger last-screen",HTML(paste("Stop Learning", shiny::icon("stop", lib = "font-awesome")))),
                tags$a(`data-glide-el`="controls",
                    tags$button(id="Back_to_1_Step", type="button", class="btn action-button", `data-glide-dir`="<<", HTML(paste(shiny::icon("rotate-left", lib = "font-awesome"))))
                )

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
                #next_condition = "input.select_train_dataset.length > 0",
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
                        footer=shinyWidgets::awesomeCheckbox("checkbox_regression_choice", label = "I would like get an amount as prediction")),
                    
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
                  h2("Step 3: Dependent & Independent Variables"),
                  br(),
                  br(),
                  h3("Now, we define the model's components..."),
                  br(),
                  br(),
                  shinyWidgets::pickerInput("select_dependent_variable",
                                            label = "C. Select the Dependent Variable that you would like to predict:",
                                            choices = NULL,
                                            multiple = FALSE),
                  shinyWidgets::awesomeCheckbox("zero_or_not_prediction", label = "Allow to predict negative values", value = TRUE),
                  br(),
                  br(),
                  shinyWidgets::pickerInput("select_independent_variables",
                                            label = "D. Please choose the Independent Variables:",
                                            choices = NULL,
                                            multiple = TRUE),
                  next_condition = "input.select_independent_variables.length > 0",
                  next_label = paste("Next: Step 4 ", shiny::icon("chevron-right", lib = "glyphicon"))

              ),
              
          
              screen(
                  
                br(),
                h2("Step 4: Cross Validation & Hypertuning"),
                br(),
                br(),
                h3("It's time to find the balance on features in order to create the most effective Regression ML Model"),
                br(),
                br(),
    
                wellPanel(style = "overflow-y:scroll; max-height: 360px; width:460px", {
                  column(12,
                    fluidRow(
                      column(9,sliderInput("subsample_slider", "E1. Subsample: ", min = 0.05, max = 1, step = 0.05, value = c(0.6, 0.8))),
                      column(3,numericInput("subsample_step_input", "Step", min=0.05, max=1, step = 0.05, value = 0.05))
                    ),
                    br(),
                    fluidRow(
                      column(9,sliderInput("colsample_bytree_slider", "E2. Colsample_bytree:", min = 0.05, max = 1, step = 0.05, value = c(0.6, 0.8))),
                      column(3,numericInput("colsample_bytree_step_input", "Step", min=0.05, max=1, step = 0.05, value = 0.05))
                    ),
                    br(),
                    fluidRow(
                      column(9,sliderInput("max_depth_slider", "E3. Max_depth:", min = 1, max = 50, step = 1, value = c(4, 11))),
                      column(3,numericInput("max_depth_step_input", "Step", min=1, max=50, step = 1 , value = 1))
                    ),
                    br(),
                    fluidRow(
                      column(9,sliderInput("min_child_weight_slider", "E4. Min_child_weight:", min = 1, max = 50, step = 1, value = c(1, 5))),
                      column(3,numericInput("min_child_weight_step_input", "Step", min=1, max=50, step = 1, value = 1))
                    ),
                    br(),
                    fluidRow(
                      column(9,sliderInput("eta_slider", "E5. Eta:", min = 0.01, max = 1, step = 0.01, value = c(0.06, 0.08))),
                      column(3,numericInput("eta_step_input", "Step", min=0.01, max = 1, step = 0.01, value = 0.01))
                    ),
                    br(),
                    fluidRow(
                      column(9,sliderInput("n_rounds_slider", "E6. N_rounds:", min = 1, max = 1000, step = 50, value = c(100, 1000))),
                      column(3,numericInput("n_rounds_step_input", "Step", min=1,  max=50, step = 50, value = 50))
                    ),
                    br(),
                    fluidRow(
                      column(9,sliderInput("n_fold_slider", "E7. Ν_fold:", min = 1, max = 20, step = 1, value = c(5, 10))),
                      column(3,numericInput("n_fold_step_input", "Step", min=1,  max=20, step = 1, value = 1))
                    )
                  ) 
                })
                

              )
            

               
            
            )
        
          
          
        ), width = 12
      
      
        
      )
    
    ),
    
    shinydashboard::tabItem(
      
      tabName = "unsupervised",
      titlePanel("Unsupervised Machine Learning"),
      mainPanel(        
        
        fluidPage(
        
        align = "center",
        
        #glide()
        
        ),width=12)
      
      
    ),
    
    shinydashboard::tabItem(
      
      tabName = "ML_models_storage",
      titlePanel("ML Models' Storage"),
      
      sidebarLayout(
        
        sidebarPanel(
          
          fluidPage(
            
            shinyWidgets::pickerInput("select_modeltype",
                                      "A. Select an available ML model type:",
                                      choices = NULL, 
                                      options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE)),
            
            br(),
            
            shinyWidgets::pickerInput("select_dataset_ML_model",
                                      "B. Select the related trained dataset:",
                                      choices = NULL,
                                      options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE)),
            
            br(),
            
            shinyWidgets::pickerInput("select_variable_to_predict",
                                      "C. Select the available variable you want to predict:",
                                      choices = NULL,
                                      options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE)),
            
            br(),
            
            shinyWidgets::pickerInput("select_test_dataset_to_predict",
                                      "D. Select the available test dataset you want to predict:",
                                      choices = NULL,
                                      options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE)),
            
            #DT::dataTableOutput("Storage_ML_Models"),
            
            br(),
            
            actionButton("Predict", "New Prediction", shiny::icon("add", lib = "font-awesome"))
            
          )
          
          , width = 3
          
        ),
        
        # Main panel
        mainPanel(
          
          fluidPage(

            #uiOutput("ML_results_preview")
            
            fluidRow(
              
              valueBoxOutput("TrainRMSE"),
              
              valueBoxOutput("TestRMSE"),
              
              valueBoxOutput("Partition_perc"),
              
            ),
            
            fluidRow(
              
              box(
                title = "Feature Importance Plot",
                id = "BoxFeatureImportance",
                collapsible = TRUE,
                closable = FALSE,
                plotlyOutput("feature_importance_plot")
              ),
              
              box(
                title = "Feature Importance",
                id = "BoxFeatureImportanceTable",
                collapsible = TRUE,
                closable = FALSE,
                DT::dataTableOutput("feature_importance_table", height = 400)
              )
            
            ),
            
            # fluidRow(
            #   
            #   box(
            #     width = 12,
            #     title = "XGB Tree Plot",
            #     id = "XGBTreePlot",
            #     collapsible = TRUE,
            #     closable = FALSE,
            #     grVizOutput("XGB_Tree_Plot",height = "800px")
            #   )
            #   
            # ),
            
            fluidRow(
              
              box(
                width = 12,
                title = "Prediction Plot",
                id = "BoxPredictionTestedPartition",
                collapsible = TRUE,
                closable = FALSE,
                plotlyOutput("prediction_plot")
              )
            
            ),
            
            fluidRow(
            
              box(
                width = 12,
                title = "Prediction in New Data",
                id = "BoxPredictionNewData",
                collapsible = TRUE,
                closable = TRUE,
                DT::dataTableOutput("New_Prediction")
              )
            
            )
            
          )
          , width = 9
          
        )
        
      )
      
    )
    
  )
  
)

