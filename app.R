#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

set.seed(100)
options(shiny.port = 8080 #, shiny.maxRequestSize = 40 * 1024 ^ 2
        ,scipen = 999) #avoid num scientific format



#----------AUTH0----------------------------------------------------------------

#auth0::use_auth0()

#usethis::edit_r_environ("project")

a0_info <- auth0::auth0_info()


#-----------AUTH0: Retrieve Users with the Get Users Endpoint-------------------

#reticulate::install_miniconda() #run one time on setup
#reticulate::py_install("pandas") #run one time on setup
reticulate::source_python("ask_auth0_for_tokens_api.py")
reticulate::source_python("send_auth0_token_to_api.py")

#----------PACKAGES & LIBRARIES-------------------------------------------------

source("packages.R")

#----------DATABASE-------------------------------------------------------------

source("db_queries.R")

#----------FUNCTIONS------------------------------------------------------------

source("functions.R")

## Only run examples in interactive R sessions
if (interactive()) {
  
  shinydashboardPlusGallery()
  
  #----------UI-----------------------------------------------------------------
  source("ui_header.R")
  source("ui_body.R")
  source("ui_sidebar.R")
  source("ui_controbar.R")
  
  ui <- dashboardPage(header, sidebar, body, controlbar)
  
  a0_ui <- auth0_ui(ui , info = a0_info)
  
  
  #----------SERVER-------------------------------------------------------------
  
  server <-  function(input, output, session) {
    
    #---------SET GLOBAL local_infile = true;-----------------------------------
    observe({
    set_global_local_infile()
    })
    #---------------------------------------------------------------------------

    values <- reactiveValues()
    #values$stored_files <- NULL
    
    #---------User Auth---------------------------------------------------------
    
    
    dUser <- shinydashboardPlus::dashboardUser(
      name = session$userData$auth0_info$name,
      image = session$userData$auth0_info$picture,
      title = session$userData$auth0_info$nickname,
      footer = p(logoutButton(), class = "text-center"),
      tags$style(
        HTML(
          ".navbar-nav>.user-menu>.dropdown-menu>li.user-header>img {
          height: 80px;
          width: 80px;
          }
          
          .navbar-nav>.user-menu>.dropdown-menu>li.user-header>p {
          font-size: 12px;
          }"
        )
      )
    )
    
    
    output$user <- renderUser(
      if (getCountofUsers(session$userData$auth0_info$sub) == 0) {   #if (!(session$userData$auth0_info$sub %in% send_api$user_id)) {
        
        saveUsertoDB(session$userData$auth0_info)
        dUser
        
      } else{

        dUser
        
      }
    )
    
    
    #--------Dashboard----------------------------------------------------------

    output$DatasetCounter <- renderValueBox({
      valueBox(
        getCountofDatasets(session$userData$auth0_info$sub), "Datasets", shiny::icon("database"),
        color = "yellow"
      )
    })
    
    output$MLModelsCounter <- renderValueBox({
      valueBox(
        getCountofModelsperUser(session$userData$auth0_info$sub), "ML Models", shiny::icon("gauge-high"),
        color = "purple"
      )
    })
    
    #--------Upload Manager-----------------------------------------------------
    dataset <- function(x) {
      req(input$uploaded_file)
      file <- input$uploaded_file
      ext <- tools::file_ext(file[x,]$datapath)
      
      dataset <- switch(
        ext,
        csv = vroom::vroom(file[x,]$datapath, delim = ","),
        sav = read.spss(
          file[x,]$datapath,
          use.value.label = TRUE,
          to.data.frame = TRUE
        ),
        tsv = vroom::vroom(file[x,]$datapath, delim = "\t"),
        xlsx = read_excel(file[x,]$datapath),
        xls = read_excel(file[x,]$datapath),
        json = fromJSON(file[x,]$datapath) %>% as.data.frame(),
        validate(
          "Invalid file; Please upload a .csv, .sav, .tsv, .xls, .xlsx or .json file"
        )
      )
      
    }
    
    
    output$file_to_save <- DT::renderDataTable({
      file <- input$uploaded_file
      
      if (is.null(file))
        return(NULL)
      else
        return(file)
      
    }, options = list(
      scrollX = TRUE,
      pageLength = 3,
      dom = 't'
      
    ))
    
    
    
    
    #Δημιουργία Tabs για διαφορετικά Dataset Files για upload
    output$dataset_tabs_upload <- renderUI({
      req(input$uploaded_file)
      file <- input$uploaded_file
      
      create_tabs <- function(x) {
        tabPanel(file[x, ]$name,
                 DT::renderDataTable({
                   dataset(x)[, input$show_vars, drop = FALSE]
                 },
                 options = list(scrollX = TRUE,
                                pageLength = 12)))
        
        
      }
      
      myTabs <- lapply(1:length(file$name), create_tabs)
      
      do.call(tabsetPanel, c(id = "dataset_tabsetPanel", myTabs))
      
    })
    
    
    observe({
      req(input$uploaded_file)
      req(input$dataset_tabsetPanel)
      
      ds_names <-
        names(dataset(
          match(input$dataset_tabsetPanel, input$uploaded_file$name)
        ))
      
      shinyWidgets::updatePickerInput(
        session,
        "show_vars",
        label = "B. Select Columns to Save",
        choices = ds_names,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10
        )
      )
      
      
    })
    
    
    
    #--------Storage------------------------------------------------------------
    
    observeEvent(input$Save_to_DB, {
      #req(dataset_selected_vars)
      #req(input$sal_input_name_file)
      
      values$dataset_selected_vars <-
        dataset(match(input$dataset_tabsetPanel, input$uploaded_file$name))[input$show_vars]
      
      
      shinyalert(
        title = "Input the file name:",
        type = "input",
        inputId = "sal_input_name_file",
        showCancelButton = TRUE,
        confirmButtonText = 'Yes, Save it!'
      )
      
      
      
    })
    
    
    observe({
      req(values$dataset_selected_vars)
      req(input$sal_input_name_file)
      
      print(values$dataset_selected_vars)
      
      write.csv(
        values$dataset_selected_vars,
        paste0(
          "C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\" ,
          input$sal_input_name_file,
          ".csv"
        ),
        row.names = FALSE
      )
      
      
      if (fileNameExistsinDB(session$userData$auth0_info$sub,
                             input$sal_input_name_file)) {
        print("Name Doesn't Exist")
        uploadedFiletoDB(session$userData$auth0_info$sub,
                         input$sal_input_name_file)
        shinyalert(title = "File Saved to Database!",
                   type = "success",
                   timer = 3000)
        
      } else{
        print("Name Exists")
        shinyalert(
          title = "Name Already Exists!",
          text = "Please try again with a different name",
          type = "error",
          timer = 3000
        )
        shinyalert(
          title = "Input the file name:",
          type = "input",
          inputId = "sal_input_name_file",
          showCancelButton = TRUE,
          confirmButtonText = 'Yes, Save it!'
        )
        
      }
      
      
      file.remove(
        paste0(
          "C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\" ,
          input$sal_input_name_file,
          ".csv"
        )
      )
      
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    reactiveDBContent <-
      eventReactive(input$sidebar_menu == "dataset_storage" |
                      input$sidebar_menu == "EDA" |
                      input$sidebar_menu == "supervised" |
                      values$shinyAlert_respones == TRUE,
                    {
                      shinyjs::disable("dataset_delete")
                      
                      values$stored_files <-
                        getSavedFiles(session$userData$auth0_info$sub)
                      
                      if (is.null(values$stored_files))
                        return(NULL)
                      else
                        return(values$stored_files)
                      
                    })
    
    output$Storage_DB <- 
      DT::renderDataTable(
        reactiveDBContent(),
        selection = 'single' ,
        options = list(
          #scrollX = TRUE,
          pageLength = 12,
          scrollY = "200px",
          paging = FALSE,
          dom = 't'
          
        )
      )
    

    observeEvent(input$Storage_DB_cell_clicked, {
      if (is.null(input$Storage_DB_rows_selected)) {

        shinyjs::disable("dataset_delete")
        
      } else {
        shinyjs::enable("dataset_delete")
        
        values$selected_file_from_DB <-
          getSelectedBlobFile(session$userData$auth0_info$sub,
                              input$Storage_DB_cell_clicked$value)
        
      }
      
    })

    

    
  
      observeEvent(input$dataset_delete, {
        
        
        if (is.null(input$Storage_DB_rows_selected)==FALSE) {

          
        shinyalert(
          title = "Are you sure you want to delete this file?",
          callbackR = function(x){

            
            if(x==TRUE){
              
              deleteFilefromDB(session$userData$auth0_info$sub, input$Storage_DB_cell_clicked$value)
              
              shinyalert(
                title = "The selected file deleted from Database",
                type = "success",
                inputId = "Storage_DB_cell_clicked$value",
                confirmButtonText = 'Ok'
              ) 
              
              values$shinyAlert_respones = x
            } 
            },
          text = "You will not be able to recover this file!",
          type = "warning",
          showCancelButton = TRUE,
          confirmButtonCol = '#DD6B55',
          confirmButtonText = 'Yes, delete it!'
        )
        
        
        
        }
      
    })
      
      output$dataset_preview <- renderUI({
        if (is.null(input$Storage_DB_rows_selected)) {
          
          return("Please select a data file from storage.")
          
          
        } else {
          DT::renderDataTable(
            values$selected_file_from_DB,
            #editable = "all",
            options = list(scrollX = TRUE,
                           pageLength = 12)
          )
        }
        
      })
    
    
    # observeEvent(input$Storage_DB_rows_selected, {
    # 
    #   if(is.null(input$Storage_DB_rows_selected)==TRUE){
    #   values$selected_file_from_DB <- NULL}
    # })
    
    
    #---------EDA---------------------------------------------------------------
    
    observeEvent(input$sidebar_menu, {
      if (input$sidebar_menu == "EDA") {
        if (is.null(input$plot_types)) {
          shinyjs::hide(id = "select_corr_calc_type")
          shinyjs::hide(id = "selected_vars_EDA_continuous")
          shinyjs::hide(id = "selected_vars_EDA_grouped_continuous")
          shinyjs::hide(id = "selected_vars_EDA_discrete")
          shinyjs::hide(id = "selected_vars_EDA_grouped_discrete")
        }
        
        
        shinyWidgets::updatePickerInput(
          session,
          "select_dataset",
          choices = reactiveDBContent()$file_name,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10
          )
        )
        
      }
      
    })
    
    observe({
      req(input$select_dataset)
      
      values$selected_file_from_DB_to_plot <-
        getSelectedBlobFile(session$userData$auth0_info$sub, input$select_dataset) %>% retype()
      
      values$EDA_vars <- names(values$selected_file_from_DB_to_plot)
      discrete <-
        lapply(values$selected_file_from_DB_to_plot, is.discrete)
      values$EDA_vars_continuous <-
        names(values$selected_file_from_DB_to_plot[which(discrete != TRUE)])
      values$EDA_vars_discrete <-
        names(values$selected_file_from_DB_to_plot[which(discrete == TRUE)])
      
      shinyWidgets::updatePickerInput(
        session,
        "selected_vars_EDA",
        choices = values$EDA_vars,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          maxOptions = 12
        )
      )
      
      shinyWidgets::updatePickerInput(
        session,
        "selected_vars_EDA_continuous",
        choices = values$EDA_vars_continuous,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          maxOptions = 12
        )
      )
      
      shinyWidgets::updatePickerInput(
        session,
        "selected_vars_EDA_grouped_continuous",
        choices = values$EDA_vars_continuous,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          maxOptions = 1
        )
      )
      
      shinyWidgets::updatePickerInput(
        session,
        "selected_vars_EDA_discrete",
        choices = values$EDA_vars_discrete,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          maxOptions = 12
        )
      )
      
      
      shinyWidgets::updatePickerInput(
        session,
        "selected_vars_EDA_grouped_discrete",
        choices = values$EDA_vars_discrete,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 10,
          maxOptions = 1
        )
      )
    })
    
    
    observe({
      req(input$plot_types)
      req(input$plots_tabsetPanel)
      
      if (
        
        ("Descriptive Statistics" %in% input$plot_types &&
         input$plots_tabsetPanel == "Descriptive Statistics") ||
        
        ("Histogram - Continuous" %in% input$plot_types &&
        input$plots_tabsetPanel == "Histogram - Continuous") ||
      
        ("Density" %in% input$plot_types &&
        input$plots_tabsetPanel == "Density") ||
      
        ("Q-Q Plots" %in% input$plot_types &&
        input$plots_tabsetPanel == "Q-Q Plots") ||
      
        ("Box Plots" %in% input$plot_types &&
        input$plots_tabsetPanel == "Box Plots") ||
      
        ("Scatter Plots" %in% input$plot_types &&
        input$plots_tabsetPanel == "Scatter Plots")) {
        
        
        shinyjs::hide(id = "selected_vars_EDA")
        
        shinyjs::hide(id = "selected_vars_EDA_discrete")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_discrete")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_continuous")
        
        shinyjs::show(id = "selected_vars_EDA_continuous")
        

        
        if (
          
          ("Q-Q Plots" %in% input$plot_types &&
             input$plots_tabsetPanel == "Q-Q Plots") ||
          
          ("Box Plots" %in% input$plot_types &&
             input$plots_tabsetPanel == "Box Plots") ||
            
          ("Scatter Plots" %in% input$plot_types &&
              input$plots_tabsetPanel == "Scatter Plots")) {
          
          shinyjs::show(id = "selected_vars_EDA_grouped_continuous")
          

        }
        
      } else if (
        
        ("Barplots - Categorical" %in% input$plot_types &&
        input$plots_tabsetPanel == "Barplots - Categorical")) {
        
        shinyjs::hide(id = "selected_vars_EDA")
        
        shinyjs::hide(id = "selected_vars_EDA_continuous")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_continuous")
        
        shinyjs::show(id = "selected_vars_EDA_discrete")
        
        shinyjs::show(id = "selected_vars_EDA_grouped_discrete")
        

        
        
      } else if (
        
        (!("Descriptive Statistics" %in% input$plot_types) ||
         input$plots_tabsetPanel != "Descriptive Statistics") ||
        
        (!("Barplots - Categorical" %in% input$plot_types) ||
        input$plots_tabsetPanel != "Barplots - Categorical") ||
      
        (!("Histogram - Continuous" %in% input$plot_types) ||
        input$plots_tabsetPanel != "Histogram - Continuous") ||
      
        (!("Density" %in% input$plot_types) ||
         input$plots_tabsetPanel != "Density") ||
      
        (!("Q-Q Plots" %in% input$plot_types) ||
         input$plots_tabsetPanel != "") ||
        
        (!("Box Plots" %in% input$plot_types) ||
         input$plots_tabsetPanel != "Box Plots") ||
        
        (!("Scatter Plots" %in% input$plot_types) ||
        input$plots_tabsetPanel != "Scatter Plots")) {
        
        shinyjs::show(id = "selected_vars_EDA")
        
        shinyjs::hide(id = "selected_vars_EDA_discrete")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_continuous")
        
        shinyjs::hide(id = "selected_vars_EDA_continuous")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_discrete")
        
        
        
      }
      
      
    })
    
    observeEvent(input$plots_tabsetPanel, {
      shinyjs::hide(id = "select_corr_calc_type")
      
      if (input$plots_tabsetPanel == "Multivariate Analysis") {
        shinyjs::show(id = "select_corr_calc_type")
        
      }
      
    })
    
    
    
    output$plot_tabs <- renderUI({
      req(input$select_dataset)
      #req(input$selected_vars_EDA)
      req(input$plot_types)
      
      create_tabs <- function(x) {
        

        shiny::tabPanel(x, {
          #wellPanel(style = "overflow-y:scroll; height:590px", {
            
          if (x == "Dimension") {
            
            renderUI({       
              
              req(input$selected_vars_EDA)
            
              radialNetwork({
                
                DataExplorer::plot_str(values$selected_file_from_DB_to_plot[input$selected_vars_EDA],
                                       type = "radial")
              }, width = "auto")
           
           })
              
          } else if (x == "Basic Info") {
            
              renderPlot({
                
                req(input$selected_vars_EDA)
                
                DataExplorer::plot_intro(values$selected_file_from_DB_to_plot[input$selected_vars_EDA])
              
              }, height = 590)
            
            
          } else if (x == "Summary Statistics") {
            
            renderDataTable({
            
              req(input$selected_vars_EDA)
              
              st(values$selected_file_from_DB_to_plot[input$selected_vars_EDA], out="return")

            }, options = list(
              scrollX = TRUE,
              pageLength = -1,
              dom = 't'
              
            ))
            
          } 
            
            else if (x == "Descriptive Statistics") {
              
              renderDataTable({
                
                req(input$selected_vars_EDA_continuous)
                
                #stat.desc: Descriptive statistics on a data frame or time series
                stat.desc(values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous])
                
              }, options = list(
                scrollX = TRUE,
                pageLength = -1,
                dom = 't'
                
              ))
              
            }
            
            else if (x == "Missing Values") {
            
              renderPlot({
                
                req(input$selected_vars_EDA)
                
                DataExplorer::plot_missing(values$selected_file_from_DB_to_plot[input$selected_vars_EDA]) 
              
              }, height = 590)
              
          } else if (x == "Histogram - Continuous") {

              renderPlotly({
                
                req(input$selected_vars_EDA_continuous)
                
                Hist <-
                DataExplorer::plot_histogram(values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                                             nrow = 1L,
                                             ncol = 2L) 
                
                subplot(Hist, nrows = length(Hist), margin = 0.02) %>% layout(height = (length(Hist) * 590))
                
              })
              

            
          } else if (x == "Density") {
            
              renderPlotly({
                
                req(input$selected_vars_EDA_continuous)
                
                Density <-
                DataExplorer::plot_density(values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                                           nrow = 1L,
                                           ncol = 2L)
                
                subplot(Density, nrows = length(Density), margin = 0.02) %>% layout(height = (length(Density) * 590))
                
                
              })
              
            
            
          } else if (x == "Multivariate Analysis") {
   
              renderPlotly({
                
                req(input$selected_vars_EDA)
                
                DataExplorer::plot_correlation(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA],
                  type = input$select_corr_calc_type ,
                  cor_args = list("use" = "pairwise.complete.obs")
                ) %>% ggplotly(height = 590)
                
              })
              
            
          } else if (x == "Barplots - Categorical") {

            renderPlotly({
              
              req(input$selected_vars_EDA_discrete)
   
              Barplot <-
                DataExplorer::plot_bar(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA_discrete],
                  #with = ,
                  by = input$selected_vars_EDA_grouped_discrete,
                  nrow = 1L,
                  ncol = 2L
                ) 
                  
                  subplot(Barplot, nrows = length(Barplot), margin = 0.02) %>% layout(height = (length(Barplot) * 590))
                  
            
              })
              
            
            
          } else if (x == "Q-Q Plots") {
          
            renderPlotly({
              
              req(input$selected_vars_EDA_continuous)
              
              QQ <-
                DataExplorer::plot_qq(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                  by = input$selected_vars_EDA_grouped_continuous,
                  nrow = 1L,
                  ncol = 2L
                ) 
                
                subplot(QQ, nrows = length(QQ), margin = 0.02) %>% layout(height = (length(QQ) * 590))
                
              })
              
            
          } else if (x == "Box Plots") {
            
              renderPlotly({
                
                req(input$selected_vars_EDA_continuous)
                
                Box <-
                  DataExplorer::plot_boxplot(
                    values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                    by = input$selected_vars_EDA_grouped_continuous,
                    nrow = 1L,
                    ncol = 2L
                  )   
                  
                  subplot(Box, nrows = length(Box), margin = 0.02) %>% layout(height = (length(Box) * 590))
                  
                  
                })
            
          } else if (x == "Scatter Plots") {
          
            
              renderPlotly({
                
                req(input$selected_vars_EDA_continuous)
                req(input$selected_vars_EDA_grouped_continuous)
                
                Scatter <- 
                  DataExplorer::plot_scatterplot(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                  by = input$selected_vars_EDA_grouped_continuous,
                  nrow = 1L,
                  ncol = 2L
                ) 
                
                subplot(Scatter, nrows = length(Scatter), margin = 0.02) %>% layout(height = (length(Scatter) * 590))
                
              })
              
            
          } else if (x == "PCA") {
         
              renderPlotly({

                req(input$selected_vars_EDA)
                
                PCA <-
                  DataExplorer::plot_prcomp(
                    na.omit(values$selected_file_from_DB_to_plot[input$selected_vars_EDA]),
                    nrow = 1L,
                    ncol = 2L
                  )
                
                subplot(PCA, nrows = length(PCA), margin = 0.02) %>% layout(height = (length(PCA) * 590))
                

              })

            
          }
          
        #})
        })
        
      
      
      }
      
      myTabs <- lapply(input$plot_types, create_tabs)
      
      do.call(tabsetPanel, c(id = "plots_tabsetPanel", myTabs))
      
    })
    
    #---------Prediction Model--------------------------------------------------
    
    
    observeEvent(input$sidebar_menu, {
      
      if (input$sidebar_menu == "supervised") {

        shinyjs::hide("ML_Stop_Button")
        
        if(length(reactiveDBContent()$file_name)>0){
        
        shinyWidgets::updatePickerInput(
          session,
          "select_train_dataset",
          choices = reactiveDBContent()$file_name,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10)
        )
        shinyWidgets::updatePickerInput(
          session,
          "select_dependent_variable",
          label = "C. Select the Dependent Variable that you would like to predict:",
          choices = dependent_var_discrete_or_not(),
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 8
          )
        )
        
        } else {
          
          shinyalert(
            html= TRUE,
            title = "Datasets' Storage is Empty!",
            text = tagList(p("Please save a Dataset to proceed with the Machine Learning Model"),
                           actionButton("Go_to_file_uploader_button", label = "Navigate to File Uploader")),
            type = "info",
            #timer = 3000,
            closeOnEsc = FALSE,
            showConfirmButton = FALSE
          )
          
          onclick("Go_to_file_uploader_button", {updateTabItems(session, "sidebar_menu", "upload_files")})
          
        }
        
      } else if(input$sidebar_menu == "ML_models_storage"){
        
        shinyjs::hide("BoxPredictionNewData")
        #shinyjs::disable("Predict")

        
        values$choices_getModelTypes <- getModelTypes(session$userData$auth0_info$sub)
        values$choiches_of_select_dataset_ML_model <- getDatasetNamessPerModelType(session$userData$auth0_info$sub, input$select_modeltype)
        values$choiches_of_select_variable_to_predict <- getTrainedVarPerDatasetNamessPerModelType(session$userData$auth0_info$sub, input$select_modeltype,input$select_dataset_ML_model)
        

        shinyWidgets::updatePickerInput(session,
                                        "select_modeltype",
                                        "A. Select an available ML model type:",
                                        choices = values$choices_getModelTypes$model_type,
                                        options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE))
        
        shinyWidgets::updatePickerInput(session,
                                        "select_dataset_ML_model",
                                        "B. Select the related trained dataset:",
                                        choices = values$choiches_of_select_dataset_ML_model$file_name,
                                        options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE))
        
        shinyWidgets::updatePickerInput(session,
                                        "select_variable_to_predict",
                                        "C. Select the available variable you want to predict:",
                                        choices = values$choiches_of_select_variable_to_predict$dependent_var,
                                        options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE))
        
        shinyWidgets::updatePickerInput(session,
                                        "select_test_dataset_to_predict",
                                        "D. Select the available test dataset you want to predict:",
                                        choices = subset(reactiveDBContent()$file_name,reactiveDBContent()$file_name!=input$select_dataset_ML_model),
                                        options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE))
        
        
      }
      
    })
    
    
    dependent_var_discrete_or_not <- eventReactive(input$checkbox_regression_choice == TRUE | input$checkbox_classification_choice == TRUE,{
                  

        if(input$checkbox_regression_choice == TRUE){
          
          return(colnames(if('ID' %in% colnames(values$train_dataset)){removecolumn(values$train_dataset[which(lapply(values$train_dataset, is.discrete) != TRUE)],"ID")}else{values$train_dataset[which(lapply(values$train_dataset, is.discrete) != TRUE)]}))
                        
        } else if (input$checkbox_classification_choice == TRUE){
          
          #ID is not able to be discrete
          return(colnames(values$train_dataset[which(lapply(values$train_dataset, is.discrete) == TRUE)]))
                        
        }
      
      })

    
    
    observeEvent(input$checkbox_regression_choice == TRUE | input$checkbox_classification_choice == TRUE,{
      
      req(input$select_train_dataset)
      req(input$select_data_partition)
      
      values$train_dataset <-
        getSelectedBlobFile(session$userData$auth0_info$sub,
                            input$select_train_dataset)
      
      values$train_indexes <- createDataPartition(seq.int(nrow(values$train_dataset)), 
                                                  p = (input$select_data_partition/100), 
                                                  list = FALSE)
      
      
      values$train_partition <- values$train_dataset[values$train_indexes, ]
      values$test_partition <- values$train_dataset[-values$train_indexes, ]
      
      
      

        
      shinyWidgets::updatePickerInput(
        session,
        "select_dependent_variable",
        label = "C. Select the Dependent Variable that you would like to predict:",
        choices = dependent_var_discrete_or_not(),
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 8
        )
      )


      
      
    })

    
    observeEvent(input$checkbox_regression_choice == TRUE | input$checkbox_classification_choice == TRUE,{
      

        shinyWidgets::updatePickerInput(
          session,
          "select_dependent_variable",
          label = "C. Select the Dependent Variable that you would like to predict:",
          choices = dependent_var_discrete_or_not(),
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 8
          )
        )
      
    })
    
    
    observe({
      
      req(values$train_dataset)
      req(input$select_dependent_variable)
      
      shinyWidgets::updatePickerInput(
        session,
        "select_independent_variables",
        label = "D. Please choose the Independent Variables:",
        choices =colnames(removecolumn(values$train_dataset,list("ID", input$select_dependent_variable))),
        selected = NULL,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          size = 8
        )
      )
    })
    
    
    #check unique choice in step 2
    observeEvent(input$checkbox_regression_choice , {
      if (input$checkbox_regression_choice == TRUE &&
          input$checkbox_classification_choice == TRUE) {
        shinyWidgets::updatePickerInput(session,
                                        "checkbox_classification_choice",
                                        selected = FALSE
        )
      }
      
    })
    
    
    #check unique choice in step 2
    observeEvent(input$checkbox_classification_choice, {
      if (input$checkbox_regression_choice == TRUE &&
          input$checkbox_classification_choice == TRUE) {
        shinyWidgets::updatePickerInput(session,
                                        "checkbox_regression_choice",
                                        selected = FALSE)
      }
      
    })

    
    #check if input$select_train_dataset changed
    observeEvent(input$select_train_dataset, {
      if (input$checkbox_regression_choice == TRUE |
          input$checkbox_classification_choice == TRUE) {
        shinyWidgets::updatePickerInput(session,
                                        "checkbox_regression_choice",
                                        selected = FALSE)
        
        shinyWidgets::updatePickerInput(session,
                                        "checkbox_classification_choice",
                                        selected = FALSE)
      }
      
    })
    
    observe({

      req(input$checkbox_regression_choice)
      req(input$select_dependent_variable)
      req(input$select_independent_variables)


      values$train_data_x <- data.matrix(subset(values$train_partition, select = input$select_independent_variables))
      values$train_label_y <- values$train_partition[[input$select_dependent_variable]]

      values$test_data_x <- data.matrix(subset(values$test_partition, select = input$select_independent_variables))
      values$test_label_y <- values$test_partition[[input$select_dependent_variable]]

      #values$xgb_train <- xgb.DMatrix(data = values$train_data_x, label = values$train_label_y)
      values$xgb_test <- xgb.DMatrix(data = values$test_data_x, label = values$test_label_y)

    })
    
    
    observe({
      
      req(input$checkbox_classification_choice)
      req(input$select_dependent_variable)
      req(input$select_independent_variables)
      
      
      values$train_data_x <- sparse.model.matrix(~., subset(values$train_partition, select = input$select_independent_variables))
      values$train_label_y <- values$train_partition[[input$select_dependent_variable]]
      
      values$test_data_x <- sparse.model.matrix(~., subset(values$test_partition, select = input$select_independent_variables))
      values$test_label_y <- values$test_partition[[input$select_dependent_variable]]
      

      
    })
    
    
    observeEvent(input$subsample_step_input,{

      shiny::updateSliderInput(session, inputId = "subsample_slider", step = input$subsample_step_input)

    })
    
    observeEvent(input$colsample_bytree_step_input,{
      
      shiny::updateSliderInput(session, inputId = "colsample_bytree_slider", step = input$colsample_bytree_step_input)
      
    })
    
    observeEvent(input$max_depth_step_input,{
      
      shiny::updateSliderInput(session, inputId = "max_depth_slider", step = input$max_depth_step_input)
      
    })
    
    observeEvent(input$min_child_step_input,{
      
      shiny::updateSliderInput(session, inputId = "min_child_slider", step = input$min_child_step_input)
      
    })
    
    observeEvent(input$eta_step_input,{
      
      shiny::updateSliderInput(session, inputId = "eta_slider", step = input$eta_step_input)
      
    })
    
    observeEvent(input$n_rounds_step_input,{
      
      shiny::updateSliderInput(session, inputId = "n_rounds_slider", step = input$n_rounds_step_input)
      
    })
    
    observeEvent(input$n_fold_step_input,{
      
      shiny::updateSliderInput(session, inputId = "n_fold_slider", step = input$n_fold_step_input)
      
    })
    

    
    observe({
      
      if(getCountofModelsperUser(session$userData$auth0_info$sub)==0){

        shinyjs::hide(selector = "a[data-value='ML_models_storage']")
        
      }
      
    })
    
    
    
    
    observeEvent(input$ML_Submit_Button,{
      
      shinyjs::hide("ML_Submit_Button")
      shinyjs::show("ML_Stop_Button")
      

      values$subsample_slider_seq <- seq(from= input$subsample_slider[1], to= input$subsample_slider[2] ,by = input$subsample_step_input)
      values$colsample_bytree_slider_seq <- seq(from = input$colsample_bytree_slider[1] , to = input$colsample_bytree_slider[2], by = input$colsample_bytree_step_input)
      values$max_depth_slider_seq <- seq(from = input$max_depth_slider[1] , to = input$max_depth_slider[2] , by = input$max_depth_step_input)
      values$min_child_weight_slider_seq <- seq(from = input$min_child_weight_slider[1] , to = input$min_child_weight_slider[2] , by = input$min_child_weight_step_input)
      values$eta_slider_seq <- seq(from = input$eta_slider[1] , to = input$eta_slider[2] , by = input$eta_step_input)
      values$n_rounds_slider_seq <- seq(from = input$n_rounds_slider[1] , to = input$n_rounds_slider[2] , by = input$n_rounds_step_input)
      values$n_fold_slider_seq <- seq(from = input$n_fold_slider[1] , to = input$n_fold_slider[2] , by = input$n_fold_step_input )
        
      
      values$bg_process <- r_bg(xgb_gs_cv_regression, 
                                args = list(train_data_x = values$train_data_x,
                                            train_label_y = values$train_label_y,
                                            subsample_choice = values$subsample_slider_seq, 
                                            colsample_bytree_choice = values$colsample_bytree_slider_seq, 
                                            max_depth_choice = values$max_depth_slider_seq, 
                                            min_child_weight_choice = values$min_child_weight_slider_seq, 
                                            eta_choice = values$eta_slider_seq, 
                                            n_rounds_choice = values$n_rounds_slider_seq, 
                                            n_fold_choice = values$n_fold_slider_seq),
                                stdout = "|", 
                                stderr = "2>&1")
      
      
    })
    
    
    
    
    
    observe({
      
      invalidateLater(500)
      req(values$bg_process)
      

      output$RunningTime <- renderText({
        
        paste("Running Time: ", as_hms(difftime(round(Sys.time()), round(values$bg_process$get_start_time()))))
        
      })
      
      if(values$bg_process$poll_io(0)[["process"]] == "ready"){
        
        shinyjs::hide("ML_Stop_Button")
        shinyjs::show("ML_Submit_Button")
        
        #######################
        
        values$result <- values$bg_process$get_result()
        
        values$result <- xgb.Booster.complete(values$result)
        # now the handle points to a valid internal booster model:
        #print(values$result$handle)
        
        values$test_partition_predict <- predict(values$result, values$xgb_test)
        
        if(input$zero_or_not_prediction == FALSE){
          
          values$test_partition_predict[values$test_partition_predict < 0] <- 0
          
        }
        
        values$test_rmse <- caret::RMSE(values$test_label_y, values$test_partition_predict)

        ######################
        
        values$CountofModelsperCase <- getCountofModelsperCase(session$userData$auth0_info$sub,getUploadedFileID(session$userData$auth0_info$sub, input$select_train_dataset),1,input$select_dependent_variable)
        
        if (file.exists('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')) {
          file.remove('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
        }
        
        #No Models Exist
        if(values$CountofModelsperCase$count_models==0){
          
          print("***No Models Exist***")
          
          shinyalert(title = paste0("The CV MLearning Process with Hypertuning just completed!"),
                     text =  paste0("The test RMSE (Best Score) is: ", round(values$test_rmse, 3), "\n",
                                    "The train RMSE (Best Score) is: ",round(values$result$best_score, 3), "\n\n",
                                    "The results came from the following configuration: \n\n",
                                    "Data Partition (%): ", input$select_data_partition, "\n",
                                    "NRounds(niter): ", values$bg_process$get_result()$niter, "\n",
                                    "NFold: ", values$bg_process$get_result()$params$nfold, "\n",
                                    "Max Depth: ", values$bg_process$get_result()$params$max_depth, "\n",
                                    "ETA: ", values$bg_process$get_result()$params$eta, "\n",
                                    "Subsample: ", values$bg_process$get_result()$params$subsample, "\n",
                                    "Colsample by tree: ", values$bg_process$get_result()$params$colsample_bytree, "\n",
                                    "Min child weight: ", values$bg_process$get_result()$params$min_child_weight, "\n\n",
                                    "Would you like to save the first Model of the Dataset: ", input$select_train_dataset ,
                                    " and the Dependent Variable: ",input$select_dependent_variable ," ?"),
                     callbackR = function(x){
                       
                       if(x==TRUE){
                         
                         shinyalert(title = "The Regression Model has been saved succefully",
                                    text = paste0("The first Regression Model of the Dataset: ", input$select_train_dataset , 
                                                  " with Dependent Variable: ",input$select_dependent_variable, " just saved!"),
                                    type = "success",
                                    inputId = { 
                                      
                                      xgb.save(values$result, 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
                                      
                                      saveModeltoDB(session$userData$auth0_info$sub,
                                                    1,
                                                    getUploadedFileID(session$userData$auth0_info$sub, input$select_train_dataset)[["id"]],
                                                    input$select_dependent_variable,
                                                    input$select_independent_variables,
                                                    input$select_data_partition,
                                                    values$test_rmse,
                                                    values$test_label_y,
                                                    values$test_partition_predict)
                                      
                                      if (file.exists('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')) {
                                        file.remove('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
                                      }
                                      
                                    },
                                    confirmButtonText = 'Ok')
                         
                       }
                       
                     },
                     type = "success",
                     showCancelButton = TRUE,
                     cancelButtonText = 'No, I will proceed with new MLearning!',
                     confirmButtonText = 'Yes, Save it!')
          
        } 
        
        else{
          
          print("***Model Exists***")
          
          getModelFile(session$userData$auth0_info$sub,
                       getUploadedFileID(session$userData$auth0_info$sub, input$select_train_dataset),
                       1,
                       input$select_dependent_variable)
          
          values$exist_model <- xgb.load('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
          
          values$exists_model_test_RMSE <- getTestRMSEofModel(session$userData$auth0_info$sub,
                                                              getUploadedFileID(session$userData$auth0_info$sub, input$select_train_dataset),
                                                              1,
                                                              input$select_dependent_variable)
          
          if (file.exists('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')) {
            file.remove('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
          }
          
          #New Model with Better test RMSE Score
          if(values$test_rmse < values$exists_model_test_RMSE){
            print("***New Model with Better test RMSE Score***")
            
            xgb.save(values$result, 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
            
            saveModeltoDB(session$userData$auth0_info$sub,
                          1,
                          getUploadedFileID(session$userData$auth0_info$sub, input$select_train_dataset)[["id"]],
                          input$select_dependent_variable,
                          input$select_independent_variables,
                          input$select_data_partition,
                          values$test_rmse,
                          values$test_label_y,
                          values$test_partition_predict)
            
            if (file.exists('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')) {
              file.remove('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
            }
            
            shinyalert(title = "New Best Score!",
                       text = paste0("The New train RMSE (Best Score) is: ",round(values$result$best_score, 3), "\n",
                                     "The New test RMSE (Best Score) is: ", round(values$test_rmse, 3), "\n\n",
                                     "The Old train RMSE (Best Score) is: ", round(values$exist_model$best_score, 3), "\n",
                                     "The Old test RMSE (Best Score) is: ", round(values$exists_model_test_RMSE, 3), "\n\n",
                                     "The results came from the following configuration: \n\n",
                                     "NRounds(niter): ", values$bg_process$get_result()$niter, "\n",
                                     "NFold: ", values$bg_process$get_result()$params$nfold, "\n",
                                     "Max Depth: ", values$bg_process$get_result()$params$max_depth, "\n",
                                     "ETA: ", values$bg_process$get_result()$params$eta, "\n",
                                     "Subsample: ", values$bg_process$get_result()$params$subsample, "\n",
                                     "Colsample by tree: ", values$bg_process$get_result()$params$colsample_bytree, "\n",
                                     "Min child weight: ", values$bg_process$get_result()$params$min_child_weight, "\n\n",
                                     "The old Regression Model of the Dataset: ", input$select_train_dataset , 
                                     " with Dependent Variable: ",input$select_dependent_variable, " just replaced with the best one!"),
                       type = "success",
                       confirmButtonText = 'Ok')
            
          }
          
          #New Model with worse or equal RMSE Score  
          else if(values$test_rmse >= values$exists_model_test_RMSE){
            print("***New Model with worse or equal RMSE Score***")
            
            shinyalert(title = "The Regression Model hasn't been improved!",
                       text = paste0("The New train RMSE (Best Score) is: ",round(values$result$best_score, 3), "\n",
                                     "The New test RMSE (Best Score) is: ", round(values$test_rmse, 3), "\n\n",
                                     "The Old train RMSE (Best Score) is: ", round(values$exist_model$best_score, 3), "\n",
                                     "New Old test RMSE (Best Score) is: ", round(values$exists_model_test_RMSE, 3), "\n\n",
                                     "The results came from the following configuration: \n\n",
                                     "NRounds(niter): ", values$bg_process$get_result()$niter, "\n",
                                     "NFold: ", values$bg_process$get_result()$params$nfold, "\n",
                                     "Max Depth: ", values$bg_process$get_result()$params$max_depth, "\n",
                                     "ETA: ", values$bg_process$get_result()$params$eta, "\n",
                                     "Subsample: ", values$bg_process$get_result()$params$subsample, "\n",
                                     "Colsample by tree: ", values$bg_process$get_result()$params$colsample_bytree, "\n",
                                     "Min child weight: ", values$bg_process$get_result()$params$min_child_weight, "\n\n",
                                     "Would you like to replace the old Regression Model with the new one of the Dataset: ", input$select_train_dataset , 
                                     " with Dependent Variable: ",input$select_dependent_variable," ?"),
                       type = "warning",
                       callbackR = function(x){
                         
                         if(x==TRUE){
                           
                           xgb.save(values$result, 'C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
                           
                           saveModeltoDB(session$userData$auth0_info$sub,
                                         1,
                                         getUploadedFileID(session$userData$auth0_info$sub, input$select_train_dataset)[["id"]],
                                         input$select_dependent_variable,
                                         input$select_independent_variables,
                                         input$select_data_partition,
                                         values$test_rmse,
                                         values$test_label_y,
                                         values$test_partition_predict)
                           
                           if (file.exists('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')) {
                             file.remove('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
                           }
                           
                           shinyalert(title = "The New Regression Model have been replaced succefully",
                                      text = paste0("Just replaced the Regression Model of the Dataset: ", input$select_train_dataset , 
                                                    " with Dependent Variable: ",input$select_dependent_variable),
                                      type = "success",
                                      confirmButtonText = 'Ok')
                           
                         }
                         
                       },
                       showCancelButton = TRUE,
                       cancelButtonText = 'Ignore, keep the old one',
                       confirmButtonText = 'Yes, save the new model')
            
          }
          
        }  



        #return to first step after result redirection
        click("Back_to_1_Step")
        click("ML_models_storage")
        
        output$RunningTime <- NULL
        values$bg_process <- NULL

        
      }
    })


    
    
    
    
    
    observeEvent(input$ML_Stop_Button, {

      shinyalert(
        title = "Are you sure you want to stop the MLearning process?",
        callbackR = function(x){

          if(x==TRUE){

            shinyalert(
              title = "The MLearning process have been stopped",
              type = "success",
              inputId = {
                shinyjs::hide("ML_Stop_Button")
                shinyjs::show("ML_Submit_Button")
                cat(paste("Killing process - PID:", values$bg_process$get_pid(), "\n"))
                values$bg_process$kill()
                output$RunningTime <- NULL
                values$bg_process <- NULL},
              confirmButtonText = 'Ok'
            )

          }

        },
        text = "Kindly note, if you stop the learning you should start from the beginning a new one!",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonCol = '#DD6B55',
        confirmButtonText = 'Yes, stop it!'
      )

    })
    
    
    
    
    
    observeEvent(input$select_modeltype,{
      
      values$choiches_of_select_dataset_ML_model <- getDatasetNamessPerModelType(session$userData$auth0_info$sub, input$select_modeltype)
      
      shinyWidgets::updatePickerInput(session,
                                      "select_dataset_ML_model",
                                      "B. Select the releted trained dataset:",
                                      choices = values$choiches_of_select_dataset_ML_model$file_name,
                                      options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE))
      
    })
    
    
    observeEvent(input$select_dataset_ML_model,{
      
      values$choiches_of_select_variable_to_predict <- getTrainedVarPerDatasetNamessPerModelType(session$userData$auth0_info$sub, input$select_modeltype,input$select_dataset_ML_model)
      
      shinyWidgets::updatePickerInput(session,
                                      "select_variable_to_predict",
                                      "C. Select the available variable you want to predict:",
                                      choices = values$choiches_of_select_variable_to_predict$dependent_var,
                                      options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE))
    })
    
    
    observeEvent(input$select_variable_to_predict,{
      
      if (file.exists('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')) {
        file.remove('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
      }
      
      getModelFile(session$userData$auth0_info$sub,
                   getUploadedFileID(session$userData$auth0_info$sub, input$select_dataset_ML_model),
                   if(input$select_modeltype=="Regression"){1},
                   input$select_variable_to_predict)
      
      values$exist_model <- xgb.load('C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model')
      
      values$feature_names <- strsplit(getIndependentVarsofModel(session$userData$auth0_info$sub,
                                                                 getUploadedFileID(session$userData$auth0_info$sub, input$select_dataset_ML_model),
                                                                 if(input$select_modeltype=="Regression"){1},
                                                                 input$select_variable_to_predict)$independent_vars, ", ")[[1]]
      
      values$importance_table <- xgb.importance(feature_names = values$feature_names, model = values$exist_model)
      
      output$TrainRMSE <- renderValueBox({
        valueBox(
          
          round(values$exist_model$best_score,3), "Train RMSE", #shiny::icon(""), color = ""
        )
      })
      
      output$TestRMSE <- renderValueBox({
        valueBox(
          
          round(getTestRMSEofModel(session$userData$auth0_info$sub,
                                   getUploadedFileID(session$userData$auth0_info$sub, input$select_dataset_ML_model),
                                   1,
                                   input$select_variable_to_predict),3), "Test RMSE", #shiny::icon(""), color = ""
        )
      })
      
      
      output$Partition_perc <- renderValueBox({
        valueBox(
          
          round(getDataPartitionofModel(session$userData$auth0_info$sub,
                                   getUploadedFileID(session$userData$auth0_info$sub, input$select_dataset_ML_model),
                                   1,
                                   input$select_variable_to_predict),3), "Data Train Partition %", #shiny::icon(""), color = ""
        )
      })
      
      output$feature_importance_plot <- renderPlotly({
        
        xgb.ggplot.importance(values$importance_table) %>% ggplotly() %>% layout(legend = list(x = 0.8, y = 0.1))
        
      })

      output$feature_importance_table <- DT::renderDataTable({
        
        values$importance_table %>% mutate_if(is.numeric, round, 3)
        
      },
      options = list(
        #pageLength = 12,
        scrollX = TRUE,
        paging = FALSE,
        dom = 't'
        
      ))
      

      
      values$test_label_y <- strsplit(getTestOriginalPartition(session$userData$auth0_info$sub,
                                                                  getUploadedFileID(session$userData$auth0_info$sub, input$select_dataset_ML_model),
                                                                  if(input$select_modeltype=="Regression"){1},
                                                                  input$select_variable_to_predict)$test_original_partition, ", ")[[1]]
      
      values$test_partition_predict <- strsplit(getTestPrediction(session$userData$auth0_info$sub,
                                                                  getUploadedFileID(session$userData$auth0_info$sub, input$select_dataset_ML_model),
                                                                  if(input$select_modeltype=="Regression"){1},
                                                                  input$select_variable_to_predict)$test_prediction, ", ")[[1]]
      

      # output$XGB_Tree_Plot <- renderGrViz({
      #   
      #   xgb.plot.tree(feature_names = values$feature_names,model = values$exist_model$finalModel, trees= 0:1, show_node_id=TRUE)
      #   
      # })
      
      index <- 1:length(values$test_label_y)
      original <- values$test_label_y %>% as.numeric()
      predicted <- values$test_partition_predict %>% as.numeric()
      values$predicted <- predicted
      data <- data.frame(index,original,predicted)

      # #melt data frame into long format
      # df <- reshape2::melt(data ,  
      #                      id.vars = 'index', 
      #                      variable.name = 'data'
      #                      )

      fig <- plot_ly(data, x = ~index, y = ~original, name = 'original', type = 'scatter', mode = 'lines+markers') 
      fig <- fig %>% add_trace(y = ~predicted, name = 'predicted', mode = 'lines+markers') 
      fig <- fig %>% layout(hovermode = 'x', yaxis = list(title = input$select_variable_to_predict),legend = list(x = 0.1, y = 0.9))


      output$prediction_plot <- renderPlotly({
        
      #   # Plot
      #   viz <- df %>%
      #   ggplot(aes(index, value)) +
      #     geom_line(aes(colour = data))+
      #     coord_fixed()
      #   
      #   plot_ly(viz + theme(legend.position = 'none'), dynamicTicks = TRUE) %>%
      #     highlight("plotly_selected")

        fig
        
      })
        
      #----------
      
      shinyWidgets::updatePickerInput(session,
                                      "select_test_dataset_to_predict",
                                      "D. Select the available test dataset you want to predict:",
                                      choices = subset(reactiveDBContent()$file_name,reactiveDBContent()$file_name!=input$select_dataset_ML_model),
                                      options = pickerOptions(actionsBox = FALSE, liveSearch = FALSE))


      
    })
    
    
    observe({
      
      req(values$feature_names)
      req(input$select_test_dataset_to_predict)
      
      values$select_test_dataset_to_predict <- getSelectedBlobFile(session$userData$auth0_info$sub,input$select_test_dataset_to_predict)
      
      temp <- values$feature_names %in% colnames(values$select_test_dataset_to_predict)

     if(sum(temp, na.rm=TRUE)>1){
       
       shinyjs::enable("Predict")
 
     } else {
       
       shinyjs::disable("Predict")
     }
      
    })

    
    observeEvent(input$Predict,{
      
      req(values$exist_model)
      req(values$select_test_dataset_to_predict)
      
      shinyjs::show("BoxPredictionNewData")
      
      values$new_dataset_to_predict <- values$select_test_dataset_to_predict %>% as.data.frame()
      
      values$dmatrix_new_prediction <- xgb.DMatrix(data = data.matrix(subset(values$new_dataset_to_predict, select = values$feature_names)))

      values$new_dataset_to_predict[,input$select_variable_to_predict] <- predict(values$exist_model, values$dmatrix_new_prediction)

      if(sum(subset(values$predicted, values$predicted < 0)) == 0) {
        
        values$new_dataset_to_predict[input$select_variable_to_predict] <- replace(values$new_dataset_to_predict[input$select_variable_to_predict], values$new_dataset_to_predict[input$select_variable_to_predict] < 0, 0)
        
      }
        
      output$New_Prediction <- renderDataTable({values$new_dataset_to_predict},
                                               options = list(scrollX = TRUE,
                                                              pageLength = 12))
      
    })


  }
  
  a0_server <- auth0_server(server , info = a0_info)
  
  # Run the application
  shiny::runApp(shinyApp(ui = a0_ui, server = a0_server), launch.browser = TRUE)
  
}
