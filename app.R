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
        )



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
  #----------UI-----------------------------------------------------------------
  source("ui_header.R")
  source("ui_body.R")
  source("ui_sidebar.R")
  
  ui <- dashboardPage(header, sidebar, body)
  
  a0_ui <- auth0_ui(ui , info = a0_info)
  
  
  #----------SERVER-------------------------------------------------------------
  
  server <-  function(input, output, session) {
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
      if (!(session$userData$auth0_info$sub %in% send_api$user_id)) {
        
        saveUsertoDB(session$userData$auth0_info)
        dUser
        
      } else{

        dUser
        
      }
    )
    

    
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
        validate(
          "Invalid file; Please upload a .csv, .sav, .tsv, .xls or .xlsx file"
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
          choices = reactiveDBContent(),
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
      
      
    })
    
    
    observe({
      req(input$plot_types)
      req(input$plots_tabsetPanel)
      
      if ((
        "Histogram - Continuous" %in% input$plot_types &&
        input$plots_tabsetPanel == "Histogram - Continuous"
      ) ||
      ("Density" %in% input$plot_types &&
       input$plots_tabsetPanel == "Density") ||
      ("Q-Q Plot" %in% input$plot_types &&
       input$plots_tabsetPanel == "Q-Q Plot") ||
      ("Box Plots" %in% input$plot_types &&
       input$plots_tabsetPanel == "Box Plots") ||
      (
        "Scatter Plots" %in% input$plot_types &&
        input$plots_tabsetPanel == "Scatter Plots"
      )) {
        shinyjs::hide(id = "selected_vars_EDA")
        
        shinyjs::hide(id = "selected_vars_EDA_discrete")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_discrete")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_continuous")
        
        shinyjs::show(id = "selected_vars_EDA_continuous")
        
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
        
        if (("Q-Q Plot" %in% input$plot_types &&
             input$plots_tabsetPanel == "Q-Q Plot") ||
            ("Box Plots" %in% input$plot_types &&
             input$plots_tabsetPanel == "Box Plots") ||
            (
              "Scatter Plots" %in% input$plot_types &&
              input$plots_tabsetPanel == "Scatter Plots"
            )) {
          shinyjs::show(id = "selected_vars_EDA_grouped_continuous")
          
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
          
        }
        
      } else if ((
        "Barplots - Categorical" %in% input$plot_types &&
        input$plots_tabsetPanel == "Barplots - Categorical"
      )) {
        shinyjs::hide(id = "selected_vars_EDA")
        
        shinyjs::hide(id = "selected_vars_EDA_continuous")
        
        shinyjs::hide(id = "selected_vars_EDA_grouped_continuous")
        
        shinyjs::show(id = "selected_vars_EDA_discrete")
        
        shinyjs::show(id = "selected_vars_EDA_grouped_discrete")
        
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
        
        
      } else if ((
        !("Barplots - Categorical" %in% input$plot_types) ||
        input$plots_tabsetPanel != "Barplots - Categorical"
      ) ||
      (
        !("Histogram - Continuous" %in% input$plot_types) ||
        input$plots_tabsetPanel != "Histogram - Continuous"
      ) ||
      (!("Density" %in% input$plot_types) ||
       input$plots_tabsetPanel != "Density") ||
      (!("Q-Q Plot" %in% input$plot_types) ||
       input$plots_tabsetPanel != "Q-Q Plot") ||
      (!("Box Plots" %in% input$plot_types) ||
       input$plots_tabsetPanel != "Box Plots") ||
      (
        !("Scatter Plots" %in% input$plot_types) ||
        input$plots_tabsetPanel != "Scatter Plots"
      )) {
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
          wellPanel(style = "overflow-y:scroll; height:590px", {
          if (x == "Dimension") {
            
              radialNetwork({
                DataExplorer::plot_str(values$selected_file_from_DB_to_plot[input$selected_vars_EDA],
                                       type = "radial")
              }, width = "auto")
           
          } else if (x == "Basic Info") {
 
              renderPlotly({
                DataExplorer::plot_intro(values$selected_file_from_DB_to_plot[input$selected_vars_EDA]) %>% ggplotly(height = 560)
              })
            
          } else if (x == "Missing Values") {
            
              renderPlotly({
                DataExplorer::plot_missing(values$selected_file_from_DB_to_plot[input$selected_vars_EDA]) %>% ggplotly(height = 560)
              })
            
          } else if (x == "Histogram - Continuous") {

              renderPlotly({
                Hist <-
                DataExplorer::plot_histogram(values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                                             nrow = 1L,
                                             ncol = 2L) 
                
                subplot(Hist, nrows = length(Hist), margin = 0.02) %>% ggplotly() %>% layout(height = (length(Hist) * 590))
                
              })
              
            
            
          } else if (x == "Density") {
     
              renderPlotly({
                Density <-
                DataExplorer::plot_density(values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                                           nrow = 1L,
                                           ncol = 2L)
                
                subplot(Density, nrows = length(Density), margin = 0.02) %>% ggplotly() %>% layout(height = (length(Density) * 590))
                
                
              })
            
          } else if (x == "Multivariate Analysis") {
   
              renderPlotly({
                DataExplorer::plot_correlation(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA],
                  type = input$select_corr_calc_type ,
                  cor_args = list("use" = "pairwise.complete.obs")
                ) %>% ggplotly() %>% layout(height = '560')
              })
            
          } else if (x == "Barplots - Categorical") {
              
            renderPlotly({
              Barplot <-
                DataExplorer::plot_bar(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA_discrete],
                  #with = ,
                  by = input$selected_vars_EDA_grouped_discrete,
                  nrow = 1L,
                  ncol = 2L
                ) 
                  
                  subplot(Barplot, nrows = length(Barplot), margin = 0.02) %>% ggplotly() %>% layout(height = (length(Barplot) * 590))
                  
                
              })
            
          } else if (x == "Q-Q Plot") {
          
            renderPlotly({
              QQ <-
                DataExplorer::plot_qq(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                  by = input$selected_vars_EDA_grouped_continuous,
                  nrow = 1L,
                  ncol = 2L
                ) 
                
                subplot(QQ, nrows = length(QQ), margin = 0.02) %>% ggplotly() %>% layout(height = (length(QQ) * 590))
                
              })
            
          } else if (x == "Box Plots") {
            
            renderPlotly({
              Box <-
                DataExplorer::plot_boxplot(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                  by = input$selected_vars_EDA_grouped_continuous,
                  nrow = 1L,
                  ncol = 2L
                ) 
                
                subplot(Box, nrows = length(Box), margin = 0.02) %>% ggplotly() %>% layout(height = (length(Box) * 590))
                
                
              })
            
          } else if (x == "Scatter Plots") {
            
              renderPlotly({
                Scatter <- 
                  DataExplorer::plot_scatterplot(
                  values$selected_file_from_DB_to_plot[input$selected_vars_EDA_continuous],
                  by = input$selected_vars_EDA_grouped_continuous,
                  nrow = 1L,
                  ncol = 2L
                ) 
                
                subplot(Scatter, nrows = length(Scatter), margin = 0.02) %>% ggplotly() %>% layout(height = (length(Scatter) * 590))
                
              })
            
          } else if (x == "Principal Component Analysis") {
         
              renderPlotly({
                PCA <-
                  DataExplorer::plot_prcomp(
                    na.omit(values$selected_file_from_DB_to_plot[input$selected_vars_EDA]),
                    nrow = 1L,
                    ncol = 2L
                  )
                
                subplot(PCA, nrows = length(PCA), margin = 0.02) %>% ggplotly() %>% layout(height = (length(PCA) * 590))
                
                
            
              
            })
            
          }
          
        })
        })
        
      }
      
      myTabs <- lapply(input$plot_types, create_tabs)
      
      do.call(tabsetPanel, c(id = "plots_tabsetPanel", myTabs))
      
    })
    
    #---------Prediction Model--------------------------------------------------
    
    
    observeEvent(input$sidebar_menu, {
      if (input$sidebar_menu == "supervised") {
        
        shinyWidgets::updatePickerInput(
          session,
          "select_train_dataset",
          choices = reactiveDBContent(),
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10
          )
        )

      }
      
    })
    
    depended_var_discrete_or_not <-
    eventReactive(input$checkbox_regression_choice == TRUE |
                  input$checkbox_classification_choice == TRUE,{
                      
                      if(input$checkbox_regression_choice == TRUE){
                        
                        return(colnames(if('ID' %in% colnames(values$train_dataset)){removecolumn(values$train_dataset[which(lapply(values$train_dataset, is.discrete) != TRUE)],"ID")}else{values$train_dataset[which(lapply(values$train_dataset, is.discrete) != TRUE)]}))
                        
                      } 
                      
                      else if (input$checkbox_classification_choice == TRUE){
                        #ID is not able to be discrete
                        return(colnames(values$train_dataset[which(lapply(values$train_dataset, is.discrete) == TRUE)]))
                        
                      }
                      
                  })
    
    observe({
      
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
          label = "C. Select the Depended Variable that you would like to predict:",
          choices = depended_var_discrete_or_not(),
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 8
          )
        )
        

      
      
      
    })
    
    observeEvent(input$select_dependent_variable,{
    
      shinyWidgets::updatePickerInput(
        session,
        "select_independent_variables",
        label = "D. Please choose the Independed Variables:",
        choices =colnames(removecolumn(values$train_dataset,list("ID", input$select_dependent_variable))),
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

    
    
    observe({

      req(input$checkbox_regression_choice)
      req(input$select_dependent_variable)
      req(input$select_independent_variables)


      values$train_data_x <- data.matrix(subset(values$train_partition, select = input$select_independent_variables))
      values$train_label_y <- values$train_partition[[input$select_dependent_variable]]

      values$test_data_x <- data.matrix(subset(values$test_partition, select = input$select_independent_variables))
      values$test_label_y <- values$test_partition[[input$select_dependent_variable]]

      values$xgb_train <- xgb.DMatrix(data = values$train_data_x, label = values$train_label_y)
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

    
    
  }
  
  a0_server <- auth0_server(server , info = a0_info)
  
  # Run the application
  shiny::runApp(shinyApp(ui = a0_ui, server = a0_server), launch.browser = TRUE)
  
}
