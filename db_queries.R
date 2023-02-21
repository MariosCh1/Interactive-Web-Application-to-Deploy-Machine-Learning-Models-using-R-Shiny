#----------DATABASE OPTIONS-----------------------------------------------------


options(
  mysql = list(
    "databaseName" = "mbads_thesis_db",
    "host" = "127.0.0.1",
    "port" = 3306,
    "user" = "developer",
    "password" = "123456@!"
  )
)

#----------DATABASE QUERIES-----------------------------------------------------

# CREATE TABLE Users (
#   user_id varchar(255) NOT NULL,
#   family_name varchar(255),
#   given_name varchar(255),
#   PRIMARY KEY (user_id)
# );

# CREATE TABLE uploadedfiles (
#   id INTEGER AUTO_INCREMENT,
#   sub VARCHAR(255),
#   file_name TEXT,
#   uploaded_file LONGBLOB,
#   PRIMARY KEY (id),
#   FOREIGN KEY (sub) REFERENCES users(sub)
# ) ;

# CREATE TABLE savedmodels (
#   sub VARCHAR(255),
#   modeltype_id INTEGER,
#   dataset_id INTEGER,
#   dependent_var VARCHAR(255),
#   independent_vars TEXT,
#   data_partition INTEGER,
#   test_rmse DOUBLE,
#   test_original_partition LONGTEXT,
#   test_prediction LONGTEXT,
#   last_update TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
#   uploaded_model LONGBLOB,
#   PRIMARY KEY (modeltype_id, dataset_id, dependent_var),
#   FOREIGN KEY (sub) REFERENCES users(sub),
#   FOREIGN KEY (dataset_id) REFERENCES uploadedfiles(id),
#   FOREIGN KEY (modeltype_id) REFERENCES modeltypes(id)
# ) ;
# 
# 
# CREATE TABLE modeltypes (
#   id INTEGER,
#   model_type TEXT,
#   PRIMARY KEY (id)
# ) ;

# INSERT INTO modeltypes (id, model_type) VALUES ("1","Regression");
# INSERT INTO modeltypes (id, model_type) VALUES ("2","Classification");


# https://www.digitalocean.com/community/questions/how-to-enable-local-capability-in-mysql-workbench
# SHOW GLOBAL VARIABLES LIKE 'local_infile';
# SET GLOBAL local_infile = true;

set_global_local_infile <- function(){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- "SET GLOBAL local_infile = true;"

  
  request <- DBI::dbGetQuery(dbcon, query)
  
  print("SET GLOBAL local_infile = true; - OK")
  
  DBI::dbDisconnect(dbcon)
  
  
}


#----------DATABASE FUNCTIONS---------------------------------------------------

saveUsertoDB <- function(user){
  print ("**************saveUsertoDB***************")
  
  dbcon <- RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  #query <- sprintf("INSERT INTO Users (user_id, family_name, given_name) VALUES ('%s', '%s', '%s')",user_id, family_name, given_name)
  #query <- paste("INSERT INTO Users (user_id, family_name, given_name) VALUES ('",user_id,"', '",family_name,"', '",given_name,"')")
  #print(query)
  
  user_tibble <- as_tibble(user)
  #print(user_tibble)
  
  RMySQL::dbWriteTable(dbcon,"users",user_tibble, append= TRUE, row.names=FALSE)
  
  RMySQL::dbDisconnect(dbcon)
  
  #DBI::dbGetQuery(dbcon, query)
  
  #DBI::dbDisconnect(dbcon)
  
  
}


# createDataTabletoDB <- function(table_name, data) {
#   print ("**************createDataTabletoDB***************")
#   
#   dbcon <- RMySQL::dbConnect(
#     RMySQL::MySQL(),
#     dbname = options()$mysql$databaseName,
#     host = options()$mysql$host,
#     port = options()$mysql$port,
#     user = options()$mysql$user,
#     password = options()$mysql$password
#   )
#   
#   table_name_nospace <- tolower(gsub(" ","",table_name))
#   table_name_nodots <- gsub("\\.","",table_name_nospace)
#   
#   RMySQL::dbWriteTable(dbcon, table_name_nodots, data, append= TRUE)
#   
#   RMySQL::dbDisconnect(dbcon)
#   
# }
# 
# writeDataTabletoDB <- function(table_name, data) {
#   print ("**************writeDataTabletoDB***************")
#   
#   dbcon <- RMySQL::dbConnect(
#     RMySQL::MySQL(),
#     dbname = options()$mysql$databaseName,
#     host = options()$mysql$host,
#     port = options()$mysql$port,
#     user = options()$mysql$user,
#     password = options()$mysql$password
#   )
#   
#   table_name_nospace <- tolower(gsub(" ","",table_name))
#   table_name_nodots <- gsub("\\.","",table_name_nospace)
#   
#   RMySQL::dbWriteTable(dbcon, table_name_nodots, data, overwrite= TRUE)
#   
#   RMySQL::dbDisconnect(dbcon)
#   
# }

uploadedFiletoDB <- function(sub,file_name) {
  print ("**************uploadedFiletoDB***************")
  
  dbcon <- RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  # table_name_nospace <- tolower(gsub(" ","",file_name))
  # table_name_nodots <- gsub("\\.","",file_name)
  # 
  file_name <- paste0(file_name,".csv")
  print(file_name)

  
  
  insert_data = data.frame(sub, file_name)
  print(insert_data)
  
  RMySQL::dbWriteTable(dbcon, "uploadedfiles", 
                       insert_data, field.types = c(id = "INTEGER AUTO_INCREMENT PRIMARY KEY", 
                                                    sub = "VARCHAR(255)", #FOREIGN KEY - DON'T DROP THE TABLE
                                                    file_name = "TEXT",
                                                    uploaded_file = "LONGBLOB"),
                       append = TRUE, row.names = FALSE)
  
  uploaded_file_query <- paste0("C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\", file_name)

  query <- dbplyr::build_sql("UPDATE uploadedfiles SET uploaded_file = LOAD_FILE(",uploaded_file_query,
                  ") WHERE sub = ",sub," AND file_name = ",file_name,";", con = dbcon)
  
  rs <- DBI::dbSendQuery(dbcon, query)
  
  DBI::dbClearResult(rs)
  
  RMySQL::dbDisconnect(dbcon)

}

fileNameExistsinDB <- function(sub, file_name) {
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  # table_name_nospace <- tolower(gsub(" ","",table_name))
  # table_name_nodots <- gsub("\\.","",table_name_nospace)
  
  request0 <- DBI::dbGetQuery(dbcon, "show tables")
  print(request0)
  
  if (!("uploadedfiles" %in% request0$Tables_in_mbads_thesis_db)) {
    return(TRUE)
    DBI::dbDisconnect(dbcon)
    
  } 
  
  else{
    file_name <- paste0(file_name, ".csv")
    print(file_name)
    
    query <-
      paste0(
        "SELECT COUNT(*) FROM uploadedfiles WHERE file_name = '",
        file_name,
        "' AND sub = '",
        sub,
        "';"
      )
    
    print(query)
    
    request <- DBI::dbGetQuery(dbcon, query)
    print(request)
    
    
    if (request == 0) {
      DBI::dbDisconnect(dbcon)
      return(TRUE)
      
      
    } else {
      DBI::dbDisconnect(dbcon)
      return(FALSE)
      
    }
    
  }
  
}

deleteFilefromDB <- function(sub,file_name) {
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "DELETE FROM uploadedfiles WHERE file_name = '",
      file_name,
      "' AND sub = '",
      sub,
      "';"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  
}

getSavedFiles <-function(sub){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT file_name FROM uploadedfiles WHERE sub = '",
      sub,
      "';"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}

getCountofUsers <-function(sub){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT COUNT(*) FROM users WHERE sub = '",
      sub,
      "';"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}

getSelectedBlobFile <-function(sub, file_name){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT uploaded_file FROM uploadedfiles WHERE sub = '",sub, "' 
                  AND file_name = '",file_name,"';")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  data <- read_csv(paste0(request, collapse = "\r\n"))

  DBI::dbDisconnect(dbcon)

  return(data)

}

getUploadedFileID <-function(sub,file_name){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT id FROM uploadedfiles WHERE sub = '",sub, "' 
                  AND file_name = '",file_name,"';")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}

getModelTypes <-function(sub){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT distinct(B.model_type) FROM mbads_thesis_db.savedmodels A
        LEFT JOIN mbads_thesis_db.modeltypes B on A.modeltype_id=B.id
        WHERE sub = '",
        sub,
        "';"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}

getDatasetNamessPerModelType <- function(sub, model_type){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT distinct(C.file_name) 
       FROM mbads_thesis_db.savedmodels A
       LEFT JOIN mbads_thesis_db.modeltypes B 
       on A.modeltype_id=B.id
       LEFT JOIN mbads_thesis_db.uploadedfiles C
       on A.dataset_id=C.id
       WHERE A.sub = '", sub,"' AND B.model_type = '", model_type,
      "' ;"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}

getTrainedVarPerDatasetNamessPerModelType <- function(sub, model_type, file_name){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT A.dependent_var 
       FROM mbads_thesis_db.savedmodels A
       LEFT JOIN mbads_thesis_db.modeltypes B 
       on A.modeltype_id=B.id
       LEFT JOIN mbads_thesis_db.uploadedfiles C
       on A.dataset_id=C.id
       WHERE A.sub = '", sub,"' AND B.model_type = '", model_type, "' AND C.file_name = '", file_name,
      "' ;"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}

getCountofDatasets <-function(sub){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT COUNT(*) FROM uploadedfiles WHERE sub = '",
      sub,
      "';"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}

getCountofModelsperUser <-function(sub){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT COUNT(*) FROM savedmodels WHERE sub = '",
      sub,
      "';"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}


getCountofModelsperCase <-function(sub, dataset_id,modeltype_id,dependent_var){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <-
    paste0(
      "SELECT COUNT(*) count_models FROM savedmodels WHERE sub = '",sub,"' AND dataset_id = ",dataset_id, " AND modeltype_id = ",modeltype_id, " AND dependent_var = '",dependent_var, "' ;"
    )
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
}


saveModeltoDB <- function(sub, modeltype_id, dataset_id, dependent_var, independent_vars, data_partition, test_rmse, test_original_partition, test_prediction) {
  print ("**************saveModeltoDB***************")
  
  dbcon <- RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )

  independent_vars <- toString(independent_vars)
  
  test_original_partition <- toString(test_original_partition)
  
  test_prediction <-  toString(test_prediction)

  insert_data = data.frame(sub, modeltype_id, dataset_id, dependent_var, independent_vars, data_partition, test_rmse, test_original_partition, test_prediction)
  
  print(insert_data)
  
  RMySQL::dbWriteTable(dbcon, "savedmodels",
                       insert_data, field.types = c(sub = "VARCHAR(255)", #FOREIGN KEY - DON'T DROP THE TABLE
                                                    modeltype_id = "INTEGER",
                                                    dataset_id = "INTEGER",
                                                    dependent_var = "VARCHAR(255)",
                                                    independent_vars = "TEXT",
                                                    data_partition = "INTEGER",
                                                    test_rmse = "DOUBLE",
                                                    test_original_partition = "LONGTEXT",
                                                    test_prediction = "LONGTEXT",
                                                    uploaded_model = "LONGBLOB"),
                       append = TRUE, 
                       #overwrite = TRUE,
                       row.names = FALSE)

  #uploaded_file_query <- paste0("C:\\Users\\mario\\Desktop\\MBADS-MSc\\Interactive-Web-Application-to-Deploy-Machine-Learning-Models-using-R-Shiny\\xgb.model")

  uploaded_file_query <- paste0("C:\\ProgramData\\MySQL\\MySQL Server 8.0\\Uploads\\xgb.model")

  query <- dbplyr::build_sql("UPDATE savedmodels 
                              SET dependent_var = ",dependent_var,",
                              independent_vars = ",independent_vars,",
                              data_partition = ",data_partition,",
                              test_rmse = ",test_rmse,",
                              test_original_partition = ",test_original_partition,",
                              test_prediction = ",test_prediction,",
                              uploaded_model = LOAD_FILE(",uploaded_file_query,") 
                             WHERE sub = ",sub," AND dataset_id = ",dataset_id, " AND modeltype_id = ",modeltype_id, " AND dependent_var = ",dependent_var," AND independent_vars = ",independent_vars, ";", con = dbcon)

  rs <- DBI::dbSendQuery(dbcon, query)

  DBI::dbClearResult(rs)

  RMySQL::dbDisconnect(dbcon)
  
  shinyjs::show(selector = "a[data-value='ML_models_storage']")

}

getModelFile <-function(sub, dataset_id, modeltype_id, dependent_var){

  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT uploaded_model FROM savedmodels WHERE sub = '",sub, "' 
                  AND dataset_id = '",dataset_id, "' AND modeltype_id = '",modeltype_id,"' AND dependent_var = '",dependent_var,"' into dumpfile 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/xgb.model' ;")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
  
}


getIndependentVarsofModel <-function(sub, dataset_id, modeltype_id, dependent_var){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT independent_vars FROM savedmodels WHERE sub = '",sub, "' 
                  AND dataset_id = '",dataset_id, "' AND modeltype_id = '",modeltype_id,"' AND dependent_var = '",dependent_var,"' ;")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
  
}


getDataPartitionofModel <-function(sub, dataset_id, modeltype_id, dependent_var){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT data_partition FROM savedmodels WHERE sub = '",sub, "' 
                  AND dataset_id = '",dataset_id, "' AND modeltype_id = '",modeltype_id,"' AND dependent_var = '",dependent_var,"' ;")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
  
}



getTestRMSEofModel <-function(sub, dataset_id, modeltype_id, dependent_var){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT test_rmse FROM savedmodels WHERE sub = '",sub, "' 
                  AND dataset_id = '",dataset_id, "' AND modeltype_id = '",modeltype_id,"' AND dependent_var = '",dependent_var,"' ;")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
  
}

getTestOriginalPartition <-function(sub, dataset_id, modeltype_id, dependent_var){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT test_original_partition FROM savedmodels WHERE sub = '",sub, "' 
                  AND dataset_id = '",dataset_id, "' AND modeltype_id = '",modeltype_id,"' AND dependent_var = '",dependent_var,"' ;")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
  
}


getTestPrediction <-function(sub, dataset_id, modeltype_id, dependent_var){
  
  dbcon <- DBI::dbConnect(
    RMySQL::MySQL(),
    dbname = options()$mysql$databaseName,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
  
  query <- paste0("SELECT test_prediction FROM savedmodels WHERE sub = '",sub, "' 
                  AND dataset_id = '",dataset_id, "' AND modeltype_id = '",modeltype_id,"' AND dependent_var = '",dependent_var,"' ;")
  
  request <- DBI::dbGetQuery(dbcon, query)
  
  
  DBI::dbDisconnect(dbcon)
  
  return(request)
  
}
