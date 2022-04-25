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


# https://www.digitalocean.com/community/questions/how-to-enable-local-capability-in-mysql-workbench
# SHOW GLOBAL VARIABLES LIKE 'local_infile';
# SET GLOBAL local_infile = true;


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