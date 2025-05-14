# --- db_connect.R ---

# Load environment variables from ~/.Renviron
readRenviron("~/.Renviron")

# Retrieve connection details from environment variables
DATABASE<-paste(Sys.getenv("DB_HOST"),"/", Sys.getenv("DB_NAME"),sep='')
DB_USER<-Sys.getenv("DB_USERNAME")
DB_PASSWORD<-Sys.getenv("DB_PASSWORD")
DB_PORT<-Sys.getenv("DB_PORT")

library(DatabaseConnector)

# Define connection details using environment variables
connectionDetails <- createConnectionDetails(
  dbms = "postgresql", 
  server = DATABASE, 
  user = DB_USER, 
  password = DB_PASSWORD, 
  port = DB_PORT)

# Function for connecting to the database
connect_db <- function() {
  conn <- DatabaseConnector::connect(connectionDetails)
  # Set schema (adjust if needed for your local setup)
  dbExecute(conn, "SET search_path TO ohdsi_cdm_202207")
  return(conn)
}

# Function for disconnecting from the database
disconnect_db <- function(connection) {
  DatabaseConnector::disconnect(connection)
}

