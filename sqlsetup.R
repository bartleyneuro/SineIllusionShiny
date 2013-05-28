library(RMySQL)

# should use dbWriteTable to create table, then RMySQL methods to open the connection, etc. 

con <- dbConnect(MySQL(), user="skoons", password="sKOONsstat1t",dbname="skoons", host="mysql2.stat")



dbWriteTable(con, name="SineIllusionShiny", value=df, field.types=list(
  allowDataUriScheme = "boolean",
  fingerprint = "varchar(35)",
  userid = "text",
  ipid = "varchar(15)",
  output_illusion_height = "int",
  output_illusion_hidden = "boolean",
  output_illusion_width = "int",
  output_questionCounter_hidden = "boolean",
  output_weightControl_hidden = "boolean",
  pixelratio = "real",
  q = "int",
  systime = "timestamp", 
  url_hostname = "text",
  url_pathname = "varchar(20)",
  url_port = "varchar(10)",
  url_protocol = "varchar(10)",
  url_search = "text",
  weight = "real",
  xy = "varchar(1)")
  )