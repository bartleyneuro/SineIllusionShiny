library(RMySQL)

# should use dbWriteTable to create table, then RMySQL methods to open the connection, etc. 

con <- dbConnect(MySQL(), user="skoons", password="sKOONsstat1t",dbname="skoons", host="mysql2.stat.iastate.edu")
load("df.Rdata")
df <- cd2
df$time <- as.POSIXct(df$time)

dbRemoveTable(con, name="SineIllusionShiny")

df <- df[,c("allowDataUriScheme", "fingerprint", "userid", "ipid", "output_illusion_height", "output_illusion_width", "pixelratio", "q", "skip", "time", "url_hostname", "url_pathname", "url_port", "url_protocol", "url_search", "weight")]

dbWriteTable(con, name="SineIllusionShiny", value=df, field.types=list(
  allowDataUriScheme = "boolean",
  fingerprint = "varchar(35)",
  userid = "text",
  ipid = "varchar(15)",
  output_illusion_height = "int",
  output_illusion_width = "int",
  pixelratio = "real",
  q = "int",
  skip = "int",
  time = "datetime", 
  url_hostname = "text",
  url_pathname = "varchar(50)",
  url_port = "varchar(5)",
  url_protocol = "varchar(8)",
  url_search = "text",
  weight = "real"), append=TRUE, row.names=FALSE
  )
dbReadTable(con, "SineIllusionShiny")


dbDisconnect(con)
