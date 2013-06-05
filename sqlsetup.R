library(RMySQL)

# should use dbWriteTable to create table, then RMySQL methods to open the connection, etc. 

# con <- dbConnect(MySQL(), user="skoons", password="sKOONsstat1t",dbname="skoons", host="mysql2.stat.iastate.edu")
con <- dbConnect(MySQL(), group="stat")


tab <- dbReadTable(con, name="SineIllusionShiny")

# # Re-initialize table...
# dbRemoveTable(con, name="SineIllusionShiny")
# load("df.Rdata")
# df <- cd2
# df <- df[,c("allowDataUriScheme", "fingerprint", "userid", "ipid", "output_illusion_height", "output_illusion_width", "pixelratio", "q", "skip", "time", "url_hostname", "url_pathname", "url_port", "url_protocol", "url_search", "weight", "iphash")]
# df$time <- as.POSIXct(df$time)

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
  url_pathname = "varchar(20)",
  url_port = "varchar(10)",
  url_protocol = "varchar(10)",
  url_search = "text",
  weight = "real",
  iphash = "varchar(35)"), append=TRUE, row.names=FALSE
  )
dbReadTable(con, "SineIllusionShiny")


dbDisconnect(con)
