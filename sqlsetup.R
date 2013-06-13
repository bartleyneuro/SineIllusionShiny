library(RMySQL)

# should use dbWriteTable to create table, then RMySQL methods to open the connection, etc. 

# con <- dbConnect(MySQL(), user="skoons", password="sKOONsstat1t",dbname="skoons", host="mysql2.stat.iastate.edu")
con <- dbConnect(MySQL(), group="stat")


tab <- dbReadTable(con, name="SineIllusionShiny")

# # Re-initialize table...
dbRemoveTable(con, name="SineIllusionShiny")
# load("df.Rdata")
# df <- cd2
# df <- df[,c("allowDataUriScheme", "fingerprint", "userid", "ipid", "output_illusion_height", "output_illusion_width", "pixelratio", "q", "skip", "time", "url_hostname", "url_pathname", "url_port", "url_protocol", "url_search", "weight", "iphash")]
# df$time <- as.POSIXct(df$time)

dbWriteTable(con, name="SineIllusionShiny", value=tab, field.types=list(
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
  iphash = "varchar(35)",
  type = "varchar(3)"), append=TRUE, row.names=FALSE
  )

library(ggplot2)
library(plyr)
library(lubridate)
tab <- dbReadTable(con, "SineIllusionShiny")
tab$time <- ymd_hms(tab$time)
tab2 <- ddply(tab, .(q, fingerprint, ipid), summarise, start=weight[which.min(time)], end=weight[which.max(time)], n=length(time), time.trial = as.numeric(max(time)-min(time)))


qplot(data=tab2, x=start, y=end, geom="jitter")+xlim(c(0,1))+ylim(c(0,1))
qplot(data=tab2, x=end, y=time.trial, geom="jitter")+xlim(c(0,1))
qplot(data=tab2, x=end, y=n, geom="jitter")+xlim(c(0,1))

write.csv(tab, "BetaTest-June10.csv", row.names=FALSE)

dbDisconnect(con)
