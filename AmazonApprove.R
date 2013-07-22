library(RMySQL)
con <- dbConnect(MySQL(), group="stat")
tab <- dbReadTable(con, name="SineIllusionShiny")[-1,]
dbDisconnect(con)

library(plyr)
library(lubridate)
tab$time2 <- ymd_hms(tab$time)
tab <- ddply(tab, .(iphash, fingerprint, ipid), transform, ntrials=length(unique(q+skip)))
tab <- tab[which(tab$weight>0),]
tab$userid <- gsub("^ ", "", gsub(" $", "", tab$userid)) # strip leading/trailing spaces.

amazonuseridx <- which(nchar(tab$userid)>=12)

amazonusers <- ddply(tab[amazonuseridx,], .(userid, fingerprint, ipid, iphash), summarise, ntrials=max(q)+1, skips = max(skip))

amazonusers$approve <- c("no", "yes")[1+as.numeric(amazonusers$ntrials>=12)]
amazonusers[,c(1, 5, 6, 7)]
ips <- ddply(tab[amazonuseridx,], .(iphash, fingerprint), summarise, userid=unique(userid))
