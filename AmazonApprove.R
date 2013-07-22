library(RMySQL)
con <- dbConnect(MySQL(), group="stat")
tab <- dbReadTable(con, name="SineIllusionShiny")[-1,]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

diffs <- c(0, .5, .5, .5, .5, .25, .25, .25, .25, .25, .25, .25, .25, .1, .1, .05, .05, .05, .05, .05, .05, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .05, .05, .05, .05, .1, .1, .1, .1, .1, .25, .25, .25, .25, .25, .25, .5, .5, .5, .5)
wopts <- -4 +cumsum(diffs)

library(plyr)
library(lubridate)
tab$time2 <- ymd_hms(tab$time)
tab <- ddply(tab, .(iphash, fingerprint, ipid), transform, ntrials=length(unique(q+skip)))
tab <- tab[which(tab$weight>0),]
tab$userid <- gsub("^ ", "", gsub(" $", "", tab$userid)) # strip leading/trailing spaces.

amazonuseridx <- which(nchar(tab$userid)>=12)

amazonusers <- ddply(tab[amazonuseridx,], .(userid, fingerprint, ipid, iphash), summarise, ntrials=max(q)+1, skips = max(skip))
amazonusers$userid <- gsub("^ ", "", gsub(" $", "", amazonusers$userid))


amazonusers$approve <- c("no", "yes")[1+as.numeric(amazonusers$ntrials>=1)]
ips <- ddply(tab[amazonuseridx,], .(iphash, fingerprint), summarise, userid=unique(userid))

qplot(data=subset(tab, userid=="A2LV5QYHH24OMY"), x=time2, y=weight)