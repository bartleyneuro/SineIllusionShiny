library(RMySQL)

con <- dbConnect(MySQL(), group="stat")
tab <- dbReadTable(con, name="SineIllusionShiny")[-1,]
dbDisconnect(con)
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
data <- ddply(tab, .(iphash, fingerprint, ipid, q, skip, type, ntrials), 
              function(df){
                with(df, 
                data.frame(startweight = wopts[weight[which.min(time2)]], 
                           endweight =  wopts[weight[which.max(time2)]], 
                           modeweight =  wopts[Mode(weight)], 
                           len = length(weight), 
                           unique.weights = length(unique(weight)),
                           time.trial = max(time2)-min(time2)))
              })

tab2 <- ddply(tab, .(iphash, fingerprint, ipid, q, skip, type, ntrials), transform,
              weight = wopts[weight],
              start.weight = wopts[rep(weight[which.min(time2)],length(time2))], 
              end.weight = wopts[rep(weight[which.max(time2)],length(time2))],
              time.diff = as.numeric(c(0, diff(time2))),
              end.time = rep(min(time2)-max(time2),length(time2)),
              len = rep(length(time2),length(time2)),
              trial.time = as.numeric(time2-max(time2)),
              seq = 1:length(time2))

library(ggplot2)
library(grid)
qplot(data=subset(data, len>1), 
      x=startweight, xend=endweight, 
      y=fingerprint, yend=fingerprint, geom="segment", 
      arrow=arrow(length = unit(0.1,"cm")), group=q+skip, alpha=I(.2)) + 
  geom_vline(aes(xintercept=0), linetype=2) +
  geom_vline(aes(xintercept=1), linetype=2) +
  facet_wrap(~type)


write.csv(tab2[,-which(names(tab2)=="userid")], "IndivTrajectory.csv")
write.csv(data, "SummaryTable.csv")

data <- read.csv("SummaryTable.csv", row.names=1, stringsAsFactors=FALSE)
tab2 <- read.csv("IndivTrajectory.csv", row.names=1, stringsAsFactors=FALSE)

tab2$time2 <- ymd_hms(tab2$time2)
qplot(data=subset(tab2, len>2 & seq>1 & ntrials>3 & trial.time>-500), x=trial.time, y=weight, group=q+skip, geom="line", colour=factor((q+skip)%%6)) + geom_point(aes(x=0, y=end.weight)) + facet_grid(type~fingerprint, scales="free_x") + xlab("Time until Trial End") + ylab("Weight") + geom_hline(yintercept=1) + geom_hline(yintercept=0)

data$fingerid <- as.numeric(factor(data$fingerprint))
tab2$fingerid <- as.numeric(factor(tab2$fingerprint))
ggplot() + 
  geom_point(data=data, aes(x=0, y=endweight), alpha=.05) + 
  geom_rug(data=data, aes(y=endweight), alpha=.05, sides="r") +
  geom_line(data=subset(tab2, len>2 & seq>1 & ntrials>3 & trial.time>-100),
            aes(x=trial.time, y=weight, group=interaction(q+skip, fingerprint)), alpha=.1) + 
  facet_grid(type~., scales="free_x") + 
  xlab("Time until Trial End") + 
  ylab("Weight") + 
  geom_hline(yintercept=1) + 
  geom_hline(yintercept=0) +
  xlim(c(-25, 1)) + 
  ylim(c(-1, 2))


data$startweight.cat <- factor(
  sapply(data$startweight, function(i) sum(i<=quantile(data$startweight, seq(.2, 1, .2)))),
  labels=paste(quantile(data$startweight, seq(0, .8, .2)), quantile(data$startweight, seq(.2, 1, .2)), sep=" - "))

ggplot() + geom_density(data=data, aes(x=endweight, group=startweight.cat,
                                       colour=startweight.cat, fill=startweight.cat), alpha=I(.2)) + 
  geom_rug(data=data, aes(x=endweight), alpha=I(.1)) + 
  facet_grid(startweight.cat~type) + scale_fill_discrete("Starting Weight") + 
  scale_colour_discrete("Starting Weight") + 
  xlab("Final Weight") + ylab("Density") + ggtitle("Density of Final Weight") + xlim(c(-.5, 1.5))

ggplot() + geom_text(data=data, aes(x=startweight, y=endweight, label=as.numeric(factor(fingerprint))), alpha=.1) + facet_wrap(~type) + xlim(c(-.25, 1.25)) + ylim(c(-.25, 1.25))

library(rjson)

freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}   

geodata <- freegeoip(unique(tab2$ipid))