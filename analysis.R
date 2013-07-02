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
data <- ddply(tab, .(iphash, fingerprint, ipid, q, skip, type, ntrials), 
              function(df){
                with(df, 
                data.frame(startweight = weight[which.min(time2)], 
                           endweight = weight[which.max(time2)], 
                           modeweight = Mode(weight), 
                           len = length(weight), 
                           unique.weights = length(unique(weight)),
                           time.trial = max(time2)-min(time2)))
              })

tab2 <- ddply(tab, .(iphash, fingerprint, ipid, q, skip, type, ntrials), transform,
              start.weight = rep(weight[which.min(time2)],length(time2)), 
              end.weight = rep(weight[which.max(time2)],length(time2)),
              time.diff = as.numeric(c(0, diff(time2))),
              end.time = rep(min(time2)-max(time2),length(time2)),
              len = rep(length(time2),length(time2)),
              trial.time = as.numeric(time2-max(time2)),
              seq = 1:length(time2))

library(ggplot2)
library(grid)
qplot(data=subset(data, len>1), x=wopts[startweight], xend=wopts[endweight], y=interaction(fingerprint, q+skip), yend=interaction(fingerprint, q+skip), geom="segment", arrow=arrow(length = unit(0.1,"cm")))


write.csv(tab2, "SummaryTable")

tab2 <- read.csv("SummaryTable", row.names=1, stringsAsFactors=FALSE)
tab2$time2 <- ymd_hms(tab2$time2)
qplot(data=subset(tab2, len>2 & seq>1 & ntrials>3 & trial.time>-500), x=trial.time, y=wopts[weight], group=q+skip, geom="line", colour=factor((q+skip)%%6)) + geom_point(aes(x=0, y=wopts[end.weight])) + facet_grid(type~fingerprint, scales="free_x") + xlab("Time until Trial End") + ylab("Weight") + geom_hline(yintercept=1) + geom_hline(yintercept=0)
