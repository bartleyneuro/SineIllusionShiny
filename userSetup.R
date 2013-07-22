# Setup data at the beginning of the trial.

diffs <- c(0, .5, .5, .5, .5, .25, .25, .25, .25, .25, .25, .25, .25, .1, .1, .05, .05, .05, .05, .05, .05, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .05, .05, .05, .05, .1, .1, .1, .1, .1, .25, .25, .25, .25, .25, .25, .5, .5, .5, .5)
wopts <- -4 +cumsum(diffs)

# make list of all combinations of w, type, with corresponding index to reference list of dataframes.
dframelist <- expand.grid(z=1:length(wopts), type=c("x", "y"), stringsAsFactors=FALSE)
dframelist$idx <- 1:nrow(dframelist)
dframelist$w <- wopts[dframelist$z]

reqtrials <- 10
trialsreq <- expand.grid(type=c("x", "y"), z=c(13, 13, sample(16:26, ceiling(reqtrials/2)-2), 37))
# sample the middle weight option, then 0, 1. Combine with each combination of x, y.
trialsreq <- trialsreq[c(1:2, sample(3:(reqtrials+2), reqtrials)),] # shuffle

trialsextra <- expand.grid(z=13:31, type=c("x", "y"))
trialsextra <- trialsextra[sample(1:(length(trialsextra$z)), length(trialsextra$z)),]
# leave repeats in to get a measure of reliability
trials <- rbind(trialsreq, trialsextra)
trials$order <- 1:nrow(trials)
trials$w <- wopts[trials$z]
trials$w <- ceiling(trials$w/.05)*.05
trials <- merge(trials, dframelist)
trials <- trials[order(trials$order),-4]

maxtrials <- nrow(trials)

dframe <- do.call("rbind", lapply(dframelist$idx, function(i) getData.wtype(dframelist$w[i], dframelist$type[i])))
dframe <- merge(dframe, dframelist, sort=FALSE)
