setwd('~/PROJECTS/number-line/zenith/R/')
source('~/CODE/R/utilities.R')
source('~/PROJECTS/number-line/numerline-utilities.R')

if(RECALC){  # read data in from scratch only if READ==T
  ## load all data and clean up format.
  years = c('2011', '2012', '2013')
  dat <- data.frame()
  for(year in years){
    ypath = sprintf('../%s/csv/', year)
    files <- list.files(ypath)
    for(f in files){
      q = read.csv2(sprintf('%s%s', ypath, f), sep=",", header=T, colClasses="character")
      nt = nrow(q)-5
      q$trial = c(rep(0,5), 1:nt)
      dat <- rbind(dat, q)
    }
  }
  
  tonum <- function(x){as.numeric(as.character(x))}
  dat$rt1 <- tonum(dat$rt1)
  dat$rt2 <- tonum(dat$rt2)
  dat$stimulus <- tonum(dat$stimulus)
  dat$response <- tonum(dat$response)
  dat$trial <- tonum(dat$trial)
  
  save(file='alldata.Rdat', 'dat')
} else {
  load(file='alldata.Rdat')
}

# clean data


maincol = rgb(0, 0.7, 0)
seccol = rgb(1, 0, 0)

maxn = max(dat$stimulus)

