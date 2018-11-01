path = '~/PROJECTS/number-line/num-density.2013-06/data/'
files = dir(path)

for(f in files){
  filename = paste0(path,f)
  q = read.table(file=filename, skip=1, sep="\t", header=TRUE)
  
}

