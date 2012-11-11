# An example of doing some manipulations with metadata

cols <- c("id","doi","title","author","journaltitle","volume","issue","pubdate","pagerange","publisher","type","reviewed.work","unused")

df <- read.csv("citations_combined.csv",strip.white=FALSE,skip=1,col.names=cols,header=FALSE)

fla.df <- subset(df,type=="fla\t")
fla.df$filenames <- paste("wordcounts_",fla.df$id,".CSV",sep="")
fla.df$filenames <- gsub("/","_",fla.df$filenames,fixed=TRUE)

sink("fla_names.txt")
writeLines(fla.df$filenames)
sink()
fla.df$pubdate <- as.numeric(substr(fla.df$pubdate,1,4))

fla.df1960s <- subset(fla.df,pubdate >= 1960 && pubdate < 1970)
