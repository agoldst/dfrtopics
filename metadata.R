#  elementary manipulation of metadata with R

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

# Subsetting of bag of words file:
# use file, readLines, etc., cf metadata.R

# TODO NEED THIS AT ALL?
# fast way to dump all the metadata into a sqlite database
# parameter: df, a data frame

make.sqlite <- function (df) {
    library(RSQLite)
    db.driver <- dbDriver("SQLite") 
    db.con <- dbConnect(db.driver,dbname="citations_combined.db")
    dbWriteTable(db.con,"document",df)
    dbDisconnect(db.con)
    dbUnloadDriver(db.driver)
}
