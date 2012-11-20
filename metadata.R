# examples of elementary manipulation using the metadata
# this file is unfinished notes and sketches, not a usable script.

# read.citations: make a dataframe from citations.CSV
# filename: source citations.csv file
# strip: determines whether read.csv will strip leading and trailing
# whitespace from each field. NB jstor puts a trailing tab at the end of each
# entry in fields that can contain multiple tab-separated entries (e.g. author)

read.citations <- function(filename="citations_combined.csv",strip=FALSE) { 
    # Let's hardcode the headers, since read.csv gets confused by
    # the trailing comma at the end of every line

    cols <- c("id","doi","title","author","journaltitle","volume","issue","pubdate","pagerange","publisher","type","reviewed.work","unused") 

    read.csv(filename,strip.white=strip,skip=1,col.names=cols,header=FALSE,as.is=TRUE)
}

# given a dataframe as returned by read.citations,
# return a dataframe with only full length articles 
fla.subset <- function (df) {
    subset(df,type=="fla\t")
}

# convert a jstor doc id to a wordcount filename
#
# implicitly vectorized for a list of id's

as.filename <- function(id) {
    result <- paste("wordcounts_",id,".CSV",sep="")
    gsub("/","_",result,fixed=TRUE)
}

as.id <- function(filename) {
    result <- gsub("^wordcounts_","",filename)
    result <- gsub("\\.CSV$","",result)
    gsub("_","/",result)
}


# make a file listing the names of the wordcount files corresponding
# to the entries in a dataframe, so that you can do something with the
# wordcount files in another program
#
# used for, e.g., making a list of the files corresponding to the fla
# entries returned by fla.subset

write.filenames <- function(df,out.file="filenames.txt") { 
    writeLines(as.filename(df$id),con=out.file)
}

# convert the pubdate field (a string in a date-time format) into numeric
# years
# 
# throws out the month and day and time information 

pubdate.to.years <- function(datestrs) {
    as.numeric(substr(datestrs,1,4))
}

# subset out a range of years from a citations dataframe 
# range is treated as inclusive on the left, exclusive on the right
# example usage:
# fla.df1960s <- datarange.subset(fla.df, 1960, 1970)
daterange.subset <- function(df,min.incl,max.excl) {
    subset(df,pubdate.to.years(pubdate) >= min.incl
        & pubdate.to.years(pubdate) < max.excl)
}

# dump all the metadata in a dataframe into a sqlite database in case
# you need efficient access to that outside of R 
#
# table.name is the name of the database table to write the data frame to
# filename is the name of the sqlite database

make.sqlite <- function (df,filename="citations_combined.db",table.name="document") {
    library(RSQLite)
    db.driver <- dbDriver("SQLite") 
    db.con <- dbConnect(db.driver,dbname=filename)
    dbWriteTable(db.con,table.name,df)
    dbDisconnect(db.con)
    dbUnloadDriver(db.driver)
}
