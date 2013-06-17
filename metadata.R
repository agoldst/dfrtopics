# examples of elementary manipulation using the metadata

# read_metadata: make a dataframe from citations.CSV files
#
# filenames: vector of citations.CSV filenames
#
# assumes that each file has a trailing comma at the end of every line,
# which makes read.csv find an extra field. DfR has changed their output
# data format before, so check results carefully.
#
# Any extra parameters are passed on to read.csv; the most useful may
# be strip.white, which determines whether read.csv will strip leading
# and trailing whitespace from each field. NB jstor puts a trailing
# tab at the end of each entry in fields that can contain multiple
# tab-separated entries (e.g. author)--even if there is only a single
# entry

read_metadata <- function(filenames,...) {
    all_rows <- do.call(rbind,lapply(filenames,read_citations,...))
    # deduplicate
    result <- unique(all_rows)

    if(any(duplicated(result$id))) {
        warning("Some rows have the same id")
    }

    result
}

# read_citations: this does the work for the above.
#
# Reads a single citations.CSV file. Opens file dialog if filename is NA.
#
# See above re: trailing commas in dfr citations.CSV files.

read_citations <- function(filename=NA,...) { 
    f <- filename
    if(is.na(filename)) { 
        cat("Select citations.CSV file from jstor dfr...\n")
        ignore <- readline("(press return to open file dialog) ")
        f <- file.choose()
        print(f)
    }

    # the nefarious trailing comma:
    cols <- scan(f,nlines=1,what=character(),sep=",",quiet=T)
    cols <- c(cols,"unused")

    subset(read.csv(f,skip=1,header=F,col.names=cols,quote="",as.is=T,...),
           select=-unused)
}

    
# legacy function: now just a wrapper around read_metadata

read.citations <- function(filename=NA,strip=FALSE) { read_metadata(filename,strip.white=strip) }

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

# as.id can be more generic
# it expects the file (possibly including a path) to end with
# wordcounts_xxxxxxxxx.XYZ
# where xxxxxx are a jstor id, but with / replaced with _
# and the file extension XYZ is alphabetic
as.id <- function(filename) {
  result <- sub("^.*wordcounts_","",filename)
  result <- sub("\\.[[:alpha:]]*$","",result)
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

# to keep month and day information, use this instead, which returns
# Date object(s)
pubdate_Date <- function(pubdate) {
    as.Date(substr(pubdate,1,10))
}

# convert a DFR id into a jstor url
#
# FIXME this works often, but not always

dfr_id_url <- function(id) {
    sub("^.*\\/","http://www.jstor.org/stable/",id)
}
                    

# dump all the metadata in a dataframe into a sqlite database in case
# you need efficient access to that outside of R 
#
# table.name is the name of the database table to write the data frame to
# filename is the name of the sqlite database

write.sqlite <- function (df,filename=file.choose(),table.name="document") {
    library(RSQLite)
    db.driver <- dbDriver("SQLite") 
    db.con <- dbConnect(db.driver,dbname=filename)
    dbWriteTable(db.con,table.name,df)
    dbDisconnect(db.con)
    dbUnloadDriver(db.driver)
}

read.sqlite <- function(database.filename=file.choose(),
                        table.name="document") { 
    library(RSQLite)
    db.driver <- dbDriver("SQLite") 
    db.con <- dbConnect(db.driver,dbname=database.filename)
    result <- dbReadTable(db.con,table.name)

    # Database cleanup

    dbDisconnect(db.con)
    dbUnloadDriver(db.driver)

    result
}

# --------
# plots, descriptive output, etc.
# --------

# return a list of strings citing the articles identified by ids
# or citing all articles in tm if no ids supplied
cite.articles <- function(tm,ids=NA)  {
    df <- tm
    if(!is.na(ids)) {
        df <- tm[tm$id %in% ids,] 
    }
    result <- paste(df$author,", \"",df$title,",\" ",df$journaltitle," ",df$volume,
          " no. ",df$issue," (",df$pubdate,"): ",df$pagerange,sep="")
    result <- gsub("_",",",result)
    gsub("\t","",result)
}

# How many of each item type appear in each temporal interval?
#
# not a very fancy plot

plot_items_by_year <- function(metadata,time_interval="year") {
    to.plot <- transform(metadata,
                         Date=cut(pubdate_Date(pubdate),
                                  breaks=time_interval))

    # TODO proper date-interval breaks with cut.Date()
    qplot(as.Date(Date),
          data=to.plot,geom="histogram",
          facets = ~ type) +
        xlab("publication date") +
        ggtitle("Number of each item type, by year")
}

# view_on_jstor
#
# Take an item id, open it in the web browser (using MacOS X "open").
# Relies on dfr_id_url() above
#
# FIXME N.B. this doesn't always work

view_on_jstor <- function(id) {
    cmd <- paste("open",dfr_id_url(id))
    system(cmd)
}
