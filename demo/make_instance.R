

prune_filelist <- function(files,metadata,aquo,adquem,types="fla\t",
                           exclude=function (metadata) { NULL }) {
    # apply cutoff dates
    keep_ids <- metadata$id[metadata$date >= as.Date(aquo) &
                            metadata$date <= as.Date(adquem) &
                            metadata$type %in% types]
    # apply further exclusions
    keep_ids <- keep_ids[!keep_ids %in% exclude(metadata)]
    files[as.id(files) %in% keep_ids]
}


exclude_shorts <- function(metadata,document_lengths,threshold) {
    document_lengths$id[document_lengths$length < threshold]
}

get_counts <- function(dirs,aquo,adquem,itemtypes,exclude) {

    message("Loading metadata...")

    metadata <- read_metadata(file.path(dirs,"citations.CSV"))
    metadata$date <- pubdate_Date(metadata$pubdate)

    message("Read ",nrow(metadata)," metadata entries")

    globs <- file.path(dirs,"wordcounts","wordcounts*.CSV")
    files <- prune_filelist(files=Sys.glob(globs),
                            metadata=metadata,
                            aquo=aquo,adquem=adquem,
                            types=itemtypes,
                            exclude=exclude)

    message("Importing ",length(files)," wordcount.CSV files")

    read_dfr(files=files)
}
 
read_britticisms <- function(filepath) {
    aframe <- read.csv(filepath, stringsAsFactors = FALSE)
    trans_table <- aframe$AMERICAN
    names(trans_table) <- aframe$BRITISH
    trans_table
}

# translate_britticisms
#
# Accepts a long-form dataframe as returned by read_dfr with
# three columns, id, WORDCOUNTS, and WEIGHT.
#
# Also accepts a named vector used as a translation table,
# where the contents of the vector are American spellings
# and the names are British spellings. Using this, translates
# British items in WORDCOUNTS into corresponding American
# spellings. Does not attempt to merge duplicate rows of the
# data frame which may be created (e.g. when colour -> color,
# you may end up with two rows for "color.") Assumes that those
# rows will effectively be merged when wordcounts are inflated
# into actual text for Mallet by docs_frame or a similar function.
#
# Probably not very fast because named vectors in R are not
# implemented as hash tables like Python dictionaries. But it's
# not a bottleneck in practice.

translate_britticisms <- function(counts, american_translations) {
    british_spellings <- names(american_translations)
    brits <- which(counts$WORDCOUNTS %in% british_spellings) 
    counts$WORDCOUNTS[brits] <- american_translations[counts$WORDCOUNTS[brits]]
    counts
}

rare_token_report <- function(overall,
                              freq_threshold=NULL,rank_threshold=NULL,
                              plotsfile="freqplots.png") {

    total <- sum(overall)
    ovf <- as.data.frame(overall)
    if(!is.null(freq_threshold)) { 
        count_threshold <- freq_threshold * total
    }
    else {
        if(is.null(rank_threshold)) {
            stop("No threshold supplied")
        }

        count_threshold <- sort(overall,decreasing=T)[rank_threshold]
    }
            
    ovf$keep <- ovf$Freq >= count_threshold
    
    message("Constructing plots...")

    png(plotsfile,width=800,height=600)
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(1,2)))
  
    freqdist <- qplot(Freq / total,data=ovf,geom="bar",log="x",fill=keep,
                      xlab="frequency",
                      ylab="number of word types",
                      main="Types") +
        theme(legend.position="none")
    
    tokdist <- freqdist + geom_bar(aes(weight=Freq)) +
        ylab("token count") + ggtitle("Tokens")

    print(freqdist,vp=viewport(layout.pos.row=1,layout.pos.col=1))
    print(tokdist,vp=viewport(layout.pos.row=1,layout.pos.col=2))
    dev.off()
    
    message("Plots saved to ",plotsfile)
    
    types_frac <- 1 - ecdf(overall)(count_threshold)
    types_total <- length(overall)
    types_msg <- sprintf("%.0f of %.0f types (%.3f)",
                         types_frac * types_total,types_total,types_frac)
    
    tokens_count <- sum(overall[overall >= count_threshold])
    tokens_msg <- sprintf("%.0f of %.0f tokens (%.3f)",
                         tokens_count,total,tokens_count / total)
    message("A frequency threshold of ",count_threshold / total,
            " or > ",floor(count_threshold)," tokens\n",
            "leaves ",types_msg," and ",tokens_msg)
    
    freqdist
}

stopword_report <- function(overall,stoplist_file) {
    stopwords <- scan(stoplist_file,what=character(),sep="\n",quiet=T)
    stopwords <- unique(stopwords)

    total <- sum(overall)
    stopcount <- sum(overall[stopwords],na.rm=T)

    message("The ",length(stopwords)," unique stopwords from ",
            stoplist_file,"\n",
            "correspond to ",stopcount, " of ",total," tokens (",
            sprintf("%.3f",stopcount / total),") in the corpus")
}

# make_instance: main function

make_instance <- function(
        outdir="instances",
        dfr_analysis_root="~/Developer/dfr-analysis",
        project_root=".",
        dfr_data_root=project_root,
        journal_dirs="pmla_all",
        aquo=as.Date("1880-01-01"),
        adquem=as.Date("2013-12-31"),
        itemtypes="fla\t",
        length_min=1000,                                # words
        lengths_outfile=file.path(outdir,"document_lengths.csv"),
        britticisms_file=file.path(project_root,"uk_us.csv"),
        freq_threshold=NULL,
        rank_threshold=10000, 
        plotfile=file.path(outdir,"freqplots.png"),
        outfile=file.path(outdir,"docs.mallet"),
        java_heap="2g") {

    # "includes"
    pwd <- getwd()
    library(ggplot2)
    library(grid)

    setwd(dfr_analysis_root)
    source("topics_rmallet.R")
    topics_rmallet_setup(java_heap)
    setwd(pwd)


    # main script: commands

    dfr_dirs <- file.path(dfr_data_root,journal_dirs)

    pwd <- getwd()
    setwd(project_root)
    message("wd now: ",project_root)

    message("regenerating stoplist_final.txt") 
    system("python stoplist_final.py",ignore.stdout=T,ignore.stderr=T)

    stoplist_file <- file.path(project_root,"stoplist_final.txt")

    if(!file.exists(outdir)) {
      dir.create(outdir)
    }

    if(length_min > 0) {
        wc_dirs <- file.path(dfr_dirs,"wordcounts")
        pythonscript <- file.path(project_root,"document_lengths.py")
        cmd <- paste("python",pythonscript,paste(wc_dirs,collapse=" "))
        cmd <- paste(cmd,">",lengths_outfile)
        message("Running ",cmd)
        system(cmd)
        lengths <- read.csv(lengths_outfile,as.is=T)
        lengths$id <- as.id(lengths$filename)

        excluder <- function(metadata) {
            c(exclude(metadata),exclude_shorts(metadata,lengths,length_min))
        }
    }
    else {
        excluder <- exclude
    }

    setwd(pwd)
    message("wd now: ",pwd)

    counts <- get_counts(dfr_dirs,
                         aquo,
                         adquem,
                         itemtypes,
                         exclude=excluder)

    message("Read ",nrow(counts)," rows") 

    message("'Normalizing' British spellings to American...")

    britticisms <- read_britticisms(britticisms_file)
    counts <- translate_britticisms(counts,britticisms)

    message("Aggregating token counts into overall counts")

    overall <- overall_counts(counts) 
    rare_token_report(overall,freq_threshold,rank_threshold,plotfile) 
    stopword_report(overall,stoplist_file)

    message("Removing infrequent word types...")

    counts <- remove_rare(counts,freq_threshold,rank_threshold,
                          .overall=overall)

    message(nrow(counts)," rows remain.") 
    message("Making MALLET instance...") 
    inst <- make_instances(docs_frame(counts),stoplist_file)

    write_instances(inst,outfile) 
    message("Instance saved to ",outfile)

}
