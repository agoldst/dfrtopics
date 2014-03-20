write_zip <- function(writer,file_base,file_ext=".json",no_zip=F) {
    if(no_zip) {
        f_out <- str_c(file_base,file_ext)
        writer(f_out)
    }
    else {
        f_temp <- file.path(tempdir(),str_c(basename(file_base),file_ext))
        writer(f_temp)
        f_out <- str_c(file_base,file_ext,".zip")
        if(file.exists(f_out)) {
            message("Removing existing ",f_out)
            unlink(f_out)
        }
        zip(f_out,f_temp,flags="-9Xj")
        unlink(f_temp)
    }
    message("Saved ",f_out)
}

#' Output data files for dfr-browser
#'
#' Transform modeling results into a format suitable for use by
#' \url{http://agoldst.github.io/dfr-browser}{dfr-browser}, the
#' web-browser based model browser. Supply either data frames of the
#' kind returned by \code{\link{model_documents}} or the names of files
#' of the kind saved by \code{\link{output_model}}. 
#'
#' This routine reports on its progress. 
#'
#' @param out_dir directory for output data files
#' @param metadata either a dataframe with metadata from 
#' \code{\link{read_metadata}} or the name of file with the DfR metadata
#' @param keys either the result of \code{\link{weighted_keys_frame}} or the 
#' name of a CSV file.
#' @param doc_topics either dataframe with documents in rows, topic weights in 
#' columns, and a final \\code{id} column with JSTOR document id's, or the name 
#' of a CSV file.
#' @param topic_scaled either a dataframe with 2D coordinates for each topic
#' or the name of a file with such coordinates. Calculate from the topic-word 
#' matrix using \code{\link{topic_scaled_2d}}
#' @param zipped should data files be zipped? dfr-browser supports client-side 
#' unzipping 
#'
#' @examples
#'
#' \dontrun{
#' m <- model_documents("citations.CSV","wordcounts","stoplist.txt",50)
#' export_browser_data("data",
#'      m$metadata,
#'      m$wkf,
#'      m$doc_topics,
#'      topic_scaled_2d(m$trainer))
#' }
#'      
#' @export
#'
#' @seealso
#' \code{\link{model_documents}}
#' \code{\link{topic_scaled_2d}}
export_browser_data <- function(
        out_dir="data",
        metadata=file.path(out_dir,"citations.CSV"),
        keys=file.path(out_dir,"keys.csv"),
        doc_topics=file.path(out_dir,"doc_topics.csv"),
        topic_scaled=file.path(out_dir,"topic_scaled.csv"),
        zipped=T) {
                         
    if(!file.exists(out_dir)) {
        dir.create(out_dir)
    }

    if(is.character(keys)) {
        if(!file.exists(keys)) {
            warning("Keys file '",keys,"' not found.")
        } else {
            keys_frame <- read.csv(keys,as.is=T)
        }
    } else {
        keys_frame <- keys
    }

    if(is.data.frame(keys_frame)) {
        tw <- ddply(keys_frame,.(topic),
                    summarize,
                    words=str_c(word[order(weight,decreasing=T)],
                                collapse='","'),
                    weights=str_c(sort(weight,decreasing=T),collapse=","),
                    alpha=unique(alpha))
        tw$words <- str_c('"',tw$words,'"')
        tw <- tw[order(tw$topic),]
        json <- str_c('{"alpha":[',
                            str_c(tw$alpha,collapse=","),
                            '],"tw":[')

        json <- str_c(json,
                      str_c('{"words":[',
                            tw$words,
                            '],"weights":[',
                            tw$weights,
                            ']}',
                            collapse=","),
                      ']}')

        tw_file <- file.path(out_dir,"tw.json")
        writeLines(json,tw_file)
        message("Wrote ",tw_file)
    }
    else {
        warning("Unable to create topic-words file.")
    }

    if(is.character(doc_topics)) {
        if(!file.exists(doc_topics)) {
            warning("Doc-topics file '",doc_topics,"' not found.")
        } else {
            dt_frame <- read.csv(doc_topics,as.is=T)
        }
    } else {
        dt_frame <- doc_topics
    }

    if(is.data.frame(dt_frame)) {

        ids <- dt_frame$id
        dtm <- Matrix(doc_topics_matrix(dt_frame),sparse=T)

        # could compress much more aggressively considering that weights are 
        # integers, so could be stored as binary data rather than ASCII

        json <- str_c('{"i":[',
                      str_c(dtm@i,collapse=","),
                      '],"p":[',
                      str_c(dtm@p,collapse=","),
                      '],"x":[',
                      str_c(dtm@x,collapse=","),
                      ']}')
        write_zip(function (f) { writeLines(json,f) },
                  file.path(out_dir,"dt"),".json",no_zip=!zipped)

        # write out per-doc token totals (lengths excluding stopwords)
        json <- str_c('{"doc_len":[',
                      str_c(Matrix::rowSums(dtm),collapse=","),
                      ']}')
        write_zip(function (f) { writeLines(json,f) },
                  file.path(out_dir,"doc_len"),".json",no_zip=!zipped)
    }
    else {
        warning("Unable to write doc-topics file.");
    }

    if(is.character(metadata)) {
        if(!file.exists(metadata)) {
            warning("Metadata file '",metadata,"' not found.")
        } else {
            md_frame <- read_metadata(metadata)
        }
    } else {
        md_frame <- metadata
    }

    if(is.data.frame(md_frame) && length(md_frame) > 0
       && nrow(md_frame) > 0 && exists("ids")) {
        i_md <- match(ids,md_frame$id)
        md_frame <- md_frame[i_md,]

        # throw out unneeded columns [doi === id]
        drops <- match(c("publisher","reviewed.work","doi"),names(md_frame))
        md_frame <- md_frame[,-drops]

        write_zip(function (f) {
            write.table(md_frame,f,
                        quote=T,sep=",",
                        col.names=F,row.names=F,
                        # d3.csv.* expects RFC 4180 compliance
                        qmethod="double")},
                    file.path(out_dir,"meta"),
                    ".csv",no_zip=!zipped)
    }
    else {
        warning("Unable to write metadata.")
    }

    if(is.character(topic_scaled)) {
        if(!file.exists(topic_scaled)) {
            warning("Scaled coordinates file '",topic_scaled,"' not found.")
        } else {
            scaled_coords <- read.csv(topic_scaled,header=F)
        }
    } else {
        scaled_coords <- topic_scaled
    }

    if(is.data.frame(scaled_coords) || is.matrix(scaled_coords)) {
        write.table(scaled_coords,file.path(out_dir,"topic_scaled.csv"),
                    quote=F,sep=",",row.names=F,col.names=F)
    } else {
        message("Unable to write scaled topic coordinates.")
    }

    message("Checking for info.json file...")
    info_file <- file.path(out_dir,"info.json")
    if(file.exists(info_file)) {
        message(info_file," ok")
    }
    else {
        writeLines(
'{
    "title": "",
    "meta_info": "<h2><\\/h2>",
    "VIS": { "prefab_plots": false } 
}'
            ,info_file)
        message(info_file," was missing. A stub file has been created.")
    }

}
