# Selected visualizations of the topic model and the corpus, with an emphasis on
# seeing how the counts are distributed over document dates.

#' Overview visualization of multiple topics
#'
#' This function is intended for a quick survey of a topic model. It generates a
#' folder of image files, one for each topic. Each file has two plots, the plot
#' of the most heavily-weighted words in the topic and the plot of the topic's
#' relative weight in the corpus over time. The plots are saved to
#' \code{output_dir/<NNN>.png}. If you have more than 1000 topics, other parts
#' of this package will probably break too.
#'
#' @param m \code{dfr_lda} object
#' @param output_dir directory to save plots to
#' @param topic topics to generate plots for (by default, all)
#' @param breaks time intervals by which to slice document-topic distributions
#'   for the box plots: passed on to \code{\link{plot_topic_series}}.
#' @param w PNG width (pixels)
#' @param h PNG height (pixels)
#'
#' @seealso \code{\link{plot_top_words}}, \code{\link{plot_topic_series}},
#' \code{\link{export_browser_data}} and the
#' \href{http://agoldst.github.io/dfr-browser}{dfr-browser} web-browser-based
#' topic model explorer
#'
#' @export
#'
topic_report <- function (m,
                          output_dir="topic_report",
                          topics=1:n_topics(m),
                          breaks="years",
                          w=1200, h=800) {
    if (!requireNamespace("ggplot2", quietly=T)) {
        stop("Plotting functions require the ggplot2 package.")
    }

    if (!file.exists(output_dir)) {
        dir.create(output_dir)
    }

    m <- load_top_words(m, 20)
    series <- topic_series(m, breaks)

    for (topic in topics) {
        filename <- file.path(output_dir,
                              sprintf("%03d.png", topic))
        png(file=filename, width=w, height=h)
        message("Saving ", filename)
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout=grid::grid.layout(1, 2)))

        print(plot_top_words(top_words(m, 20), topic),
              vp=grid::viewport(layout.pos.row=1, layout.pos.col=1))

        print(plot_series(series[series$topic == topic, ]),
              vp=grid::viewport(layout.pos.row=1, layout.pos.col=2))

        dev.off()
    }
}

#' Plot a single topic's most probable words
#'
#' Creates a simple plot visualizing word weights in a topic as horizontal bars.
#' The weightiest words appear at the top of the plot.
#'
#' @param frm a data frame of topic top words, from \code{\link{top_words}}
#' @param topic topic number
#' @param n number of words to show on plot. If NULL (by default), use all the
#'   words supplied in \code{frm}
#' @return a \link[ggplot2]{ggplot} object
#'
#' @seealso \code{\link{top_words}}
#'
#' @examples
#' \dontrun{
#' top_words(m, n=10) %>%
#'      plot_top_words(m, topic=2)
#'
#' # or with a different scoring scheme:
#' top_words (m, n=10, weighting=tw_blei_lafferty(m) %>%
#'      plot_top_words(m, topic=3)
#' }
#'
#' @export
#'
plot_top_words <- function (frm, topic, n=NULL) {
    if (!requireNamespace("ggplot2", quietly=T)) {
        stop("Plotting functions require the ggplot2 package.")
    }

    if (length(topic) > 1) {
        stop("This function only plots a single topic at once")
    }

    keys <- frm[frm$topic == topic, ]
    if (!is.null(n)) {
        keys <- frm[1:n, ]
    }


    ggplot2::ggplot(keys,
            ggplot2::aes_string(xend="weight", y="word", yend="word")) +
        ggplot2::geom_segment(x=0, size=2) +
        ggplot2::scale_y_discrete(name="weight", limits=rev(keys$word)) +
        ggplot2::theme(axis.title.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_text(color="black", size=10),
            legend.position="none") +
        ggplot2::ggtitle(sprintf("Top words in topic %d %s",
            topic, paste(keys$word[1:3], collapse=" ")))
}

#' Plot time series of yearly topic proportions as bars
#'
#' Plot time series of topics as bars (faceted for multiple topics).
#'
#' This is a convenience function for quickly visualizing output from
#' \code{\link{topic_series}}. It does not offer fine-grained control over the
#' plot: for that, I recommend making plots yourself from the data frame.
#'
#' @param series three-column data frame, as from \code{\link{topic_series}}
#' @param group name of column in \code{series} corresponding to individual
#'   series (e.g., \code{"topic"})
#' @param date name of column in \code{series} corresponding to dates/times
#' @param weight name of column in \code{series} corresponding to weights
#' @param labels character vector to label groups by (values of
#'   \code{series[[group]]} are used by default)
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso \code{\link{topic_series}} to generate the series
#'
#' @examples
#' \dontrun{
#' # slightly more informative labels on a faceted plot
#' topic_series(m) %>%
#'      plot_series(labels=topic_labels(m, 4))
#'
#' # only two topics
#' topic_series(m) %>%
#'      filter(topic %in% c(5, 10)) %>%
#'      plot_series()
#' }
#'
#' @export
#'
plot_series <- function (series, group="topic", date="pubdate", weight="weight",
                         labels=unique(series[[group]])) {
    if (!requireNamespace("ggplot2", quietly=T)) {
        stop("Plotting functions require the ggplot2 package.")
    }

    # ensure topics appear in order
    series[[group]] <- factor(series[[group]], levels=unique(series[[group]]),
                              labels=labels, ordered=T)

    # and dates are Dates
    series[[date]] <- as.Date(series[[date]])

    # calculate bar width as a quarter of the breaks interval
    bar_width <- diff(as.Date(unique(series[[date]])[1:2]))
    bar_width <- as.numeric(bar_width, units="days") / 4

    result <- ggplot2::ggplot(series,
            ggplot2::aes_string(x=date, y=weight)) +
        ggplot2::geom_bar(stat="identity", fill="grey80", width=bar_width) +
        ggplot2::geom_smooth(method="loess", fill="grey60", color="black", se=F)

    if (length(unique(series[[group]])) > 1) {
        result <- result +
            ggplot2::facet_wrap(as.formula(paste("~", group)))
    }

    result +
        ggplot2::scale_y_continuous(labels=scales::percent_format()) +
        ggplot2::labs(x="date", y="proportion of total weight",
             title=paste0(group, "s over time"))
}

#' Plot topics on the plane
#'
#' This plot gives a heuristic sense of how "close" topics are to one another.
#' Use in conjunction with \code{\link{topic_scaled_2d}} or your own favorite
#' dimensionality-reduction technique.
#'
#' @param coords two-column matrix of coordinates, with one row for each topic
#' @param labels topic labels to appear on the plot (just numbers are used by
#'   default)
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @seealso \code{\link{topic_scaled_2d}}, \code{\link[stats]{cmdscale}}
#'
#' @examples
#' \dontrun{
#' topic_scaled_2d(m) %>% plot_topic_scaled()
#' }
#'
#' @export
#'
plot_topic_scaled <- function (coords, labels=1:nrow(coords)) {
    if (!requireNamespace("ggplot2", quietly=T)) {
        stop("Plotting functions require the ggplot2 package.")
    }

    if (is.character(labels)) {
        # make labels run vertically
        labels <- stringr::str_wrap(labels, 8)
    }

    frm <- data.frame(label=labels,
                      x=coords[ , 1], y=coords[ , 2])
    ggplot2::ggplot(frm, ggplot2::aes_string(x="x", y="y", label="label")) +
        ggplot2::geom_text(hjust=1, alpha=0.8) +
        # a little extra horizontal air
        ggplot2::scale_x_continuous(expand=c(.1,.1)) +
        ggplot2::theme(axis.title=ggplot2::element_blank(),
              axis.line=ggplot2::element_blank(),
              axis.text=ggplot2::element_blank(),
              axis.ticks=ggplot2::element_blank()) +
        ggplot2::ggtitle("Relative positions of topics")
}

#' Plot word frequencies within topics over time
#'
#' Displays the allocation among topics of a given word. This is a
#' useful diagnostic for models of corpora that are spread over time.
#' The Gibbs sampling state must be loaded. Many analogous plots that
#' condition word weights on two or more variables are possible; see the
#' package vignette for a description of how they can be constructed.
#'
#' @param m \code{dfr_lda} object
#' @param word a term found in the model vocabulary
#' @param n number of topics to include in plot. The cut is made by ranking
#'   topics according to their overall totals of \code{word}.
#' @param breaks time periodicity of the series
#'
#' @return A \link[ggplot2]{ggplot} object.
#'
#' @export
plot_word_topic_series <- function (m, word, n=n_topics(m), breaks="years") {
    if (is.null(m$ss)) {
        stop(
"The Gibbs sampling state must be available. Use load_sampling_state.")
    }

    periods <- cut.Date(metadata(m)$pubdate, breaks=breaks)
    total_series <- colSums(
        sum_col_groups(t(doc_topics(m)), periods)
    )

    series <- sum_col_groups(topic_docs_term(m, word), periods)
    series <- rescale_cols(series, 1 / total_series)
    series_frame <- gather_matrix(series, col_names=c("topic", "year", "weight"))

    keep_topics <- words_top_topics(m, n)
    flt <- lazyeval::interp(~ word == x, x=word)
    keep_topics <- dplyr::filter_(keep_topics, flt)
    series_frame <- series_frame[series_frame$topic %in% keep_topics$topic, ]
    series_frame$topic <- factor(topic_labels(m, 3)[series_frame$topic])

    ggplot2::ggplot(series_frame,
            ggplot2::aes_string(
                x="year", y="weight", group="topic", fill="topic")) +
        ggplot2::geom_area() +
        ggplot2::labs(x="date",
             y=stringr::str_c('"', word, '" as fraction of corpus'),
             title=stringr::str_c('allocation of "', word, '" among topics'))
}

