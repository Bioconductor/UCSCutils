cached_rest_api_results <- new.env(parent=emptyenv())

load_package_gracefully <- function(package, ...)
{
    if (!requireNamespace(package, quietly=TRUE))
        stop("Could not load package ", package, ". Is it installed?\n\n  ",
             wmsg("Note that ", ..., " requires the ", package, " package. ",
                  "Please install it with:"),
             "\n\n    BiocManager::install(\"", package, "\")")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### make_data_frame_from_list_of_rows()
###

.lossless_num_to_int <- function(x)
{
    stopifnot(is.numeric(x))
    y <- suppressWarnings(as.integer(x))
    if (identical(as.numeric(y), x)) y else x
}

### Looks like tabular data in JSON is usually row-oriented (i.e. one list
### element per row, each row itself being represented by a named list of
### length-1 atomic vectors), instead of column-oriented. At least that's
### how UCSC's /getData/track endpoint returns their track data.
### Soooo inefficient!
make_data_frame_from_list_of_rows <- function(list_of_rows)
{
    stopifnot(is.list(list_of_rows), is.null(names(list_of_rows)))
    if (length(list_of_rows) == 0L) {
        ## Happens for example with
        ## fetch_UCSC_track_data("eboVir3", "unipAliSwissprot").
        warning(wmsg("track is empty ==> returning a 0x0 data frame"))
        return(data.frame())
    }

    ## Turn list of rows into list of columns (transposition).

    ## 1st implementation.
    #ans_colnames <- names(list_of_rows[[1L]])
    #list_of_cols <- lapply(setNames(ans_colnames, ans_colnames),
    #    function(colname) {
    #        col <- sapply(list_of_rows, function(row) row[[colname]],
    #                      USE.NAMES=FALSE)
    #        if (is.numeric(col))
    #            col <- .lossless_num_to_int(col)
    #        col
    #    }
    #)

    ## 2nd implementation: About 3x faster than the above!
    ## Assumes that all list elements in 'list_of_rows' are ordered the
    ## same i.e. have the same names in the same order.
    #ans_ncol <- length(list_of_rows[[1L]])
    #stopifnot(all(lengths(list_of_rows) == ans_ncol))
    ### Turn 'list_of_rows' into a matrix of type list with
    ### 'length(list_of_rows)' rows and 'ans_ncol' cols.
    #m <- do.call(rbind, list_of_rows)
    #ans_colnames <- colnames(m)
    #list_of_cols <- lapply(setNames(seq_len(ans_ncol), ans_colnames),
    #    function(j) {
    #        col <- unlist(m[ , j], recursive=FALSE, use.names=FALSE)
    #        if (is.numeric(col))
    #            col <- .lossless_num_to_int(col)
    #        col
    #    }
    #)

    ## 3rd implementation: Even slightly faster than 2nd implementation.
    ## Also assumes that all list elements in 'list_of_rows' are ordered
    ## the same i.e. have the same names in the same order.
    ans_ncol <- length(list_of_rows[[1L]])
    ans_colnames <- names(list_of_rows[[1L]])
    stopifnot(all(lengths(list_of_rows) == ans_ncol))
    ## Turn 'list_of_rows' into a matrix of type list with
    ## 'ans_ncol' rows and 'length(list_of_rows)' cols.
    unlisted <- unlist(list_of_rows, recursive=FALSE, use.names=FALSE)
    m <- matrix(unlisted, nrow=ans_ncol)
    list_of_cols <- lapply(setNames(seq_len(ans_ncol), ans_colnames),
        function(i) {
            col <- unlist(m[i, ], recursive=FALSE, use.names=FALSE)
            if (is.numeric(col))
                col <- .lossless_num_to_int(col)
            col
        }
    )

    as.data.frame(list_of_cols, check.names=FALSE)
}

