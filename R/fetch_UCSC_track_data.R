### =========================================================================
### fetch_UCSC_track_data()
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .extract_table_data_from_parsed_json()
###
### WARNING: For JSON parsed with rjson::fromJSON()!
###
### Lots of massaging and sanity checks to deal with the messiness of UCSC's
### JSON.
###

.extract_table_data_from_parsed_json <- function(parsed_json, primary_table)
{
    stopifnot(is.list(parsed_json), !is.null(names(parsed_json)))
    table_data <- parsed_json[[primary_table]]
    ## The table data is either put all together in a single table or split
    ## into one table per chromosome. If the former then 'table_data' is an
    ## unnamed list with one list element per row in the table. If the latter
    ## then 'table_data' is a named list with one list element per chromosome,
    ## where each list element represents a table.
    stopifnot(is.list(table_data))
    if (is.null(names(table_data))) {
        ## One single table.
        ans <- make_data_frame_from_list_of_rows(table_data)
    } else {
        ## 'table_data' is a named list with the chromosome names on it. Each
        ## list element in 'table_data' is itself a list that represents a
        ## table.
        idx <- which(lengths(table_data) != 0L)
        if (length(idx) == 0L) {
            ## We don't know what the columns would have been if the track
            ## had some data so we return a 0-col data frame.
            ## Happens for example with fetch_UCSC_track_data("eboVir3", "gap").
            warning(wmsg("track is empty ==> returning a 0x0 data frame"))
            return(data.frame())
        }
        dfs <- lapply(table_data[idx], make_data_frame_from_list_of_rows)
        ans <- do.call(rbind, unname(dfs))
    }
    stopifnot(nrow(ans) == parsed_json[["itemsReturned"]])
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .extract_table_data_from_parsed_json2()
###
### WARNING: For JSON parsed with jsonlite::fromJSON()!
###
### A slightly simpler alternative to .extract_table_data_from_parsed_json()
### that takes advantage of the built-in data massaging capabilities of
### jsonlite::fromJSON().
### NOT a drop-in replacement for .extract_table_data_from_parsed_json() as
### it first requires switching from rjson::fromJSON to jsonlite::fromJSON
### in internal helper .parse_json() defined in R/REST_API.R.
### As Marcel pointed out, unlike the former the latter recognizes JSON
### lists that represent tabular data and automatically turns them into data
### frames. See https://github.com/Bioconductor/Contributions/issues/3343
###
### Note that the code below still needs to perform a little bit of data
### massaging and sanity checks to deal with the messiness of UCSC's JSON!
###

### The table data is either put all together in a single data frame or split
### into one data frame per chromosome.
### Make sure to always return a single data frame.
.table_data_as_data_frame <- function(table_data)
{
    if (is.data.frame(table_data))
        return(table_data)
    ## 'table_data' should be either an empty list or a named list of
    ## data frames (one per chromosome).
    stopifnot(is.list(table_data))
    if (length(table_data) == 0L) {
        warning(wmsg("track is empty ==> returning a 0x0 data frame"))
        return(data.frame())
    }
    ## From now on, 'table_data' is expected to be a named list of data frames,
    ## one per chromosome.
    stopifnot(!is.null(names(table_data)))
    if (length(table_data) != 1L)
        return(do.call(rbind, unname(table_data)))
    ans <- table_data[[1L]]
    if (length(ans) == 0L) {
        warning(wmsg("track is empty ==> returning a 0x0 data frame"))
        return(data.frame())
    }
    ans
}

.extract_table_data_from_parsed_json2 <- function(parsed_json, primary_table)
{
    stopifnot(is.list(parsed_json), !is.null(names(parsed_json)))
    table_data <- parsed_json[[primary_table]]
    ans <- .table_data_as_data_frame(table_data)
    stopifnot(nrow(ans) == parsed_json[["itemsReturned"]])
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### fetch_UCSC_track_data()
###

### No caching!
fetch_UCSC_track_data <- function(genome, primary_table, api.url=UCSC.api.url())
{
    check_genome(genome)
    if (!(isSingleString(primary_table) && nzchar(primary_table)))
        stop(wmsg("'primary_table' must be a single (non-empty) string"))
    parsed_json <- API_get_track_data(genome, primary_table, api.url=api.url)
    .extract_table_data_from_parsed_json2(parsed_json, primary_table)
}

