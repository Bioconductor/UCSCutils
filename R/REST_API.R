### =========================================================================
### Thin R wrappers to UCSC REST API endpoints
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###


.parse_json <- function(response)
{
    ## If the JSON in the content of the response is encoded with
    ## Windows-1252, then content(response, as="text", encoding="UTF-8")
    ## will silently return an NA (see ?httr::content). This happens for
    ## example with the following query:
    ##   query <- list(genome="eboVir3", track="iedbBcell")
    ##   .API_query("getData/track", query=query)
    ## This query returns a response with bytes 233 (\xe9) and 246 (\xf6)
    ## in response$content. These bytes cause the call to content() below
    ## to silently return an NA.
    json_string <- content(response, as="text", encoding="UTF-8")
    stopifnot(is.character(json_string), length(json_string) == 1L)
    if (is.na(json_string)) {
        json_string <- content(response, as="json_string",
                               encoding="Windows-1252")
        stopifnot(isSingleString(json_string))
    }
    parsed_json <- jsonlite::fromJSON(json_string)
    ## Sanity checks.
    stopifnot(is.list(parsed_json), !is.null(names(parsed_json)))
    parsed_json
}

.check_response_status <- function(response, fallback_errmsg=NULL)
{
    status_code <- response[["status_code"]]
    ## HTTP errors 400 and 415 still return parsable JSON with valuable
    ## details about the error.
    ## Error 400 happens for example with the following query:
    ##   query <- list(genome="mm9", track="chainNetBosTau6Viewnet")
    ##   .API_query("getData/track", query=query)
    ## Error 415 happens for example with the following query:
    ##   query <- list(genome="mm9", track="bamMmsNumtSSorted")
    ##   .API_query("getData/track", query=query)
    if (status_code %in% c(400L, 415L)) {
        parsed_json <- .parse_json(response)
        stop(wmsg("[HTTP ", status_code, "] ",
                  parsed_json$statusMessage, ": ", parsed_json$error))
    }
    if (is.null(fallback_errmsg)) {
        stop_for_status(response)
        return(invisible(NULL))
    }
    if (status_code >= 300L)
        stop(wmsg("[HTTP ", status_code, "] ", fallback_errmsg))
}

### Returns parsed JSON content of the response.
.API_query <- function(endpoint, query=list(), api.url=UCSC.api.url(),
                       fallback_errmsg=NULL)
{
    stopifnot(isSingleString(endpoint), nzchar(endpoint),
              is.list(query),
              isSingleString(api.url), nzchar(api.url))
    if (length(query) != 0L)
        stopifnot(!is.null(names(query)))
    url <- paste0(api.url, "/", endpoint)
    response <- GET(url, user_agent("Bioconductor UCSC.utils"), query=query)
    .check_response_status(response, fallback_errmsg)
    .parse_json(response)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### We only support the following endpoints at the moment:
###   - /list/ucscGenomes
###   - /list/chromosomes
###   - /list/tracks
###   - /getData/track
### See https://genome.ucsc.edu/goldenPath/help/api.html#Endpoint for the
### full list of endpoints.
###
### All the functions below return parsed JSON.
###

### Endpoint /list/ucscGenomes
API_list_genomes <- function(api.url=UCSC.api.url())
{
    endpoint <- "list/ucscGenomes"
    fallback_errmsg <- c("failed to get list of UCSC genomes from ", api.url)
    parsed_json <- .API_query(endpoint, api.url=api.url,
                              fallback_errmsg=fallback_errmsg)
    ans <- parsed_json[["ucscGenomes"]]
    ## Sanity check.
    stopifnot(is.list(ans))
    ans
}

### Endpoint /list/chromosomes
API_list_chromosomes <- function(genome, api.url=UCSC.api.url())
{
    stopifnot(isSingleString(genome), nzchar(genome))

    endpoint <- "list/chromosomes"
    query <- list(genome=genome)
    fallback_errmsg <- c(genome, ": unknown UCSC genome ",
                         "(or ", api.url, " is down?)")
    parsed_json <- .API_query(endpoint, query=query, api.url=api.url,
                              fallback_errmsg=fallback_errmsg)
    ## Sanity check.
    stopifnot(identical(parsed_json[["genome"]], genome))
    parsed_json
}

### Endpoint /list/tracks
API_list_tracks <- function(genome, api.url=UCSC.api.url())
{
    stopifnot(isSingleString(genome), nzchar(genome))

    endpoint <- "list/tracks"
    query <- list(genome=genome)
    fallback_errmsg <- c(genome, ": unknown UCSC genome ",
                         "(or ", api.url, " is down?)")
    parsed_json <- .API_query(endpoint, query=query, api.url=api.url,
                              fallback_errmsg=fallback_errmsg)
    ans <- parsed_json[[genome]]
    ## Sanity check.
    stopifnot(is.list(ans))
    ans
}

### Endpoint /getData/track
### Note that the endpoint expects the supplied 'track' argument to be the
### name of the track's primary table rather than the track's name.
### E.g. "catLiftOffGenesV1" rather than "CAT/Liftoff Genes".
API_get_track_data <- function(genome, primary_table, api.url=UCSC.api.url())
{
    stopifnot(isSingleString(genome), nzchar(genome),
              isSingleString(primary_table), nzchar(primary_table))

    endpoint <- "getData/track"
    query <- list(genome=genome, track=primary_table)
    fallback_errmsg <- c(genome, "/", primary_table, ": ",
                         "unknown UCSC genome/primary_table ",
                         "(or ", api.url, " is down?)")
    parsed_json <- .API_query(endpoint, query=query, api.url=api.url,
                              fallback_errmsg=fallback_errmsg)
    ## Sanity checks.
    stopifnot(identical(parsed_json[["genome"]], genome))
    if (!is.null(parsed_json[["track"]]))
        stopifnot(identical(parsed_json[["track"]], primary_table))
    parsed_json
}

