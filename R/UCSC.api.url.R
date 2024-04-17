### =========================================================================
### UCSC.api.url()
### -------------------------------------------------------------------------
###

### See https://genome.ucsc.edu/goldenPath/help/api.html#Mirrors
.UCSC_API_URLS <- c(
  primary="https://api.genome.ucsc.edu",  # primary URL (West Coast)
  europe="https://genome-euro.ucsc.edu/cgi-bin/hubApi",
  asia="https://genome-asia.ucsc.edu/cgi-bin/hubApi"
)

.pattern2url <- function(pattern)
{
    if (!nzchar(pattern))
        return(.UCSC_API_URLS["primary"])
    aliases <- names(.UCSC_API_URLS)
    idx <- grep(pattern, aliases, ignore.case=TRUE)
    if (length(idx) == 0L) {
        in1string <- paste0("\"", c("", aliases), "\"")
        in1string[[length(in1string)]] <-
            paste0("or ", in1string[[length(in1string)]])
        in1string <- paste(in1string, collapse=", ")
        stop(wmsg("'api.url' must start with \"https://\" (or \"http://\") ",
                  "or match alias ", in1string))
    }
    if (length(idx) > 1L) {
        in1string <- paste0("\"", aliases[idx], "\"", collapse=" and ")
        stop(wmsg("\"", tolower(pattern), "\" matches more than ",
                  "one alias (", in1string, ")"))
    }
    .UCSC_API_URLS[idx]
}

normarg_api.url <- function(api.url)
{
    if (!isSingleString(api.url))
        stop(wmsg("'api.url' must be a single string"))
    if (grepl("^https?://", api.url, ignore.case=TRUE))
        return(api.url)
    .pattern2url(api.url)
}

UCSC.api.url <- function(api.url=NULL)
{
    ans <- getOption("UCSC.api.url")
    if (is.null(api.url))
        return(ans)
    api.url <- normarg_api.url(api.url)
    options(UCSC.api.url=api.url)
    invisible(ans)  # return previous URL invisibly
}

