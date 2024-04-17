### =========================================================================
### UCSC.api.url()
### -------------------------------------------------------------------------
###


UCSC.api.url <- function(url=NULL)
{
    ans <- getOption("UCSC.api.url")
    if (is.null(url))
        return(ans)
    if (!(isSingleString(url) && nzchar(url)))
        stop(wmsg("'url' must be a single (non-empty) string"))
    if (!startsWith(tolower(url), "http"))
        stop(wmsg("'url' must start with \"http\""))
    options(UCSC.api.url=url)
    invisible(ans)  # return old URL invisibly
}

