cached_rest_api_results <- new.env(parent=emptyenv())

load_package_gracefully <- function(package, ...)
{
    if (!requireNamespace(package, quietly=TRUE))
        stop("Could not load package ", package, ". Is it installed?\n\n  ",
             wmsg("Note that ", ..., " requires the ", package, " package. ",
                  "Please install it with:"),
             "\n\n    BiocManager::install(\"", package, "\")")
}

lossless_num_to_int <- function(x)
{
    stopifnot(is.numeric(x))
    y <- suppressWarnings(as.integer(x))
    if (identical(as.numeric(y), x)) y else x
}

