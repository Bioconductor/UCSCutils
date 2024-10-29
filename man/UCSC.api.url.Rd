\name{UCSC.api.url}

\alias{UCSC.api.url}

\title{Get or set the default UCSC API URL}

\description{
  Convenience helper for getting or setting global option \code{UCSC.api.url}.
}

\usage{
UCSC.api.url(api.url=NULL)
}

\arguments{
  \item{api.url}{
    A single string containing the URL to the alternative UCSC API to use
    by default.

    For convenience, \code{api.url} can also be one of the following aliases:
    \itemize{
      \item \code{"primary"}: alias for \code{"https://api.genome.ucsc.edu"}
            (primary URL, US West Coast);
      \item \code{""}: same as \code{"primary"};
      \item \code{"europe"}: alias for
            \code{"https://genome-euro.ucsc.edu/cgi-bin/hubApi"}
            (Europe mirror);
      \item \code{"asia"}: alias for
            \code{"https://genome-asia.ucsc.edu/cgi-bin/hubApi"}
            (Asia mirror).
    }
  }
}

\details{
  Various functions in the \pkg{UCSC.utils} package query the UCSC REST API.
  This is the case for example for \code{\link{list_UCSC_genomes}},
  \code{\link{get_UCSC_chrom_sizes}}, \code{\link{list_UCSC_tracks}}
  and more.

  Global option \code{UCSC.api.url} controls the UCSC API URL
  that these functions use y default. The option is set to
  \code{"https://api.genome.ucsc.edu"} (primary URL, US West Coast)
  at package startup.

  \code{UCSC.api.url()} and \code{UCSC.api.url(some_url)} are provided
  as convenient ways of doing \code{getOption("UCSC.api.url")}
  and \code{options(UCSC.api.url=some_url)}, respectively.
}

\value{
  When called with no argument, \code{UCSC.api.url()} returns
  \code{getOption("UCSC.api.url")}.

  When passed an URL, \code{UCSC.api.url(some_url)} returns the
  \emph{previous} URL, that is, the UCSC API URL that was previously
  used by default. Note that the previous URL is returned invisibly.
}

\seealso{
  \itemize{
    \item \url{https://genome.ucsc.edu/goldenPath/help/api.html#Mirrors}
          a list of alternative UCSC API URLs.

    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.
  }
}

\examples{
UCSC.api.url()  # current default value of the UCSC API URL
get_UCSC_chrom_sizes("ce11", recache=TRUE)

## Temporarily use the mirror in Europe:
previous_url <- UCSC.api.url("europe")
UCSC.api.url()  # new default value of the UCSC API URL

get_UCSC_chrom_sizes("ce11", recache=TRUE)

## Restore previous default value:
UCSC.api.url(previous_url)
UCSC.api.url()
}

\keyword{utilities}
