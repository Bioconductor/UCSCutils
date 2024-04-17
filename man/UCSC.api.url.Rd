\name{UCSC.api.url}

\alias{UCSC.api.url}

\title{Get or set the default UCSC API URL}

\description{
  Convenience helper for getting or setting global option \code{UCSC.api.url}.
}

\usage{
UCSC.api.url(url=NULL)
}

\arguments{
  \item{url}{
    Alternative UCSC API URL to use by default.
  }
}

\details{
  Various functions in the \pkg{UCSC.utils} package query the UCSC REST API.
  This is the case for example for \code{\link{list_UCSC_genomes}},
  \code{\link{get_UCSC_chrom_sizes}}, \code{\link{list_UCSC_tracks}}
  and others.

  Global option \code{UCSC.api.url} controls the UCSC API URL
  that these functions use y default. The option is set to
  \code{"https://api.genome.ucsc.edu"} at package startup.

  \code{UCSC.api.url()} and \code{UCSC.api.url(url)} are provided
  as convenient ways of doing \code{getOption("UCSC.api.url")}
  and \code{options(UCSC.api.url=url)}, respectively.
}

\value{
  When called with no argument, \code{UCSC.api.url()} returns
  \code{getOption("UCSC.api.url")}.

  When passed an URL, \code{UCSC.api.url(url)} returns the \emph{old} URL,
  that is, the UCSC API URL that was previously used by default. Note
  that the \emph{old} URL is returned invisibly.
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

## Temporarily use the mirror in Asia:
old_url <- UCSC.api.url("https://genome-asia.ucsc.edu/cgi-bin/hubApi")
UCSC.api.url()  # new default value of the UCSC API URL
\donttest{
get_UCSC_chrom_sizes("ce11", recache=TRUE)
}
## Restore old default value:
UCSC.api.url(old_url)
}

\keyword{manip}
