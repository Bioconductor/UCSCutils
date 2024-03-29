\name{fetch_UCSC_track_data}

\alias{fetch_UCSC_track_data}

\title{Fetch UCSC track data}

\description{
  Fetch the track data for a given UCSC genome/track.
}

\usage{
fetch_UCSC_track_data(genome, primary_table, api.url=UCSC.api.url())
}

\arguments{
  \item{genome}{
    A single string specifying the name of a UCSC genome e.g. \code{"hs1"},
    \code{"danRer11"}, or \code{"wuhCor1"}. See \code{?\link{list_UCSC_genomes}}
    for how to get the list of valid UCSC genome names.
  }
  \item{primary_table}{
    A single string specifying the name of the primary table associated
    with the track from which to fetch the data.
    See \code{?\link{list_UCSC_tracks}} for how to get the list of tracks
    and associated primary tables for a given UCSC genome.
  }
  \item{api.url}{
    The URL of the UCSC API. By default, the URL returned by
    \code{UCSC.api.url()} is used. Note that what \code{UCSC.api.url()}
    returns is controlled by a global option. See \code{?\link{UCSC.api.url}}
    for more information.
  }
}

\value{
  A data frame.
}

\seealso{
  \itemize{
    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.

    \item \code{\link{list_UCSC_tracks}} to get the list of tracks and
          associated primary tables for a given UCSC genome.

    \item \code{\link{UCSC_dbselect}} for a more efficient and more flexible
          way to retrieve data directly from the UCSC MariaDB server.
  }
}

\examples{
gorGor6_gap_data <- fetch_UCSC_track_data("gorGor6", "gap")
head(gorGor6_gap_data)

## --- Comparison with UCSC_dbselect() ---

gorGor6_gap_data2 <- UCSC_dbselect("gorGor6", "gap")

## Easy sanity checks.
stopifnot(
  identical(dim(gorGor6_gap_data), dim(gorGor6_gap_data2)),
  identical(colnames(gorGor6_gap_data), colnames(gorGor6_gap_data2))
)

## But the two data frames are not identical:
identical(gorGor6_gap_data, gorGor6_gap_data2)  # FALSE!

## However, they have the same content. Ony reason they're not
## identical is because their rows are not in the same order.
## Let's reorder the rows in the two data frames by genomic location,
## that is, first by chromosome, then by chromStart, and finally
## by chromEnd:
sort_rows <- function(gap_data) {
  oo <- order(gap_data$chrom, gap_data$chromStart, gap_data$chromEnd)
  gap_data <- gap_data[oo, ]
  rownames(gap_data) <- NULL
  gap_data
}

gorGor6_gap_data <- sort_rows(gorGor6_gap_data)
gorGor6_gap_data2 <- sort_rows(gorGor6_gap_data2)

## Now the two data frames are identical:
stopifnot(identical(gorGor6_gap_data, gorGor6_gap_data2))
}

\keyword{manip}
