
test_that("fetch_UCSC_track_data()", {
    result <- fetch_UCSC_track_data("gorGor6", "ncbiRefSeqCurated")
    expect_true(is.data.frame(result))
    EXPECTED_COLNAMES <- c("bin", "name", "chrom", "strand",
                           "txStart", "txEnd", "cdsStart", "cdsEnd",
                           "exonCount", "exonStarts", "exonEnds", "score",
                           "name2", "cdsStartStat", "cdsEndStat", "exonFrames")
    expect_identical(colnames(result), EXPECTED_COLNAMES)
    expect_true(nrow(result) >= 400L)

    result <- fetch_UCSC_track_data("gorGor6", "gap")
    expect_true(is.data.frame(result))
    EXPECTED_COLNAMES <- c("bin", "chrom", "chromStart", "chromEnd", "ix" ,                                "n", "size", "type", "bridge")
    expect_identical(colnames(result), EXPECTED_COLNAMES)
    expect_true(nrow(result) >= 800L)

    result <- fetch_UCSC_track_data("eboVir3", "iedbBcell")
    expect_true(is.data.frame(result))
    expect_true(ncol(result) == 76L)
    expect_true(nrow(result) >= 50L)
})

