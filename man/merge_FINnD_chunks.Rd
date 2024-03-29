% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_FINnD_chunks.R
\name{merge_FINnD_chunks}
\alias{merge_FINnD_chunks}
\title{Merge results from all chunks of a given sweep.final_nD run}
\usage{
merge_FINnD_chunks(workdir, FINnD_digest_dir.to_merge, save_outputs = FALSE)
}
\arguments{
\item{workdir}{Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
of the dynamic sweep master file (e.g., \strong{\emph{0_EXPLO_DYN_MASTER.xlsx}}) \cr
and where output files will be stored if saved by user. \cr
(character string)}

\item{FINnD_digest_dir.to_merge}{Name of sweep.final_nD digest directory
to which current chunks should be merged. \cr
For instance: "4_FINnD_0_SWEEP_FINnD_demo_001_000_digest"}

\item{save_outputs}{If TRUE, saves merged chunks outputs to sweep.final_nD digest directory.}
}
\value{
Merged chunks sweep.final_nD outputs, including results, chunk logs, chunked parameter spaces.
}
\description{
Merge results from all chunks of a given sweep.final_nD run
}
