% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read10x.R
\name{collpase_chains}
\alias{collpase_chains}
\title{Read 10x Genomics data containing alpha and beta chains}
\usage{
collpase_chains(clone_file)
}
\arguments{
\item{clone_file}{A single .txv file from 10x Genomics}
}
\value{
A tibble with alpha and beta chains collapsed
}
\description{
Read in 10x Genomics data and collapse alpha and beta
chains appropriately.
}
