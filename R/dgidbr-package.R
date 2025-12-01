#' DGIdbr: Query DGIdb for gene sets and aggregate drug interactions
#'
#' Helpers to run DGIdb GraphQL queries for case-control (group) or subtype gene
#' sets, aggregate interaction scores, and write CSV outputs. The main entry
#' point is \code{\link{DGIdbr}}; lower-level helpers include
#' \code{\link{build_gene_sets}} and \code{\link{run_gene_set}}.
#'
#' Set environment variable \code{DGIDB_URL} to override the default GraphQL
#' endpoint.
#'
#' @keywords internal
"_PACKAGE"

#' @import httr
#' @import jsonlite
#' @import utils
NULL
