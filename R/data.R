#' Gene expression profile of GEO dataset GSE62254
#'
#' A dataset containing the gene expression profile of 300
#' gastric cancer samples.
#'
#' @format A gene expression data frame with 406 gene symbols
#' and 300 samples:
#' @source \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE62254}
"GSE62254"


#' Phenotype and subtype information of GEO dataset GSE62254
#'
#' A dataframe containing the subtype and survival information of 300
#' gastric cancer samples.
#'
#' @format A dataframe with 300 observations and 7 variables:
#' \describe{
#'  \item{GEO_ID}{GEO id number}
#'  \item{Subgroup}{subtype information in EMP subtyping system}
#'  \item{Recur}{recurrence information, 1 means with recurrence}
#'  \item{DFS.m}{disease free survival (DFS), in months}
#'  \item{Death}{survial condition, 1 means death}
#'  \item{OS.m}{overall survival (OS), in months}
#'  \item{ACRG.sub}{subtype information in ACRG subtyping system}
#' }
#' @source \url{https://www.nature.com/articles/nm.3850}
"GSE62254_subtype"
