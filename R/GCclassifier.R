#' @title GCclassifier: an R package for the prediction of molecular subtypes of gastric cancer
#' @description Gastric cancer (GC) can be divided into gene
#' expression-based and biologically distinct molecular subtypes.
#' This package is to built to label gastric cancer samples based
#' on their log2 scaled gene expression profiles (GEP). \cr
#' Three subtyping systems of gastric cancer were implemented, namely "EMP", "ACRG"
#' and "TCGA", each subtyping yielded an distinct subtype numbers, with
#' clear clinical relevance and genomic characterization. More details
#' could be found in the listed references. \cr
#' For EMP system, two subtypes were identified - EP (epithelial phenotype) and
#' MP (mesenchymal subtype), in which the MP associated with worst overall and
#' disease-free susvival. To build the random forest based classifier, prognostic
#' signatures in the coresponding paper was collected and the MP was identified
#' if the probability of MP was larger than or equal to the Youden index value (trained from the
#' TCGA-STAD dataset) or customized probability, if not then the sample would be classified into EP. \cr
#' For ACRG method, MSI and MSS/EMT signature genes were downloaded from
#' corresponding reference. We first trained a random forest model which identified
#' the MSI and MSS/EMT subtypes, the remaining samples were stratified using the Z-normalized
#' gene expression levels of the TP53 signature genes (MDM2 and CDKN1A), and samples with
#' higher expression levels than the Youden index value trained in TCGA-STAD would be
#' labeled as MSS/TP53+, otherwise MSS/TP53-. \cr
#' For TCGA classifier, the signature genes were extracted and the top 200 genes of
#' each subtype were selected to train the model, samples with highest probability would be
#' given the corresponding label, and if probabilities of four labels all smaller than
#' indicated probability, then NA (not available) would be introduced. \cr
#' To facilitate the usage of GCclasssifier package, a shiny application was implemented
#' within the package by running function `classifyGC_interface()`, users could
#' predict their own data by setting up the parameters and upload the data in required format,
#' the application would do the prediction and the results could be downloaded directly
#' on the webpage.
#'
#' @references Oh, S.C., Sohn, B.H., Cheong, JH. et al. Clinical and genomic landscape of gastric
#' cancer with a mesenchymal phenotype. Nat Commun 9, 1777 (2018).
#' https://doi.org/10.1038/s41467-018-04179-8.
#' @references Cristescu, R., Lee, J., Nebozhyn, M. et al. Molecular analysis of gastric cancer
#' identifies subtypes associated with distinct clinical outcomes. Nat Med 21, 449–456 (2015).
#' https://doi.org/10.1038/nm.3850.
#' @references The Cancer Genome Atlas Research Network. Comprehensive molecular characterization of
#' gastric adenocarcinoma. Nature 513, 202–209 (2014).
#' https://doi.org/10.1038/nature13480
#'
#' @docType package
#' @name GCclassifier
NULL
