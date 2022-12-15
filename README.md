# GCclassifier: an R package for prediction of gene expression-based molecular subtype of gastric cancer

Gastric cancer (GC) can be divided into gene expression-based and biologically distinct molecular subtypes. This package is to built to label gastric cancer samples based on their log2 scaled gene expression profiles (GEP). More information about the usage of `GCclassifier` could be found here by running `browseVignettes('GCclassifier')`.

**Install package**
```{r}
  # Required packages: run if not already installed
  if(!requireNamespace('BiocManager')){
    install.packages('BiocManager')
  }
  if(!requireNamespace('devtools')){
    install.packages('devtools')
  }

  ## Users need to install these packages before using GCclassifier
  BiocManager::install(c(
    'impute', 'dplyr', 'magrittr', 'AnnotationDbi', 'randomForest', 'org.Hs.eg.db', 
    'shiny', 'DT', 'shinyjs', 'BiocStyle'), force = T)
  ### install: latest version
  devtools::install_github("Ronlee12355/GCclassifier", build_vignettes = TRUE)
```

**Quick start**
```{r}
  library(GCclassifier)
  data("GSE62254")
  
  emp.res <- get_molecular_subtype(
    Expr = GSE62254, ## gene expression profile with log2 transformation
    method = 'EMP', ## subtyping system
    idType = 'SYMBOL' ## the gene identifier type in gene expression profile
  )

  acrg.res <- get_molecular_subtype(
    Expr = GSE62254, ## gene expression profile with log2 transformation
    method = 'ACRG', ## subtyping system
    idType = 'SYMBOL' ## the gene identifier type in gene expression profile
  )
  
  tcga.res <- get_molecular_subtype(
    Expr = GSE62254, 
    method = 'TCGA', 
    idType = 'SYMBOL'
  )
```
**Design**   
`GCclassifier` package builds on standard R package workflow and was developed in RStudio following guidelines in R packages [http://r-pkgs.had.co.nz/].

**References**      
Oh, S.C., Sohn, B.H., Cheong, JH. et al. Clinical and genomic landscape of gastric cancer with a mesenchymal phenotype. Nat Commun 9, 1777 (2018). https://doi.org/10.1038/s41467-018-04179-8             

Cristescu, R., Lee, J., Nebozhyn, M. et al. Molecular analysis of gastric cancer identifies subtypes associated with distinct clinical outcomes. Nat Med 21, 449–456 (2015). https://doi.org/10.1038/nm.3850     

The Cancer Genome Atlas Research Network. Comprehensive molecular characterization of gastric adenocarcinoma. Nature 513, 202–209 (2014).     
https://doi.org/10.1038/nature13480      

R Core Team. R: A Language and Environment for Statistical Computing [Internet]. Vienna, Austria: R Foundation for Statistical Computing; 2013. http://www.R-project.org/

Wickham, H. R Packages: Organize, Test, Document, and Share Your Code. 1st ed. O’Reilly Media. 2015.    

