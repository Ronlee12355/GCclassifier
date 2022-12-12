# GCclassifier: an R package for prediction of gene expression-based molecular subtype of gastric cancer

Gastric cancer (GC) can be divided into gene expression-based and biologically distinct molecular subtypes. This package is to built to label gastric cancer samples based on their log2 scaled gene expression profiles (GEP).

**Install package**
```{r}
  # Required packages: run if not already installed
  if(!requireNamespace('BiocManager')){
    install.packages('BiocManager')
  }
  if(!requireNamespace('devtools')){
    install.packages('devtools')
  }
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
```

**References**
Oh, S.C., Sohn, B.H., Cheong, JH. et al. Clinical and genomic landscape of gastric cancer with a mesenchymal phenotype. Nat Commun 9, 1777 (2018).   
https://doi.org/10.1038/s41467-018-04179-8.      

Cristescu, R., Lee, J., Nebozhyn, M. et al. Molecular analysis of gastric cancer identifies subtypes associated with distinct clinical outcomes. Nat Med 21, 449–456 (2015).      
https://doi.org/10.1038/nm.3850.
