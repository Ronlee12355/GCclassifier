# `GCclassifier`: an R package for prediction of gene expression-based molecular subtype of gastric cancer

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
