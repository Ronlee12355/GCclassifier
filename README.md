# GCclassifier: An R package for the prediction of molecular subtypes of gastric cancer

<img src="https://github.com/Ronlee12355/GCclassifier/blob/main/GCclassifier.png" height="230" align="right"/>

Gastric cancer (GC) can be divided into gene expression-based and biologically distinct molecular subtypes. This package is to built to predict gastric cancer samples based on their log2 scaled gene expression profiles (GEP). More information about the usage of `GCclassifier` could be found by running `browseVignettes('GCclassifier')`.

Installing the package in RStudio is recommended, since RStudio makes it easier for an average user to work with R Markdown and the vignette of `GCclassifier` is written in R Mardown format and the Pandoc [<http://pandoc.org/>] is required for the vignette creating [<https://bookdown.org/yihui/rmarkdown/installation.html>]. The RStudio IDE can be downloaded via this link [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/). The package installation requires a minimum R version of 4.1.0.

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
    'impute', 'dplyr', 'magrittr', 'AnnotationDbi', 'randomForest', 
    'org.Hs.eg.db', 'shiny', 'DT', 'shinyjs', 'BiocStyle', 'survminer'), force = T)
  
  ### install: latest version (R version >= 4.1.0 required)
  ### build_vignettes = T, if installing using RStudio
  devtools::install_github("Ronlee12355/GCclassifier", build_vignettes = T)
  
  ### if not installing from RStudio IDE, no vignette creating is recommended since it requires Pandoc and other dependancies
  devtools::install_github("Ronlee12355/GCclassifier", build_vignettes = F)
```

**Quick start**

```{r}
  library(GCclassifier)
  data("GSE62254")
  
  emp.res <- classifyGC(
    Expr = GSE62254, ## gene expression profiles with log2 transformation
    method = 'EMP', ## subtyping system
    idType = 'SYMBOL' ## the gene identifier type in gene expression profiles
  )

  acrg.res <- classifyGC(
    Expr = GSE62254, 
    method = 'ACRG', 
    idType = 'SYMBOL' 
  )
  
  tcga.res <- classifyGC(
    Expr = GSE62254, 
    method = 'TCGA', 
    idType = 'SYMBOL'
  )
```

**Internal Shiny application**   
To extend the usage of `GCclassifier`, a build-in Shiny application was developed and users could obtain the subtype prediction results simply by uploading their gene expression profiles.    

```
library(GCclassifier)
classifyGC_interface()
```

Supported browers:
- Chrome (recommended)
- Firefox
- Safari
- Microsoft Edge    
  

**Design**   
`GCclassifier` package builds on standard R package workflow and was developed in RStudio following guidelines in R packages [<http://r-pkgs.had.co.nz/>].

**Citation**   
Jiang Li, Lingli He, Xianrui Zhang, Xiang Li, Lishi Wang, Zhongxu Zhu, Kai Song, Xin Wang. GCclassifier: An R package for the prediction of molecular subtypes of gastric cancer. Comput. Struct. Biotechnol. J. 2024; 23: 752-758. <https://doi.org/10.1016/j.csbj.2024.01.010>

**References**   
Oh, S.C., Sohn, B.H., Cheong, JH. et al. Clinical and genomic landscape of gastric cancer with a mesenchymal phenotype. Nat Commun 9, 1777 (2018). <https://doi.org/10.1038/s41467-018-04179-8>

Cristescu, R., Lee, J., Nebozhyn, M. et al. Molecular analysis of gastric cancer identifies subtypes associated with distinct clinical outcomes. Nat Med 21, 449--456 (2015). <https://doi.org/10.1038/nm.3850>

The Cancer Genome Atlas Research Network. Comprehensive molecular characterization of gastric adenocarcinoma. Nature 513, 202--209 (2014). <https://doi.org/10.1038/nature13480>

R Core Team. R: A Language and Environment for Statistical Computing [Internet]. Vienna, Austria: R Foundation for Statistical Computing; 2013. <http://www.R-project.org/>

Wickham, H. R Packages: Organize, Test, Document, and Share Your Code. 1st ed. O'Reilly Media. 2015.
