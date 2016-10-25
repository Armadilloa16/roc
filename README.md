Simple package including minimalistic implementations for calcuating ROC curves
and associated things (AUC values and Cost functions).

If you wish to use this package, you should use the R package `devtools` to 
install it. If you don't have `devtools`, you can install it with:

```
install.packages("devtools")
```

and then you can install this package with 

```
devtools::install_github("Armadilloa16/roc")
```

after which you will be able to load and attach it in the normal way, with

``` 
library(roc)
```

if you need a tutorial on how the functions provided are used, 
you should install this package with 

```
devtools::install_github("Armadilloa16/roc", build_vignettes = TRUE)
```

and then you will be able to access the vignette with 

```
browseVignettes("roc")
```
