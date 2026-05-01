








importanceV <- function(x,
                       basal = "BA",
                       plot = "ID",
                       abundance = "n",
                       species = "Species",
                       keep.cols = FALSE){
## across the whole data set
    specs = unique(x[,species])
    spec.df <- as.data.frame(specs)
    "Species" -> names(spec.df)
    spec.df$ImpVal <- NA
    ttl.trees <- sum(x[,abundance])
    p = (unique(x[,plot]))
    ttl.plots <- length(p[[1]])
    ttl.BA <- sum(x[,basal])
    for(i in seq_along(specs[[1]])){ # calculate importance values
      spp = specs[[1]][i]
      spec <- x[x[, species] == spp, ]
      plots = length(unique(spec[,plot]))
      plot.pc = (plots/ttl.plots) * 100
      indiv = sum(spec[,abundance])
      indiv.pc = (indiv/ttl.trees) * 100
      ba.pc = (sum(spec[,basal])/ttl.BA) * 100
      spec.df$ImpVal[i] <- (ba.pc + indiv.pc + plot.pc)/3
    }
    outie <- spec.df

 # print(outie)
}
iv = importanceV(ark)
View(iv)
iv[iv$Species == "Carya_*",]
ar_ca <- iv[grepl("Carya_", iv$Species), ]
