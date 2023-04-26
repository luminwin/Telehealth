############################################################################
#   The R code of the following paper with a subset of the data
############################################################################
#   Lu M. and Liao X. (2023). Telehealth Utilization in U.S. Medicare 
#          Beneficiaries Aged 65 Years and Older During the COVID-19 Pandemic. 
#          BMC Public Health. 23:368-382. 
#
# @ARTICLE{LuBMC2023,
#   AUTHOR={Lu, Min and Liao, Xinyi},   
#   TITLE={Telehealth utilization in U.S. Medicare beneficiaries aged 65 years 
#             and older during the COVID-19 pandemic},      
#   JOURNAL={BMC Public Health },      
#   VOLUME={23}, 
#   PAGES={368-382},          
#   YEAR={2023},      
#   URL={https://www.frontiersin.org/articles/10.3389/fpubh.2022.946944},       
#   DOI={10.1186/s12889-023-15263-0},      
#   ISSN={2296-2565},
# }
############################################################################

## Import the data and sample weights from the survey
## Note that this is only a subset of the data from the paper
load(url("https://luminwin.github.io/Telehealth/dat.rda") )

## Import variable names for nice display
info <- read.csv("https://luminwin.github.io/Telehealth/info.csv")

############################################################################
#  Step 1: Impute Missing data & Tuning Parameters (Figure S1)
############################################################################
library(randomForestSRC)

dat <- impute(cbind(ACV_TELMED, ACV_INTERNET)~., data = dat, nimpute = 1)

ot <- tune(cbind(ACV_TELMED, ACV_INTERNET)~., data = dat, 
           # na.action = "na.impute",  ## Another option to replace line 34
           seed = 1)  ## changing the seed might provide a slightly different result

# Check the result
ot$optimal

## nice little wrapper for plotting results
## install.packages("interp")
library("interp")
  plot.tune <- function(o, linear = TRUE) {
    x <- o$results[,1]
    y <- o$results[,2]
    z <- o$results[,3]
    so <- interp(x=x, y=y, z=z, linear = linear)
    idx <- which.min(z)
    x0 <- x[idx]
    y0 <- y[idx]
    filled.contour(x = so$x,
                   y = so$y,
                   z = so$z,
                   xlim = range(so$x, finite = TRUE) + c(-2, 2),
                   ylim = range(so$y, finite = TRUE) + c(-2, 2),
                   color.palette =
                     colorRampPalette(c("yellow", "red")),
                   xlab = "Minumum size of terminal node  (nodesize)",
                   ylab = "Number of variables to possibly split at each node (mtry)",
                   key.title = title(main = "Misclassification error", cex.main = 1),
                   plot.axes = {axis(1);axis(2);points(x0,y0,pch="x",cex=1,font=2);
                     points(x,y,pch=16,cex=.25)})
  }

#pdf("tune.pdf", width = 8, height = 7)
plot.tune(ot)
#dev.off()

############################################################################
#  Step 2: Fitting a Random Forest Model 
############################################################################
o <- rfsrc(cbind(ACV_TELMED, ACV_INTERNET)~., 
           data = dat, ntree = 1000, seed = 1, 
           nodesize = ot$optimal["nodesize"],
           # na.action = "na.impute",  ## Another option to replace line 34
           mtry = ot$optimal["mtry"]
)
## report the prediction error (misclassification error)
get.mv.error(o)
############################################################################
#  Step 3: Variable Importance (Table 2, S2)
############################################################################

rf.ci <- subsample(o, 
                    # B = 1000,
                    # block.size = 30,
                    #subratio = 0.5, ## increase the ratio for small sample size 
                    )

otcms <- c("ACV_TELMED","ACV_INTERNET")
for (i in 1:2){
ex.ci <- extract.subsample(rf.ci, m.target = otcms[i])
table.sig <- cbind(ex.ci$vmp, 
                   ex.ci$se.jk.Z, ex.ci$var.jk.sel.Z[, "pvalue"])
rownames(table.sig) <- info[match(rownames(table.sig),info$Var),"x.Nam"]
print(otcms[i])
print(table.sig)
}

############################################################################
#  Step 3: Interaction Detection (Figure S2)
############################################################################
inter <- find.interaction(o, method = "maxsubtree")

x.Nam <- info[match(rownames(inter),info$Var),"x.Nam"]

colnames(inter) <- rownames(inter) <- x.Nam

#pdf("heat.pdf", width = 12, height = 12)
heatmap(inter,margins = c(15,15))
#dev.off()



