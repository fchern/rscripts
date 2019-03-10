#!/usr/bin/env Rscript
library("glmnet")
library("glue")
library(ggplot2)
suppressPackageStartupMessages(library("argparse"))

options(width=150)

extract <- function(o, s) { 
    index <- which(coef(o, s) != 0) 
    data.frame(name=rownames(coef(o))[index], coef=coef(o, s)[index]) 
}

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-v", "--verbose", action="store_true", default=TRUE, help="Print extra output [default]")
parser$add_argument("-q", "--quietly", action="store_false", dest="verbose", help="Print little output")
parser$add_argument("-c", "--count", type="integer", default=5, help="Number of random normals to generate [default %(default)s]", metavar="number")
parser$add_argument("--branch", default="prod", help = "Function to generate random deviates [default \"%(default)s\"]")
parser$add_argument("--tag", default="prod", help = "Function to generate random deviates [default \"%(default)s\"]")
parser$add_argument("--key", default="key", help = "basket name [default \"%(default)s\"]")
parser$add_argument("--keep", default=0.66666, type="double", help="ratio of dim you want to keep in the first component == \"rnorm\" [default %(default)s]")
parser$add_argument("--sd", default=1, type="double", metavar="standard deviation", help="Standard deviation if generator == \"rnorm\" [default %(default)s]")

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()

# print some progress messages to stderr if "quietly" wasn't requested
if ( args$verbose ) { 
    write("Maybe i am", stderr())
    write("writing some verbose output to standard error...\n", stderr()) 
}

if (args$keep < 0.3){
    print("the ratio you keep is too small")
}

branchName <- args$branch
tagName <- args$tag
keyName <- args$key


workingDirName <- glue("/home/fchern/test/{branchName}/{tagName}/{keyName}")

print(glue("we are loadingwriting from to {workingDirName}"))

XName<-glue("{workingDirName}/X.csv")
yName<-glue("{workingDirName}/y.csv")
wName<-glue("{workingDirName}/w.csv")
boundName<-glue("{workingDirName}/bound.csv")


X <- read.csv(glue("{workingDirName}/X.csv"))
y <- read.csv(glue("{workingDirName}/y.csv"))
w <- read.csv(glue("{workingDirName}/w.csv"))
bounds <- read.csv(glue("{workingDirName}/bounds.csv"))

# some very simply sanity checks
stopifnot(nrow(X) == nrow(y) == nrow(w))
stopifnot(nrow(bounds) == ncol(X))

lambdaSequence = seq(5e-1,1e-5,length=100)

# alpha <- 0.0 is ridge regression
cvfit <- glmnet(as.matrix(X),
                as.matrix(y),
                weights = w, 
                lower.limits = as.vector(bounds$lower),
                upper.limits = as.vector(bounds$uppper),
                family = "gaussian", 
                lambda = lambdaSequence,
                alpha = 0.0,
                folds = 20)

print('ploting beta path')

plot(cvfit)

lambdaId <- 0
for (lambda in cvfit$lambda){
    lambdaId <- lambdaId+1
    #lambdaValue <- cvfit$lambda[lambda]
    outputBetaFilename <- glue("{workingDirName}/betas.{lambdaId}.csv")
    print(glue("dump lambda value {lambda} to file{outputBetaFilename}"))
    betas <- extract(cvfit, lambda)
    write.csv(betas, outputBetaFilename)
}
