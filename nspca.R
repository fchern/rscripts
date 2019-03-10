#!/usr/bin/env Rscript
library("nsprcomp")
library("glue")
library(ggplot2)
suppressPackageStartupMessages(library("argparse"))

options(width=150)

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-v", "--verbose", action="store_true", default=TRUE, help="Print extra output [default]")
parser$add_argument("-q", "--quietly", action="store_false", dest="verbose", help="Print little output")
parser$add_argument("-c", "--count", type="integer", default=5, help="Number of random normals to generate [default %(default)s]", metavar="number")
parser$add_argument("--branch", default="prod", help = "Function to generate random deviates [default \"%(default)s\"]")
parser$add_argument("--tag", default="prod", help = "Function to generate random deviates [default \"%(default)s\"]")
parser$add_argument("--basket", default="insuarance", help = "basket name [default \"%(default)s\"]")
parser$add_argument("--window", default="s600", help = "window in seconds [default \"%(default)s\"]")
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

# do some operations based on user input
#if( args$generator == "rnorm") {
#cat(paste(rnorm(args$count, mean=args$mean, sd=args$sd), collapse="\n"))
#} else {
#cat(paste(do.call(args$generator, list(args$count)), collapse="\n"))
#}

if (args$keep < 0.3){
	print("the ratio you keep is too small")
}

branchName <- args$branch
tagName <- args$tag
basketName <- args$basket
windowName <- args$window

filename <- glue("/home/fchern/test/{branchName}/{tagName}/{basketName}/{windowName}/historical.csv")

print(glue("we are loading X from {filename}"))

#X <- read.csv(filename)

print('dim of X:')
print(dim(MASS::Boston))
print('head of X:')
head(MASS::Boston)

print("number of columns")
numCol = ncol(MASS::Boston)
print(numCol)

# ------------------------------
# main course
# ------------------------------
set.seed(1)

# ignore 1/3 of the input high dimension
dimToKeep = round(numCol/args$keep)

# ++++++ make sure the input data is already scaled! +++++++
pca <- nsprcomp(MASS::Boston,
		k = c(dimToKeep,4, 2, 2), 
		nneg = TRUE, 
		scale. = FALSE, 
		nrestart = 10, 
		em_tol = 1e-4, 
		verbosity = 1)

rotationMatrix = pca$rotation

rn <- row.names(rotationMatrix)
rotation <- as.data.frame(rotationMatrix)


# ------------------------------
# some basic sanity check for the solution
# ------------------------------
print("dot product between PC1 and PC2")
print(sum(rotation$PC1 * rotation$PC2))
print(as.matrix(rotation) %*% t(as.matrix(rotation)))
print(rotation$PC1)
row.names(rotation) = rn
print(rotation)

# ------------------------------
# writing output to csv
# ------------------------------
#outputCsvFilename <- glue("/home/fchern/test/{branchName}/{tagName}/{basketName}/{windowName}/1st_comp.csv")
outputCsvFilename <- glue("/tmp/pc1.csv")
rotation$PC1 <- round(rotation$PC1, digits = 5)
print(glue("we are writing the PC1 to {filename}"))
write.csv(file = outputCsvFilename, x = rotation$PC1, quote = F, row.names = F)

# ------------------------------
# plot output to png
# ------------------------------
outputPngFilename <- glue("/home/fchern/test/{branchName}/{tagName}/{basketName}/{windowName}/1st_comp.png")
#todo: replace the filename
png("first_component_loading.png")
barplot(rotationMatrix[,"PC1"], las=2,  main = glue("1st principal component of basket {basketName}"), font.main = 4, horiz=TRUE)
dev.off()

quit()
