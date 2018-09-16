if(dev.cur() != 1) {dev.off()}
rm(list=ls())

require( tikzDevice )
source('functions.r')
source('exemplars.r')
library(prodlim)
options(width=200)

# load data from csv
df = read.csv('csv/switchtest.csv')

# plot data from each SHJ type
# dev.new(width=10, height=10)
# par(mfrow=c(2,2), pty="s")

for(i in 2:4) {
	for (j in unique(df$training)) {
		IND = (df$shj==i & df$training==j)
		data = pairwise( df$start[IND], df$finish[IND]  )

		# GET FILE NAME FOR PLOT
		# fname = paste0('pdf/Test_', j, '_Type_',  toString(i),'.pdf')
		# savepdf(fname)
		# par(mgp=c(1,0.05,0), tcl=-0, mar=c(2,2,0,2))

		fname = paste0('png/Test_', j, '_Type_',  toString(i),'.png')
		savepng(fname, width = 250, height = 250)
		par(mgp=c(1,0.05,0), tcl=-0, mar=c(1,1,0.1,0.1))

		classlabels = assignments[,i]

		# plotting
		plotpairwise(data, classlabels, sorting=T, LIM = 0.4)
		# title(main = paste0('Type ',  toString(i)), cex=2)
		# Finish it
		
		dev.off()
		# S = paste0('pdfcrop ',fname,' ',fname)
		# system(S)
	}
}

