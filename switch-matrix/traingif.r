if(dev.cur() != 1) {dev.off()}
rm(list=ls())

library(animation)
source('functions.r')
source('exemplars.r')

# load data from csv
df = read.csv('csv/switchtrain.csv')
df = subset(df,attempt_num==1)


for (c in 2:4) {
	fname = paste0('type',toString(c),'.gif')
	saveGIF( {
		for (b in c(1:12,12,12,12)) {
			IND = (df$shj==c) & (df$block==b)
			data = pairwise( df$start[IND], df$finish[IND]  )
			classlabels = assignments[,c]


			par(mgp=c(1,0.05,0), tcl=-0, mar=c(2,2,1.9,2))
			plotpairwise(data, classlabels, sorting=T, LIM = 0.4)
			title(main = paste0('Type ',  toString(c), ' Block ', toString(b)), cex=2)
			}
		}, 
			ani.width = 300, ani.height = 300, 
			interval = 0.6, 
			movie.name = fname
	)

}



