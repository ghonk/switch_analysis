clr <- function(){cat(rep("\n", 50))}

savepdf <- function(fname, width=10, height=10){
  pdf(fname, width=width/2.54, height=height/2.54, pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}
savepng <- function(fname, width=300, height=300){
  png(fname, width=width, height=height, pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

# ------- FUNCTION TO COMPUTE THE PAIRWISE MATRIX
pairwise <- function(start, finish) {
	switches = matrix(0, nrow = 8, ncol = 8)
	source('exemplars.r')
	for (S in 1:8) {
		for (F in 1:8) {
			if (S != F) {
				switches[S,F] = sum(start==S & finish==F)
			}
		}
	}

	rownames(switches) = exemplarstr
	colnames(switches) = exemplarstr
	return(switches)
}


mat2df <- function(DATA) {
	exemplars = c('000','001','010','011','100','101','110','111')

	DATA = as.table(DATA)
	rownames(DATA) <- exemplars
	colnames(DATA) <- exemplars
	DATA = as.data.frame(DATA)
	colnames(DATA) <- c("Start","Finish",'Freq')
	return(DATA)
}



plotpairwise <- function(data, classlabels, sorting=F, LIM = 1) {

	# various infos
	exemplars = c('000','001','010','011','100','101','110','111')
	positions = seq(0,1,by=1/7)
	classcolors = c('red','blue')[classlabels]

	# info on the data
	numsubs = max(rowSums(data)) / 12
	N = sum(data)
	data_norm = sweep(data,1,rowSums(data),`/`)
	egorder = order(classlabels)

	# resort everything if desired
	if (sorting) { 
		data_norm = data_norm[egorder, ]
		data_norm = data_norm[ ,egorder]
		exemplars = exemplars[egorder]
		classcolors = classcolors[egorder]
	}

	data_norm = round(data_norm , digits=2) 

	# plotting the figure
	image(data_norm, 
		col=gray.colors(1000, start = 0, end = 1), 
		zlim = c(0,LIM),
		asp=1, axes=FALSE,frame.plot=FALSE)

	# title(xlab="Starting Exemplar", ylab="Final Exemplar")

	# text data in each cell
	for(i in 1:8) {
		for (A in 1:2) {
			axis(A, at=positions[i], labels=exemplars[i], col.axis=classcolors[i])
		}
	
		for(j in 1:8) {
			if (i!=j) {
				C = rgb(0,0,0)
				# eglab = paste0(exemplars[i], ' > ', exemplars[j] , '\n\n')
				datalab = paste0(round(data_norm[i,j] ,digits=2)*100,'%')

				# text(positions[i],positions[j],labels=eglab,
				# 	cex = .6, col=C)
				text(positions[i],positions[j],labels=datalab, col=C)
			}
		}
	}

	# set up axis
	box()

}