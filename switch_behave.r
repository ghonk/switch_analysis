library(tidyr)
library(ggplot2)

#kill any pdfs
if(any(grepl("Acrobat",
  readLines(textConnection(system('tasklist', 
    intern = TRUE))))) == TRUE) {
  system('taskkill /f /im acrobat.exe', ignore.stdout = TRUE)
}

setwd('C://Users//garre//Dropbox//aa projects//switchrite//data//')
datafile <- 'switchrite_final.csv'
df <- read.csv(datafile, stringsAsFactors = FALSE)
df$pid        <- as.factor(df$pid)
df$shj_cond   <- as.factor(df$shj_cond)
df$phase      <- as.factor(df$phase)
df$train_cond <- as.factor(df$train_cond) 
df$eg_type    <- as.factor(df$eg_type)

df_st <- subset(df, phase == 'switch_train')

df_st$fin_eg <- 
  paste0(as.character(df_st$var_10), 
  	as.character(df_st$var_11), 
  	as.character(df_st$var_12))

cats <- c('111','011','101','110',
		  '000','100','010','001')

eg_mat <- 
  data.frame(matrix(as.numeric(unlist(lapply(unique(df_st$pid), function(x) {
    list(levels(df_st$pid)[x], 
  	  lapply(unique(df_st$block_num), function(y) {
        with(subset(df_st, subset = ((pid == x) & (block_num == y))),
          length(unique(fin_eg)))
        }))
    }))), ncol = 13, byrow = TRUE))

colnames(eg_mat) <- c('pid', as.character(seq(1,12,1)))
head(eg_mat)

eg_df <- gather(eg_mat, block, eg_count, 2:13)
eg_df$block <- factor(eg_df$block, levels = as.character(seq(1,12,1)))

pdf('distinct_switch_egs.pdf')

ggplot(eg_df, aes(x = block, y = eg_count)) +
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 4, 
  	jitter.height = 0), alpha = .7, size = .75) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 4,
    show.legend = FALSE) +
  theme(legend.position = 'none')

dev.off()
system('open "distinct_switch_egs.pdf"')