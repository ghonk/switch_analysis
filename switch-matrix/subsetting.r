cat(rep("\n", 50))
if(dev.cur() != 1) {dev.off()}
rm(list=ls())

library(prodlim)
source('exemplars.r')
ALLDATA = read.csv('csv/switchit_full_dataset.csv')

# SWITCHIT TEST DATA
relevantcols = 	c('pid',	'train_cond',	'shj_cond',	'trial_num',	'var_9',	'var_10',	'var_11',	'var_12',	'var_13',	'var_14',	'var_15',	'var_16',		'var_18',	'var_19')
newnames = 		c('pid',	'training',		'shj',		'trial',		"finish1",	"finish2",	"finish3",	"start1",	"start2",	"start3",	"startcat",	"finishcat",	'accuracy',	'switch_count')
df = subset(ALLDATA,phase=='switch_test')
df = df[relevantcols] 
colnames(df) = newnames
df$start = row.match(df[c('start1','start2','start3')],exemplars)
df$finish = row.match(df[c('finish1','finish2','finish3')],exemplars)
write.table(df, file = "csv/switchtest.csv", append = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

# SWITCHIT TRAIING DATA
relevantcols = 	c('pid', 	'shj_cond', 'block_num',	'trial_num',	'var_10',	'var_11',	'var_12',	'var_13',	'var_14',	'var_15',	'var_16',	'var_17',		'var_19',	'trial_attempts', 	'switch_count')
newnames = 		c('pid',	'shj',		'block',		'trial',		"finish1",	"finish2",	"finish3",	"start1",	"start2",	"start3",	"startcat",	"finishcat",	'accuracy',	'attempt_num',		'switch_count')
df = subset(ALLDATA,phase=='switch_train')
df = df[relevantcols] 
colnames(df) = newnames
df$start = row.match(df[c('start1','start2','start3')],exemplars)
df$finish = row.match(df[c('finish1','finish2','finish3')],exemplars)
write.table(df, file = "csv/switchtrain.csv", append = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")


print('done')
