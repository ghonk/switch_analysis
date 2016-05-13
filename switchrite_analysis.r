# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Switchrite Analysis # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 
# # load packages
#library(plyr)
#library(dplyr)
library(ggplot2)
library(lmerTest)
library(lsmeans)
library(tidyr)


# # # set data location and load
setwd('C://Users//garre//Dropbox//aa projects//switchrite//data//')
datafile <- 'switchrite_final.csv'
datafull <- read.csv(datafile, stringsAsFactors = FALSE)
datafull$pid        <- as.factor(datafull$pid)
datafull$shj_cond   <- as.factor(datafull$shj_cond)
datafull$phase      <- as.factor(datafull$phase)
datafull$train_cond <- as.factor(datafull$train_cond) 
datafull$eg_type    <- as.factor(datafull$eg_type)
# # additional DFs/refs for exploratory analyses
training_data <- subset(datafull, subset = (phase == 'classify_train' | phase == 'switch_train'))
training_data.ref2  <- within(training_data, shj_cond <- 
  relevel(shj_cond, ref = 'two'))
datafull.ref2       <- within(datafull, shj_cond <- 
  relevel(shj_cond, ref = 'two'))
datafull.swi_ref    <- within(datafull, train_cond <- 
  relevel(train_cond, ref = 'switch'))
datafull.par_ref    <- within(datafull, eg_type <- 
  relevel(eg_type, ref = 'partial'))


# # # # # # # # # # # # INITIAL PROCESSING FROM RAW # # # # # # # # # # # # # # #
# # # # set file location
# # setwd('C://Users//garre//Dropbox//aa projects//switchrite//data//raw data')

# # # # load and process all raw data files
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # combine all files
# file_name <- paste0("switchrite_", Sys.Date(), ".csv")
# copy_command <- paste0("copy *.csv ", file_name)
# shell(copy_command, wait = TRUE)

# # # load data
# datafull <- read.csv(file_name, header = FALSE, stringsAsFactors = FALSE)
# # # set PID as int
# datafull$V1 <- as.integer(datafull$V1)
# # # clean all non-participant data rows
# datafull <- subset(datafull, subset = (V1 > 4000))
# # # set other vars to factors as needed
# datafull$V2  <- as.factor(datafull$V2)
# datafull$V3  <- as.factor(datafull$V3)
# datafull$V9  <- as.factor(datafull$V9)
# datafull$V10 <- as.factor(datafull$V10)
# datafull$V11 <- as.factor(datafull$V11)
# # # set all column names
# colnames(datafull) <- c('pid', 'cond', 'shj_cond', 'bal_cond', 'phase', 
#   'block_num', 'trial_num', 'var_8', 'var_9', 'var_10', 'var_11', 'var_12',
#   'var_13', 'var_14', 'var_15', 'var_16', 'var_17', 'var_18', 'var_19', 'switch_count')

# # # # create training condition variable
# datafull$train_cond <- mapvalues(datafull$cond, 
#   from = c('1', '2', '3', '4', '5', '6'),
#   to = c('classify', 'classify', 'classify', 'switch', 'switch', 'switch'))

# # # # create partial eg variable
# eg_vec <- rep('full', nrow(datafull))
# eg_vec[datafull$var_9  == 'nan' | 
#        datafull$var_10 == 'NaN' | 
#        datafull$var_11 == 'NaN'] <- 'partial'
# datafull$eg_type <- as.factor(eg_vec)     

# # # # create training data subset
# training_data <- subset(datafull, subset = (phase == 'classify_train' | phase == 'switch_train'))
# training_data$var_8 <- as.numeric(training_data$var_8)

# # # # create combined inference and classification test trial acc and RT vars
# # accuracy
# test_acc_vals <- datafull$var_19
# test_acc_vals[datafull$phase != 'inference_test'] <- 
#   datafull$var_18[datafull$phase != 'inference_test']
# test_acc_vals[datafull$phase != 'inference_test' & 
#   datafull$phase != 'classify_test' &
#   datafull$phase != 'switch_test'] <- NA
# datafull$test_acc <- test_acc_vals
# # RTs
# test_rt_vals <- datafull$var_18
# test_rt_vals[datafull$phase != 'inference_test'] <- 
#   as.numeric(datafull$var_17[datafull$phase != 'inference_test'])
# test_rt_vals[datafull$phase != 'inference_test' & 
#   datafull$phase != 'classify_test' &
#   datafull$phase != 'switch_test'] <- NA
# datafull$test_rt <- test_rt_vals

# # load required functions
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # get standard error
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm = FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
    
}

# # plot multiple figs
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# # # outlier split / chance analysis
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # number of trials for each test phase sub condition
test_trials <- list(c(12, 24, 26), c(24, 36, 36), 8) # # classify, inference, switch; 2, 3, 4

# # # calculate binomial probability for being below chance

trial_list <- unlist(test_trials, recursive = TRUE)
max_trials <- max(trial_list)
binom_df <- data.frame(0:max_trials)

for(idx in 1:length(trial_list)){
  if(idx == 7){
    binom_df <- cbind(binom_df, c(round(dbinom(0:trial_list[[idx]], 
      trial_list[[idx]], .33), 3), rep(NA, max_trials - trial_list[[idx]]))) 
  } else {
    binom_df <- cbind(binom_df, c(round(dbinom(0:trial_list[[idx]], 
      trial_list[[idx]], .5), 3), rep(NA, max_trials - trial_list[[idx]]))) 
  }
}

names(binom_df) <- c('trial', 'cla2', 'cla3', 'cla4', 'inf2', 'inf3', 'inf4', 'swi')

# # # shape data 
binom_df2 <- spread(subset(gather(binom_df, condition, mean_val, -trial),
  mean_val < .05), condition, mean_val, fill = NA)

# # # find # of trials correct that is below chance 
num_trials_chance <- c(min(binom_df2$trial[binom_df2$cla2 == max(binom_df2$cla2, 
    na.rm = TRUE)], na.rm = TRUE),
  min(binom_df2$trial[binom_df2$cla3 == max(binom_df2$cla3, 
    na.rm = TRUE)], na.rm = TRUE),
  min(binom_df2$trial[binom_df2$cla4 == max(binom_df2$cla4, 
    na.rm = TRUE)], na.rm = TRUE),
  min(binom_df2$trial[binom_df2$inf2 == max(binom_df2$inf2, 
    na.rm = TRUE)], na.rm = TRUE),
  min(binom_df2$trial[binom_df2$inf3 == max(binom_df2$inf3, 
    na.rm = TRUE)], na.rm = TRUE),
  min(binom_df2$trial[binom_df2$inf4 == max(binom_df2$inf4, 
    na.rm = TRUE)], na.rm = TRUE),
  min(binom_df2$trial[binom_df2$swi == max(binom_df2$swi,
    na.rm = TRUE)], na.rm = TRUE))

# # # calculate proportion correct to be under chance  
chance_means <- num_trials_chance/trial_list

# # # make dataframe of test phase means
mean_data <- aggregate(test_acc ~ shj_cond + phase + pid, mean, data = datafull)
mean_data$cla_filt <- rep(0, nrow(mean_data))
mean_data$inf_filt <- rep(0, nrow(mean_data))

# # # find low performers
mean_data$cla_filt[which(mean_data$shj_cond == 'two' & 
                        mean_data$phase == 'classify_test' & 
                        mean_data$test_acc > chance_means[[1]])] <- 1
mean_data$cla_filt[which(mean_data$shj_cond == 'three' & 
                        mean_data$phase == 'classify_test' & 
                        mean_data$test_acc > chance_means[[2]])] <- 1
mean_data$cla_filt[which(mean_data$shj_cond == 'four' & 
                        mean_data$phase == 'classify_test' & 
                        mean_data$test_acc > chance_means[[3]])] <- 1
mean_data$inf_filt[which(mean_data$shj_cond == 'two' & 
                        mean_data$phase == 'inference_test' & 
                        mean_data$test_acc > chance_means[[1]])] <- 1
mean_data$inf_filt[which(mean_data$shj_cond == 'three' & 
                        mean_data$phase == 'inference_test' & 
                        mean_data$test_acc > chance_means[[2]])] <- 1
mean_data$inf_filt[which(mean_data$shj_cond == 'four' & 
                        mean_data$phase == 'inference_test' & 
                        mean_data$test_acc > chance_means[[3]])] <- 1

# # # find PIDs for lower-than-chance performers
cla_filt_pids <- mean_data$pid[which(mean_data$cla_filt == 1)]
inf_filt_pids <- mean_data$pid[which(mean_data$inf_filt == 1)]
# # # intialize filter vectors
datafull$cla_filt <- rep(0, nrow(datafull))
datafull$inf_filt <- rep(0, nrow(datafull))
# # # add to final data
datafull$cla_filt[which(datafull$pid %in% cla_filt_pids)] <- 1
datafull$inf_filt[which(datafull$pid %in% inf_filt_pids)] <- 1


# # # inconsistent features analysis
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

classify_data <- read.csv('classify_data.csv', stringsAsFactors = FALSE)
classify_data$allfeats <- paste0(classify_data$f1, classify_data$f2, classify_data$f3)
classify_data2 <- subset(classify_data, classify_data$shj == 2)
classify_data4 <- subset(classify_data, classify_data$shj == 4)

classify_eg_data <- subset(data.frame(datafull$idx, datafull$pid, datafull$train_cond,
  datafull$shj_cond, datafull$phase, datafull$var_9, datafull$var_10, datafull$var_11, 
  datafull$test_acc), datafull$phase == 'classify_test')
colnames(classify_eg_data) <- gsub('datafull.', '', colnames(classify_eg_data))

classify_eg_data$all_feats <- paste0(classify_eg_data$var_9, 
  classify_eg_data$var_10, classify_eg_data$var_11)

classify_eg_data2 <- subset(classify_eg_data, classify_eg_data$shj_cond == 'two')
classify_eg_data4 <- subset(classify_eg_data, classify_eg_data$shj_cond == 'four')

classify_eg_data2$feat_con <- mapvalues(classify_eg_data2$all_feats, 
  from = classify_data2$allfeats, to = classify_data2$feat_con)
classify_eg_data4$feat_con <- mapvalues(classify_eg_data4$all_feats,
  from = classify_data4$allfeats, to = classify_data4$feat_con)
classify_eg_data2$feat_con <- as.factor(classify_eg_data2$feat_con)
classify_eg_data4$feat_con <- as.factor(classify_eg_data4$feat_con)

type2model <- glmer(test_acc ~ train_cond * feat_con + (1|pid), data = classify_eg_data2,
  family = binomial)
summary(type2model)
lsmeans(type2model, 'train_cond', by = 'feat_con', type = 'response')

type4model <- glmer(test_acc ~ train_cond * feat_con + (1|pid), data = classify_eg_data4,
  family = binomial)
summary(type4model)
lsmeans(type4model, 'train_cond', by = 'feat_con', type = 'response')

plot1 <- ggplot(aggregate(test_acc ~ train_cond + feat_con + pid, mean, data = classify_eg_data2),
  aes(train_cond, test_acc, fill = feat_con)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Type II') + geom_jitter(alpha = .7, 
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) + 
  scale_colour_manual('feature consistency', values = c('con' = 'forest green','irr' = 'blue'))

plot2 <- ggplot(aggregate(test_acc ~ train_cond + feat_con + pid, mean, data = classify_eg_data4),
  aes(train_cond, test_acc, fill = feat_con)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Type II') + geom_jitter(alpha = .7, 
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) + 
  scale_colour_manual('feature consistency', values = c('con' = 'forest green','inc' = 'blue'))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # start analysis
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # condition count
with(datafull, tapply(pid, cond, FUN = function(x) length(unique(x))))


# # training analysis
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
trn_swi_acc <- glmer(var_19 ~ shj_cond + (1|pid/block_num), 
  data = subset(training_data, 
    subset = (phase == 'switch_train' & trial_attempts == 1)), family = binomial)
summary(trn_swi_acc)

trn_cla_acc <- glmer(var_19 ~ shj_cond + (1|pid), 
  data = subset(training_data.ref2, 
    subset = (phase == 'classify_train' & trial_attempts == 1)), family = binomial)
summary(trn_cla_acc)

# # overall performance
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
all_acc_mean <- aggregate(test_acc ~ train_cond + eg_type, mean, 
  data = datafull)

# # # accuracy - all test phases collapsed
all_acc <- glmer(test_acc ~ train_cond + (1|pid),  
  data = datafull, family = binomial)
summary(all_acc)
lsmeans(all_acc, 'train_cond', type = 'response')

# # all RTs
# # # #
all_rt <- lmer(test_rt ~ train_cond + (1|pid), data = datafull,
  REML = FALSE)
summary(all_rt)
lsmeans(all_rt, 'train_cond', type = 'response')

# # # accuracy - phases with partial examples collapsed
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
inf_cla_acc <- glmer(test_acc ~ train_cond * eg_type + (1|pid),  
  data = subset(datafull,  
    subset = (phase == 'inference_test' | phase == 'classify_test')), 
  family = binomial)
summary(inf_cla_acc)
lsmeans(inf_cla_acc, 'train_cond', by = 'eg_type', type = 'response')

# # RTs
# # # #
inf_cla_rt <- lmer(test_rt ~ train_cond * eg_type + (1|pid),  
  data = subset(datafull,  
    subset = (phase == 'inference_test' | phase == 'classify_test')), , REML = FALSE)
summary(all_rt)
lsmeans(inf_cla_rt, 'train_cond', by = 'eg_type', type = 'response') 

# # mean approach just to make sure it's consistent
inf_cla_acc_mean <- lmer(test_acc ~ train_cond * eg_type + (1|pid),  
  data = aggregate(test_acc ~ train_cond + eg_type + pid, mean, 
    data = subset(datafull, 
      subset = (phase == 'inference_test' | phase == 'classify_test'))),
  REML = FALSE)
summary(inf_cla_acc_mean)


# # inference test
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

inf_means <- aggregate(test_acc ~ train_cond + eg_type, mean, 
  data = subset(datafull, phase == 'inference_test'))

# # condition only
inf_reg_all <- glmer(test_acc ~ train_cond + (1|pid),  
  data = subset(datafull, subset = (phase == 'inference_test')), 
  family = binomial)
summary(inf_reg_all)

# # accuracy predicted by condition plus example type
inf_reg_all <- glmer(test_acc ~ train_cond * eg_type + (1|pid),  
  data = subset(datafull, subset = (phase == 'inference_test')), family = binomial)
summary(inf_reg_all)
lsmeans(inf_reg_all, 'train_cond', by = 'eg_type', type = 'response')

# # RT predicted by condition and example type
inf_reg_all_rt <- lmer(test_rt ~ train_cond * eg_type + (1|pid),  
  data = subset(datafull, phase == 'inference_test'), REML = FALSE)
summary(inf_reg_all_rt)
lsmeans(inf_reg_all_rt, 'train_cond', by = 'eg_type', type = 'response')

# # accuracy by shj type
inf_reg_shj <- glmer(test_acc ~ shj_cond + (1|pid),  
  data = subset(datafull.ref2, phase == 'inference_test' & train_cond == 'switch'), 
  family = binomial)
summary(inf_reg_shj)

inf_reg_2 <- glmer(test_acc ~ train_cond * eg_type + (1|pid),  
  data = subset(datafull, subset = (phase == 'inference_test' & shj_cond == 'two')), 
  family = binomial)
summary(inf_reg_2)

inf_reg_3 <- glmer(test_acc ~ train_cond * eg_type + (1|pid),    
  data = subset(datafull, subset = (phase == 'inference_test' & shj_cond == 'three')), 
  family = binomial)
summary(inf_reg_3)

inf_reg_4 <- glmer(test_acc ~ train_cond * eg_type + (1|pid),    
  data = subset(datafull, subset= (phase == 'inference_test' & shj_cond == 'four')), 
  family = binomial)
summary(inf_reg_4)


# # Classify test
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # condition only
cla_reg_all <- glmer(test_acc ~ train_cond + (1|pid),  
  data = subset(datafull, phase == 'classify_test'), family = binomial)
summary(cla_reg_all)
lsmeans(cla_reg_all, "train_cond", type = "response")

# # accuracy predicted by condition and example type
cla_reg_all <- glmer(test_acc ~ train_cond * eg_type + (1|pid),  
  data = subset(datafull, subset = (phase == 'classify_test')), family = binomial)
summary(cla_reg_all)
lsmeans(cla_reg_all, "train_cond", by = 'eg_type', type = "response")


# # accuracy predicted by condition and example type
cla_reg_all <- glmer(test_acc ~ train_cond + (1|pid),  
  data = subset(datafull, subset = (phase == 'classify_test' & eg_type == 'partial')), family = binomial)
summary(cla_reg_all)
lsmeans(cla_reg_all, "train_cond", by = 'eg_type', type = "response")

# # RTs
cla_reg_rt <- lmer(test_rt ~ train_cond * eg_type + (1|pid),  
  data = subset(datafull, phase == 'classify_test'), REML = FALSE)
summary(cla_reg_rt)

# # # shj analysis
cla_reg_shj <- glmer(test_acc ~ shj_cond + (1|pid),  
  data = subset(datafull.ref2, subset = (phase == 'classify_test' & train_cond == 'switch')), 
  family = binomial)
summary(cla_reg_shj)

cla_reg_2 <- glmer(test_acc ~ as.factor(train_cond) * as.factor(eg_type) + (1|pid),    
  data = subset(datafull, subset = (phase == 'classify_test' & shj_cond == 'two')), 
  family = binomial)
summary(cla_reg_2)

cla_reg_3 <- glmer(test_acc ~ as.factor(train_cond) * as.factor(eg_type) + (1|pid),  
  data = subset(datafull, phase == 'classify_test' & shj_cond == 'three'), 
  family = binomial)
summary(cla_reg_3)

cla_reg_4 <- glmer(test_acc ~ as.factor(train_cond) * as.factor(eg_type) + (1|pid),  
  data = subset(datafull, subset = (phase == 'classify_test' & shj_cond == 'four')), 
  family = binomial)
summary(cla_reg_4)


# # switch test
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

swi_reg_all <- glmer(test_acc ~ as.factor(train_cond) + (1|pid),  
  data = subset(datafull, phase == 'switch_test'), 
  family = binomial)
summary(swi_reg_all)

swi_reg_all_rt <- lmer(test_rt ~ as.factor(train_cond) + (1|pid),  
  data = subset(datafull, phase == 'switch_test'), REML = FALSE)
summary(swi_reg_all_rt)

swi_reg_shj <- glmer(test_acc ~ shj_cond + (1|pid),  
  data = subset(datafull.ref2, phase == 'switch_test' & train_cond == 'switch'),
  family = binomial)
summary(swi_reg_shj)

swi_reg_2 <- glm(test_acc ~ as.factor(train_cond),  
  data = subset(datafull, phase == 'switch_test' & shj_cond == '2'), 
  family = binomial)
summary(swi_reg_2)

swi_reg_3 <- glm(test_acc ~ as.factor(train_cond),  
  data = subset(datafull, phase == 'switch_test' & shj_cond == '3'), 
  family = binomial)
summary(swi_reg_3)

swi_reg_4 <- glm(test_acc ~ as.factor(train_cond),  
  data = subset(datafull, subset=(phase == 'switch_test' & shj_cond == 'four')), 
  family = binomial)
summary(swi_reg_4)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # visualization
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # accuracy across blocks
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
training_data$shj_cond <- factor(training_data$shj_cond, c("two","three","four"))

acc_x_block_plot_swi <- ggplot(data = aggregate(var_19 ~ block_num + shj_cond + pid,
  mean, data = subset(training_data, subset = (trial_attempts == 1 & train_cond == 'switch'))), 
  aes(x = as.factor(block_num), y = var_19, fill = shj_cond)) + 
  geom_boxplot(outlier.colour = NA, lwd = 1, fatten = 1) +
  scale_x_discrete(name = '') + scale_y_continuous(name = 'Switch', 
    limits = c(0,1.01), breaks = seq(0, 1, .25),) +
  scale_fill_brewer('Category Type', palette = 'Purples') +
  theme(axis.text.y = element_text(size = 14, color = 'black'), axis.text.x = element_blank(),
    axis.title.y = element_text(size = 18), legend.title = element_blank(), 
    legend.text = element_text(size = 16), 
    legend.position = 'none') #+
  #coord_cartesian(ylim = c(0.00,1.01)) # +
  #geom_text(aes(label = ifelse(var_19 < .30, as.character(pid),'')), 
  #  position = position_dodge(), hjust = 0, vjust = 0) +
acc_x_block_plot_cla <- ggplot(data = aggregate(var_19 ~ block_num + shj_cond + pid,
  mean, data = subset(training_data, subset = (trial_attempts == 1 & train_cond == 'classify'))), 
  aes(x = as.factor(block_num), y = var_19, fill = shj_cond)) + 
  geom_boxplot(outlier.colour = NA, lwd = 1, fatten = 1) +
  scale_x_discrete(name = '') + scale_y_continuous(name = 'Classify', 
    limits = c(0,1.01), breaks = seq(0, 1, .25),) +
  scale_fill_brewer('Category Type', palette = 'Purples', breaks = c('II', 'III', 'IV')) +
  theme(axis.text.y = element_text(size = 14, color = 'black'), 
    axis.text.x = element_text(size = 14, color = 'black'), 
    axis.title.y = element_text(size = 18, vjust = 0.5), 
    legend.text = element_text(size = 16)) # +
  #geom_text(aes(label = ifelse(var_19 < .30, as.character(pid),'')), 
  #  position = position_dodge(), hjust = 0, vjust = 0) +
multiplot(acc_x_block_plot_swi,
          acc_x_block_plot_cla, cols = 1)

# # attempt counter
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

trial_attempt_plot <- ggplot(training_data, aes(x = train_cond, y = trial_attempts)) + 
  geom_jitter()

trial_attempt_plot <- ggplot(data = aggregate(trial_attempts ~ train_cond + shj_cond +
  pid, mean, data = training_data), aes(x = shj_cond, y = trial_attempts, fill = train_cond)) + 
  geom_boxplot(outlier.colour = 'red') + 
  geom_jitter(alpha = .7, aes(color = train_cond), position = position_jitterdodge()) +
  geom_text(aes(label = ifelse(trial_attempts > 1.4, as.character(pid),'')), hjust = 2, vjust = 2) +
  scale_colour_manual('task', values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
	  axis.title.y = element_text(vjust = 1.5),
	  axis.title.x = element_text(vjust = -.5))

# # # # # # # # # SWITCH # # # # # # # # # # # # # # # # # # # # # # # #


switchcount_2 <- ggplot(data = aggregate(switch_count ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'switch' & shj_cond == 'two'))),
  aes(x = as.factor(block_num), y = switch_count)) + geom_violin() + 
  geom_jitter(alpha = .7, 
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) + 
  scale_x_discrete(name = 'Block Number') + 
  scale_y_continuous(name = 'Type II', limits = c(.99,3.01))

switchcount_3 <- ggplot(data = aggregate(switch_count ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'switch' & shj_cond == 'three'))),
  aes(x = as.factor(block_num), y = switch_count)) + geom_violin() + 
  geom_jitter(alpha = .7,
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) + 
  scale_x_discrete(name = 'Block Number') + 
  scale_y_continuous(name = 'Type III', limits = c(.99,3.01))

switchcount_4 <- ggplot(data = aggregate(switch_count ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'switch' & shj_cond == 'four'))),
  aes(x = as.factor(block_num), y = switch_count)) + geom_violin() + 
  geom_jitter(alpha = .7,
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) + 
  scale_x_discrete(name = 'Block Number') + 
  scale_y_continuous(name = 'Type IV', limits = c(.99,3.01))

multiplot(switchcount_2, 
          switchcount_3, 
          switchcount_4,  cols = 1)

# # overlay <-ggplot()


trial_x_attemptcount2 <- ggplot(data = aggregate(trial_attempts ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'switch' & shj_cond == 2))),
  aes(x = as.factor(block_num), y = trial_attempts)) + geom_violin() + 
  geom_jitter(alpha = .7) + scale_x_discrete(name = 'Block Number')
trial_x_attemptcount3 <- ggplot(data = aggregate(trial_attempts ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'switch' & shj_cond == 3))),
  aes(x = as.factor(block_num), y = trial_attempts)) + geom_violin() + 
  geom_jitter(alpha = .7) + scale_x_discrete(name = 'Block Number')
trial_x_attemptcount4 <- ggplot(data = aggregate(trial_attempts ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'switch' & shj_cond == 4))),
  aes(x = as.factor(block_num), y = trial_attempts)) + geom_violin() + 
  geom_jitter(alpha = .7) + scale_x_discrete(name = 'Block Number')
multiplot(trial_x_attemptcount2, 
          trial_x_attemptcount3, 
          trial_x_attemptcount4, cols = 1)

switch_test_plot <- ggplot(data = aggregate(test_acc ~ train_cond + shj_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'switch_test'))),
  aes(x = shj_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Switch Test Subject Means') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge()) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01)) 

# # # # # # # CLASSIFY # # # # # # # # # # # # # # # # # # # # # # # # # # #

trial_x_attemptcount2 <- ggplot(data = aggregate( ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'classify' & shj_cond == 'two'))),
  aes(x = as.factor(block_num), y = trial_attempts)) + geom_violin() +
  scale_y_continuous(limits = c(.99,1.7)) + geom_jitter(alpha = .7) + 
  scale_x_discrete(name = 'Block Number')

trial_x_attemptcount3 <- ggplot(data = aggregate(trial_attempts ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'classify' & shj_cond == 'three'))),
  aes(x = as.factor(block_num), y = trial_attempts)) + geom_violin() + 
  scale_y_continuous(limits = c(.99,1.7)) + geom_jitter(alpha = .7) + 
  scale_x_discrete(name = 'Block Number')

trial_x_attemptcount4 <- ggplot(data = aggregate(trial_attempts ~ block_num + train_cond + pid,
  mean, data = subset(training_data, subset = (train_cond == 'classify' & shj_cond == 'four'))),
  aes(x = as.factor(block_num), y = trial_attempts)) + geom_violin() + 
  scale_y_continuous(limits = c(.99,1.7)) + geom_jitter(alpha = .7) + 
  scale_x_discrete(name = 'Block Number')

multiplot(trial_x_attemptcount2, 
          trial_x_attemptcount3, 
          trial_x_attemptcount4, cols = 1)

# # # # # # # # trial attempts by block for both training conditions # # # # # # # # # # # # # # # #


trial_acc_line_plot_swi <- ggplot(data = aggregate(test_acc ~ block_num + train_cond +
  shj_cond, mean, data = subset(training_data, subset = (train_cond == 'switch' & trial_attempts == 1))), 
  aes(x = as.factor(block_num), y = trial_attempts, group = shj_cond, shape = shj_cond)) + geom_line() + 
  geom_point() + scale_x_discrete(name = 'Block Number') + 
  scale_y_continuous(limits = c(1,1.5), name = 'Switch Trial Attempts')


trial_acc_line_plot_cla <- ggplot(data = aggregate(trial_attempts ~ block_num + train_cond +
  shj_cond, mean, data = subset(training_data, subset = (train_cond == 'classify'))), 
  aes(x = as.factor(block_num), y = trial_attempts, group = shj_cond, shape = shj_cond)) + geom_line() + 
  geom_point() + scale_x_discrete(name = 'Block Number') + 
  scale_y_continuous(limits = c(1,1.5), name = 'Classify Trial Attempts')


multiplot(trial_acc_line_plot_swi, trial_acc_line_plot_cla, cols = 1)

# # # # # # # # trial accuracy by block for both training conditions # # # # # # # # # # # # # # # #


trial_acc_line_plot_swi <- ggplot(data = aggregate(test_acc ~ block_num + train_cond +
  shj_cond, mean, data = subset(training_data, subset = (train_cond == 'switch' & trial_attempts == 1))), 
  aes(x = as.factor(block_num), y = test_acc, group = shj_cond, shape = shj_cond)) + geom_line() + 
  geom_point() + scale_x_discrete(name = 'Block Number') + theme(legend.position="none") +
  scale_y_continuous(limits = c(.4, 1), name = 'Switch Trial Accuracy')


trial_acc_line_plot_cla <- ggplot(data = aggregate(test_acc ~ block_num + train_cond +
  shj_cond, mean, data = subset(training_data, subset = (train_cond == 'classify' & trial_attempts == 1))), 
  aes(x = as.factor(block_num), y = test_acc, group = shj_cond, shape = shj_cond)) + geom_line() + 
  geom_point() + scale_x_discrete(name = 'Block Number') + 
  scale_y_continuous(limits = c(.4, 1), name = 'Classify Trial Accuracy')


multiplot(trial_acc_line_plot_swi, trial_acc_line_plot_cla, cols = 2)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # inference viz by shj


inference_test_all <- ggplot(data = aggregate(test_acc ~ train_cond + shj_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'inference_test'))),
  aes(x = shj_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('All Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') +
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01)) # +
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = -.5) +

inference_test_part <- ggplot(data = aggregate(test_acc ~ train_cond + shj_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'inference_test' & eg_type == 'partial'))),
  aes(x = shj_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Inference Means - Partial Members') + 
  geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01)) # + 
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = -.5) +

inference_test_full <- ggplot(data = aggregate(test_acc ~ train_cond + shj_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'inference_test' & eg_type == 'full'))),
  aes(x = shj_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Complete Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01)) # +
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0)

multiplot(inference_test_all, 
          inference_test_part, 
          inference_test_full, cols = 3)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # inference viz collapsed

# # TRY FFIELDS FOR THIS

inference_test_all_coll <- ggplot(data = aggregate(test_acc ~ train_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'inference_test'))),
  aes(x = train_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('All Members') + geom_jitter(alpha = .9, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(name = 'Training Condition') +
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01))# + 
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0)

inference_test_part_coll <- ggplot(data = aggregate(test_acc ~ train_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'inference_test' & eg_type == 'partial'))),
  aes(x = train_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Inference Means - Partial Members') + 
  geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(name = 'Training Condition') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01))# + 
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = -.5)

inference_test_full_coll <- ggplot(data = aggregate(test_acc ~ train_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'inference_test' & eg_type == 'full'))),
  aes(x = train_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Complete Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(name = 'Training Condition') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
  axis.title.y = element_text(vjust = 1.5),
  axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01))# + 
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0)

multiplot(inference_test_all_coll, 
          inference_test_part_coll, 
          inference_test_full_coll, cols = 3)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # classify viz

classify_se <- summarySE(subset(datafull, 
  subset = (phase == 'classify_test' & is.finite(test_acc) == TRUE)),
  measurevar = "test_acc", groupvars = c("train_cond", "shj_cond"))


classify_test_all <- ggplot(data = aggregate(test_acc ~ train_cond + shj_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'classify_test'))),
  aes(x = shj_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('All Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01))# +  
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0, position = position_jitter())

classify_test_part <- ggplot(data = aggregate(test_acc ~ train_cond + shj_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'classify_test' & eg_type == 'partial'))),
  aes(x = shj_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Classify Means - Partial Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01))# +   
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0, position = position_jitter())

classify_test_full <- ggplot(data = aggregate(test_acc ~ train_cond + shj_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'classify_test' & eg_type == 'full'))),
  aes(x = shj_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Complete Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(limits = levels(datafull$shj_cond), name = 'Category Structure') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.01), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(0.00,1.01))# + 
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  # hjust = 0, vjust = 0, position = position_jitter()) +

multiplot(classify_test_all, 
          classify_test_part, 
          classify_test_full, cols = 3)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # classify viz collapsed

classify_test_all_coll <- ggplot(data = aggregate(test_acc ~ train_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'classify_test'))),
  aes(x = train_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('All Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(name = 'Training Condition') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(.03,1.00))# +   
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0, position = position_jitter(width = .1, height = .1)) +

classify_test_part_coll <- ggplot(data = aggregate(test_acc ~ train_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'classify_test' & eg_type == 'partial'))),
  aes(x = train_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Classify Means - Partial Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(name = 'Training Condition') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(.03,1.00))# +   
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0, position = position_jitter(width = .1, height = .1)) +

classify_test_full_coll <- ggplot(data = aggregate(test_acc ~ train_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'classify_test' & eg_type == 'full'))),
  aes(x = train_cond, y = test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('Complete Members') + geom_jitter(alpha = .7, aes(color = train_cond), 
    position = position_jitterdodge(jitter.width = 2.5, jitter.height = 0)) +
  scale_colour_manual('task', 
    values = c('classify' = 'blue','switch' = 'forest green')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'dark grey')) +
  scale_x_discrete(name = 'Training Condition') + 
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 2),
    axis.title.y = element_text(vjust = 1.5),
    axis.title.x = element_text(vjust = -.5)) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.1), name = 'Accuracy Mean') +
  coord_cartesian(ylim = c(.03,1.00))# +   
  #geom_text(aes(label = ifelse(test_acc < .30, as.character(pid),'')), 
  #  hjust = 0, vjust = 0, position = position_jitter(width = .1, height = .1))

multiplot(classify_test_part_coll, 
          classify_test_full_coll, cols = 2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

cond_names <- c('classify' = 'Classify', 'switch' = 'Switch')

cla_test_eg_plot <- ggplot(aggregate(test_acc ~ train_cond + eg_type + pid,
  mean, data = subset(datafull, subset = (phase == 'classify_test'))), 
  aes(eg_type, test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('') + geom_jitter(alpha = .7, 
    position = position_jitterdodge(jitter.width = 3, jitter.height = 0)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 4,
    show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = 'line', colour = 'black',  linetype = 1, size = 1.5, aes(group = 1)) +
  facet_grid(. ~ train_cond, labeller = as_labeller(cond_names)) +
  scale_colour_manual('task', guide = FALSE, values = c('classify' = 'black','switch' = 'black')) +
  scale_fill_manual('task',  guide = FALSE, values = c('classify' = 'light grey','switch' = 'light grey')) +
  scale_x_discrete(name = '', breaks = c('full', 'partial'), labels = c('Training', 'Incomplete')) + 
  theme(plot.title = element_text(size = 26, face = "bold", vjust = 2, colour = 'black'),
    axis.title.y = element_text(vjust = 4, size = 18, colour = 'black'),
    axis.text.y = element_text(size = 18, colour = 'black'),
    axis.text.x = element_text(vjust = -.5, size = 18, colour = 'black'),
    strip.text.x = element_text(size = 18, colour = 'black')) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Percent Correct') +
  coord_cartesian(ylim = c(.03,.97))


inf_test_eg_plot <- ggplot(aggregate(test_acc ~ train_cond + eg_type + pid,
  mean, data = subset(datafull, subset = (phase == 'inference_test'))), 
  aes(eg_type, test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('') + geom_jitter(alpha = .7, 
    position = position_jitterdodge(jitter.width = 3, jitter.height = 0)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 4,
    show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = 'line', colour = 'black',  linetype = 1, size = 1.5, aes(group = 1)) +
  facet_grid(.~train_cond, labeller = as_labeller(cond_names)) +
  scale_colour_manual('task', guide = FALSE, values = c('classify' = 'black','switch' = 'black')) +
  scale_fill_manual('task',  guide = FALSE, values = c('classify' = 'light grey','switch' = 'light grey')) +
  scale_x_discrete(name = '', breaks = c('full', 'partial'), labels = c('One\nFeature', 'Two\nFeatures')) + 
  theme(plot.title = element_text(size = 26, face = "bold", vjust = 2, colour = 'black'),
    axis.title.y = element_text(vjust = 4, size = 18, colour = 'black'),
    axis.text.y = element_text(size = 18, colour = 'black'),
    axis.text.x = element_text(vjust = -.5, size = 18, colour = 'black'),
    strip.text.x = element_text(size = 18, colour = 'black')) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Percent Correct') +
  coord_cartesian(ylim = c(.03,.97))


swi_test_eg_plot <- ggplot(aggregate(test_acc ~ train_cond + pid,
  mean, data = subset(datafull, subset = (phase == 'switch_test'))), 
  aes(train_cond, test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  ggtitle('') + geom_jitter(alpha = .7, 
    position = position_jitterdodge(jitter.width = 1.5, jitter.height = 0)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 4,
    show.legend = FALSE) +
  scale_colour_manual('task', values = c('classify' = 'black','switch' = 'black')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'light grey')) +
  scale_x_discrete(name = '', breaks = c('classify', 'switch'), labels = c('Classify', 'Switch')) + 
  theme(plot.title = element_text(size = 26, face = "bold", vjust = 2, colour = 'black'),
    axis.title.y = element_text(vjust = 4, size = 18, colour = 'black'),
    axis.text.y = element_text(size = 18, colour = 'black'),
    axis.text.x = element_text(vjust = -.5, size = 18, colour = 'black'),
    strip.text.x = element_text(size = 18, colour = 'black'),
    legend.position = 'none') +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Percent Correct') +
  coord_cartesian(ylim = c(.03,.97))

multiplot(cla_test_eg_plot, inf_test_eg_plot, cols = 1)
