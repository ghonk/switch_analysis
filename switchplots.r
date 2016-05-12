library(tikzDevice)
library(ggplot2)

setwd('C://Users//garre//Dropbox//aa projects//switchrite//data//')
datafile <- 'switchrite_final.csv'
datafull <- read.csv(datafile, stringsAsFactors = FALSE)
training_data <- subset(datafull, 
  subset = (phase == 'classify_train' | phase == 'switch_train'))

cond_names <- c('classify' = 'Classify', 'switch' = 'Switch')

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


tikz('classify_test.tex', width=3.7, height=3.6, pointsize=10, sanitize = TRUE)

ggplot(aggregate(test_acc ~ train_cond + eg_type + pid, mean, 
    data = subset(datafull, subset = (phase == 'classify_test'))), 
  aes(eg_type, test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 1.75, jitter.height = 0), 
    alpha = .7, size = .75) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 2,
    show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = 'line', colour = 'black',  linetype = 1, 
    size = 1.5, aes(group = 1)) +
  facet_grid(. ~ train_cond, labeller = as_labeller(cond_names)) +
  scale_colour_manual('task', guide = FALSE, values = c('classify' = 'black','switch' = 'black')) +
  scale_fill_manual('task',  guide = FALSE, values = c('classify' = 'light grey','switch' = 'light grey')) +
  scale_x_discrete(name = '', breaks = c('full', 'partial'), labels = c('Training', 'Incomplete')) + 
  theme(plot.title = element_blank(),
    axis.title.y = element_text(size = 9.5, colour = 'black'),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = 'black'),
    axis.text.x = element_text(size = 9.5, colour = 'black'),
    strip.text.x = element_text(size = 10, colour = 'black'), 
    plot.margin = unit(c(0,1,.1,0), "cm")) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Proportion Correct') +
  coord_cartesian(ylim = c(.03,.97))

dev.off()


tikz('inference_test.tex', width=3.7, height=3.6, pointsize=10, sanitize = TRUE)

ggplot(aggregate(test_acc ~ train_cond + eg_type + pid, mean, 
    data = subset(datafull, subset = (phase == 'inference_test'))), 
    aes(eg_type, test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 1.75, jitter.height = 0), 
    alpha = .7, size = .75) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 2,
    show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = 'line', colour = 'black',  linetype = 1, 
    size = 1.5, aes(group = 1)) +
  facet_grid(. ~ train_cond, labeller = as_labeller(cond_names)) +
  scale_colour_manual('task', guide = FALSE, values = c('classify' = 'black','switch' = 'black')) +
  scale_fill_manual('task',  guide = FALSE, values = c('classify' = 'light grey','switch' = 'light grey')) +
  scale_x_discrete(name = '', breaks = c('full', 'partial'), labels = c('One\nFeature', 'Two\nFeatures')) + 
  theme(plot.title = element_blank(),
    axis.title.y = element_text(size = 9.5, colour = 'black'),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = 'black'),
    axis.text.x = element_text(size = 9.5, colour = 'black'),
    strip.text.x = element_text(size = 10, colour = 'black'), 
    plot.margin = unit(c(0,1,.1,0), "cm")) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Proportion Correct') +
  coord_cartesian(ylim = c(.03,.97))

dev.off()


tikz('switch_test.tex', width=3.7, height=3.6, pointsize=10, sanitize = TRUE)

ggplot(aggregate(test_acc ~ train_cond + pid, mean, 
    data = subset(datafull, subset = (phase == 'switch_test'))), 
  aes(train_cond, test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 1.75, jitter.height = 0), 
    alpha = .7, size = .75) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 2,
    show.legend = FALSE) +
  scale_colour_manual('task', values = c('classify' = 'black','switch' = 'black')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'light grey')) +
  scale_x_discrete(name = '', breaks = c('classify', 'switch'), labels = c('Classify', 'Switch')) + 
  theme(plot.title = element_blank(),
    axis.title.y = element_text(size = 9.5, colour = 'black'),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = 'black'),
    axis.text.x = element_text(size = 9.5, colour = 'black'),
    strip.text.x = element_text(size = 10, colour = 'black'), 
    plot.margin = unit(c(0,1,.1,0), "cm"),
    legend.position = 'none') +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Proportion Correct') +
  coord_cartesian(ylim = c(.03,.97))

dev.off()


tikz('training_fig.tex', width = 7, height = 3.5, pointsize = 10, sanitize = TRUE)

training_data$shj_cond <- factor(training_data$shj_cond, c("two","three","four"))

acc_x_block_plot_swi <- ggplot(data = aggregate(var_19 ~ block_num + shj_cond + pid,
    mean, data = subset(training_data, 
      subset = (trial_attempts == 1 & train_cond == 'switch'))), 
  aes(x = as.factor(block_num), y = var_19, fill = shj_cond)) + 
  geom_boxplot(outlier.colour = NA, lwd = 1, fatten = 1) +
  scale_x_discrete(name = '') + scale_y_continuous(name = 'Switch', 
    limits = c(0,1.01), breaks = seq(0, 1, .25),) +
  scale_fill_brewer('Category Type', palette = 'Purples') +
  theme(axis.text.y = element_text(size = 8, color = 'black'), axis.text.x = element_blank(),
    axis.title.y = element_text(size = 10), legend.title = element_blank(), 
    legend.text = element_text(size = 16), 
    legend.position = 'none',
    plot.margin = unit(c(0,0,0,0), "cm")) 

acc_x_block_plot_cla <- ggplot(data = aggregate(var_19 ~ block_num + shj_cond + pid,
    mean, data = subset(training_data, 
      subset = (trial_attempts == 1 & train_cond == 'classify'))), 
  aes(x = as.factor(block_num), y = var_19, fill = shj_cond)) + 
  geom_boxplot(outlier.colour = NA, lwd = 1, fatten = 1) +
  scale_x_discrete(name = '') + scale_y_continuous(name = 'Classify', 
    limits = c(0,1.01), breaks = seq(0, 1, .25),) +
  scale_fill_brewer('Category Type', palette = 'Purples', breaks = c('II', 'III', 'IV')) +
  theme(axis.text.y = element_text(size = 8, color = 'black'), 
    axis.text.x = element_text(size = 8, color = 'black'), 
    axis.title.y = element_text(size = 10, vjust = 0.5), 
    legend.text = element_text(size = 16), 
    legend.position = 'none', 
    plot.margin = unit(c(0,0,0,0), "cm")) 

multiplot(acc_x_block_plot_swi,
          acc_x_block_plot_cla, cols = 1)

dev.off()