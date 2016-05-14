library(tikzDevice)
library(ggplot2)
library(gtable)
library(grid)
 
setwd('C://Users//garre//Dropbox//aa projects//switchrite//data//')
datafile <- 'switchrite_final.csv'
datafull <- read.csv(datafile, stringsAsFactors = FALSE)
training_data <- subset(datafull, 
  subset = (phase == 'classify_train' | phase == 'switch_train'))

cond_names <- c('classify' = 'Classify', 'switch' = 'Switch')

tikz('classify_test.tex', width = 3.3, height = 3, pointsize = 10, sanitize = TRUE)

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
    plot.margin = unit(c(0,0,.1,0), "cm")) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Proportion Correct') +
  coord_cartesian(ylim = c(.03,.97))

dev.off()


tikz('inference_test.tex', width = 3.3, height = 3, pointsize = 10, sanitize = TRUE)

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
  scale_x_discrete(name = '', breaks = c('full', 'partial'), labels = c('Two\nFeatures', 'One\nFeature')) + 
  theme(plot.title = element_blank(),
    axis.title.y = element_text(size = 9.5, colour = 'black'),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = 'black'),
    axis.text.x = element_text(size = 9.5, colour = 'black'),
    strip.text.x = element_text(size = 10, colour = 'black'), 
    plot.margin = unit(c(0,0,.1,0), "cm")) +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Proportion Correct') +
  coord_cartesian(ylim = c(.03,.97))

dev.off()


tikz('switch_test.tex', width = 3, height = 2.2, pointsize = 10, sanitize = TRUE)

ggplot(aggregate(test_acc ~ train_cond + pid, mean, 
    data = subset(datafull, subset = (phase == 'switch_test'))), 
  aes(train_cond, test_acc, fill = train_cond)) + geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitterdodge(jitter.width = 1.75, jitter.height = 0), 
    alpha = .7, size = .75) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, colour = 'black', size = 2,
    show.legend = FALSE) +
  facet_wrap(~ train_cond, labeller = as_labeller(cond_names), scales = 'free_x') +
  scale_colour_manual('task', values = c('classify' = 'black','switch' = 'black')) +
  scale_fill_manual('task', values = c('classify' = 'light grey','switch' = 'light grey')) +
  scale_x_discrete(name = '', breaks = c('classify', 'switch'), labels = c('Classify', 'Switch')) + 
  theme(plot.title = element_blank(),
    axis.title.y = element_text(size = 9.5, colour = 'black'),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, colour = 'black'),
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = 10, colour = 'black'),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    plot.margin = unit(c(0,0,.1,0), "cm"),
    legend.position = 'none') +
  scale_y_continuous(limits = c(0,1.00), breaks = seq(0, 1, 0.2), name = 'Mean Proportion Correct') +
  coord_cartesian(ylim = c(.03,.97))

dev.off()


training_data$shj_cond <- factor(training_data$shj_cond, c("two","three","four"))
levels(training_data$shj_cond) <- c('II', 'III', 'IV')

tikz('training_fig.tex', width = 7.5, height = 3, pointsize = 10, sanitize = TRUE)

acc_x_block_plot_swi <- ggplot(data = aggregate(var_19 ~ block_num + shj_cond + pid,
    mean, data = subset(training_data, 
      subset = (trial_attempts == 1 & train_cond == 'switch'))), 
  aes(x = as.factor(block_num), y = var_19, fill = shj_cond)) + 
  geom_boxplot(outlier.colour = NA, lwd = 1, fatten = 1) +
  scale_x_discrete(name = '') + scale_y_continuous(name = 'Switch', 
    limits = c(0,1.01), breaks = seq(0, 1, .25),) +
  scale_fill_brewer(name = 'Category\nStructure', palette = 'Purples') +
  theme(axis.text.y = element_text(size = 8, color = 'black'), 
    axis.text.x = element_blank(),
    axis.title.x = element_text(size = 10, color = 'black'), 
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 10),
    plot.margin = unit(c(.2,0,0,0), "cm"))

acc_x_block_plot_cla <- ggplot(data = aggregate(var_19 ~ block_num + shj_cond + pid,
    mean, data = subset(training_data, 
      subset = (trial_attempts == 1 & train_cond == 'classify'))), 
  aes(x = as.factor(block_num), y = var_19, fill = shj_cond)) + 
  geom_boxplot(outlier.colour = NA, lwd = 1, fatten = 1) +
  scale_x_discrete(name = 'Block Number') + scale_y_continuous(name = 'Classify', 
    limits = c(0,1.01), breaks = seq(0, 1, .25),) +
  scale_fill_brewer(name = 'Category\nStructure', palette = 'Purples') +
  theme(axis.text.y = element_text(size = 8, color = 'black'), 
    axis.text.x = element_text(size = 8, color = 'black'),
    axis.title.x = element_text(size = 10, color = 'black'), 
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 10),
    plot.margin = unit(c(.2,0,0,0), "cm")) 

gt1 <- ggplot_gtable(ggplot_build(acc_x_block_plot_swi))
gt2 <- ggplot_gtable(ggplot_build(acc_x_block_plot_cla))

gt <- gtable(widths = unit(c(4, 1), "null"), height = unit(c(1, 1), "null"))
gt <- gtable_add_grob(gt, gt1[, -5], 1, 1)
gt <- gtable_add_grob(gt, gt2[, -5], 2, 1)
gt <- gtable_add_grob(gt, gt1[, 5], 1, 2, 2)

grid.newpage()
grid.draw(gt)

dev.off()









