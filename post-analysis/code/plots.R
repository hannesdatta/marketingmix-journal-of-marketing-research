# generate plots for slides, w/ focus on China


libs_to_load <- c('data.table', 'stargazer', 'lme4', 'car', 'kableExtra', 'stringi', 'ggplot2', 'ggpubr', 'plyr')
for (lib in libs_to_load) eval(parse(text=paste('library(', lib, ')')))

# Load data
elast <- fread('../externals/elast_results_nov12sh.csv')

require(lattice)
require(latticeExtra)

tmp = elast[, list(mean=mean(elast)),by=c('country')]


barchart(elast ~ country, data = elast)


ggplot(data=elast, aes(x=country, y=elast))
