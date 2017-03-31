# Preamble
	require(data.table)
	require(lattice)
	require(latticeExtra)
	load(file='..\\temp\\attributes.RData')
	
#############################################
# Table of product attributes per category  #
# and summary stats  						#
#############################################

sink('../audit/attributes.txt')	
for (i in 1:length(attribs)) {
	cat('////////////////////////////////////////////////////\n')
	cat(paste0('Category: ', names(attribs)[i],'\n'))
	cat('////////////////////////////////////////////////////\n')
	
	attr_cols = colnames(attribs[[i]])[grepl('attr_', colnames(attribs[[i]]))]
	
	for (coln in attr_cols) {
		cat(paste0('\nAttribute: ', coln,'\n'))
		if (eval(parse(text=paste0('class(attribs[[i]]$', coln,')')))=='factor') {
			print(table(attribs[[i]][,coln,with=F]))
			} else {
			print(summary(attribs[[i]][,coln,with=F]))
			}
		
		}
	cat('\n\n\n\n\n\n\n\n')
	}
			
sink()
		


