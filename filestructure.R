fs <- list.files(all.files=T, full.names=T, recursive=T)

# exclude any invisible files

library(data.table)

files = data.table(full_filepath = fs)
files[, full_filepath := gsub('^[.][/]','',full_filepath)]

files[, filename:=sapply(full_filepath, function(x) rev(unlist(strsplit(x, '/')))[[1]])]
files[, index:=1:.N]
files[, path:=gsub(filename, '', full_filepath), by = 'index']
files[, index:=NULL]



# define files that need to be excluded from the fill tree (e.g., system-internal files)
files[, exclude:=F]
files[grepl('^[.]git', full_filepath), exclude:=T]

# define confidential files (i.e., files that will not be shared/cannot be shared)
files[, confidential:=F]
files[grepl('^[.]confidential', filename), confidential:=T]
files[grepl('^data', full_filepath), confidential:=T]

# any output/temp/audit files (as they contain stats from the original data)
files[grepl('temp[/]|[audit/]|[/]output', full_filepath), confidential:=T]

# include file info
info = file.info(files$full_filepath)

files <- cbind(files, info)

# remove unnecessary content
files[, ':=' (isdir=NULL, mode=NULL,exe=NULL)]

setcolorder(files, c('path', 'filename', 'full_filepath', 'exclude', 'confidential', 'ctime', 'mtime','atime'))
setnames(files, 'mtime', 'timestamp_modified')
setnames(files, 'ctime', 'timestamp_created')
setnames(files, 'atime', 'timestamp_last_accessed')

# generate tree
files[exclude==F]

setorder(files, path, filename, timestamp_created)

dir.create('docs')
fwrite(files[exclude==F][, exclude:=NULL], file = 'docs/files_in_repository.csv')

# create copy

