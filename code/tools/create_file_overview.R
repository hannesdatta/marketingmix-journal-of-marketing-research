### CREATE DIRECTORY/FILE TREE FROM PROJECT
### FOR DOCUMENTATION PURPOSES

cat('Creating directory/file tree from project... please wait.', fill = T)


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
files[grepl('[.]gitignore', full_filepath), exclude:=F]
files[grepl('dataverse_log.json', full_filepath), exclude:=T]

files <- files[exclude==F][, exclude:=NULL]


# define confidential files (i.e., files that will not be shared/cannot be shared)
files[, confidential:=F]
files[grepl('^[.]confidential', filename), confidential:=T]
files[grepl('^data', full_filepath), confidential:=T]

# any output/temp/audit files (as they contain stats from the original data)
files[grepl('temp[/]|audit[/]|output[/]|externals[/]|external[/]', full_filepath), confidential:=T]
files[grepl('readme', filename, ignore.case=T), confidential:=F]
files[grepl('penn_worldtables', full_filepath), confidential:=F]

files[grepl('^datareport', full_filepath), confidential:=F]


# include file info
info = file.info(files$full_filepath)

files <- cbind(files, info)

# remove unnecessary content
files[, ':=' (isdir=NULL, mode=NULL,exe=NULL)]

setcolorder(files, c('path', 'filename', 'full_filepath', 'confidential', 'ctime', 'mtime','atime'))
setnames(files, 'mtime', 'timestamp_modified')
setnames(files, 'ctime', 'timestamp_created')
setnames(files, 'atime', 'timestamp_last_accessed')

setorder(files, path, filename, timestamp_created)

dir.create('docs')
fwrite(files, file = 'docs/files_in_repository.csv')

