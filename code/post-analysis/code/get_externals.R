unlink('../externals/*')
unlink('../temp/*')
unlink('../output/*')

dir.create('../externals')
dir.create('../temp')
dir.create('../output')

copy_from_to <- function(pattern, from, to) {
  for (f in list.files(pattern = pattern, path = from, recursive = TRUE)) {
    cat(paste0("Copying ", f, "...\n"))
    file.copy(paste0(from, f), paste0(to, f))
  }
}

copy_from_to('preclean*.*', '../../analysis/temp/', '../externals/')
copy_from_to('*.csv', '../../analysis/output/', '../externals/')
copy_from_to('*.txt', '../../analysis/output/', '../externals/')
copy_from_to('*.html', '../../analysis/output/', '../externals/')

sink('../externals/get_externals.txt')
cat('done.')
sink()