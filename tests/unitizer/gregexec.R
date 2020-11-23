unitizer_sect('basic tests', {
  nums <- c('555-233-1290 555-241-2150', '555-123-1234', 'hello world')
  dat <- svgchop:::gregexec("([0-9]+)-", nums, perl=TRUE)
  regmatches(nums, dat)
})
