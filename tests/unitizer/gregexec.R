unitizer_sect('basic tests', {
  nums <- c('555-233-1290 555-241-2150', '555-123-1234', 'hello world')
  dat <- svgchop:::gregexec("([0-9]+)-", nums, perl=TRUE)
  regmatches(nums, dat)

  svgchop:::gregexec("([0-9]+)-", character(), perl=TRUE)
  svgchop:::gregexec("([0-9]+)-", NA, perl=TRUE)
  svgchop:::gregexec(character(), c('hello', 'world'), perl=TRUE)
  svgchop:::gregexec(NA, c('hello', 'world'), perl=TRUE)

  x <- c('hello', 'world')
  m <- svgchop:::gregexec('hello', x, perl=TRUE)
  regmatches(x, m)
})
