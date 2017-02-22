context('Test the makeGAM function')


test_that('makeGAM works with all correct argument options', {
  
  expect_error(makeGAM(letters[1:3]), NA)
  expect_error(makeGAM(letters[1:3], response = 'Rate'), NA)
  
  expect_error(makeGAM(letters[1:3], invariant = '0 + i'), NA)
  expect_error(makeGAM(letters[1:3], linear = TRUE), NA)
  expect_error(makeGAM(letters[1:3], linear = FALSE), NA)
  expect_error(makeGAM(letters[1:3], linear = letters[5:6]), NA)
  
  
  expect_error(makeGAM(letters[1:3], returnstring = TRUE), NA)
  expect_error(makeGAM(letters[1:3], returnstring = FALSE), NA)
  
  expect_true(is(makeGAM(letters[1:3], returnstring = FALSE), 'formula'))
  expect_true(is(makeGAM(letters[1:3], returnstring = TRUE), 'character'))
  
  
  
})

test_that('makeGAM returns errors when it should', {
  
  expect_error(makeGAM())
  expect_error(makeGAM(1:4))
  expect_error(makeGAM(letters[1:3], response = 1))
  expect_error(makeGAM(letters[1:3], response = letters[5:6]))
  expect_error(makeGAM(letters[1:3], invariant = 1))
  expect_error(makeGAM(letters[1:3], invariant = letters[5:6]))
  expect_error(makeGAM(letters[1:3], linear = 1:4))
  expect_error(makeGAM(letters[1:3], returnstring = 'awdwd'))

})
