context("General utilities")

# Function Call Extractor=======================================================
test_that("extract_function_call() returns NA if not found", {
  expect_equal(extract_function_call("abc()", "def"), NA)
})

test_that("extract_function_call() returns NA if not a function", {
  expect_equal(extract_function_call("def", "def"), NA)
})

test_that("extract_function_call() returns NA if partial function name", {
  expect_equal(extract_function_call("undef()", "def"), NA)
})

test_that("extract_function_call() finds function no args", {
  expect_equal(
    extract_function_call("def()", "def"),
    list(match="def()", args="")
  )
})

test_that("extract_function_call() finds function simple", {
  expect_equal(
    extract_function_call("def(one, two)", "def"),
    list(match="def(one, two)", args="one, two")
  )
})

test_that("extract_function_call() finds function complex", {
  line <- "one foo(ab(),cde|;!@#$%^&*(_+),f(g(hi))) jkl() bp(asdf())"
  expect_equal(
    extract_function_call(line, "foo"),
    list(match="foo(ab(),cde|;!@#$%^&*(_+),f(g(hi)))",
         args="ab(),cde|;!@#$%^&*(_+),f(g(hi))")
  )
})
