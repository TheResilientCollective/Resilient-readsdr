test_that("array_equations() returns the expected list", {

  dims_obj  <- list(global_dims = list(Age = c("1", "2")),
                    dictionary  = list(I = c("Age")))

  vendor    <- "isee"
  dim_names <- "Age"

  aux_obj <- list(name     = "I_to_R",
                  equation = "par_gamma*I")

  actual <- array_equations(aux_obj, dims_obj, dim_names, vendor)

  expected <- list(equations  = c("par_gamma*I_1", "par_gamma*I_2"),
                    are_const  = c(FALSE, FALSE),
                    elems      = c("1", "2"))

  expect_equal(actual, expected)

  dims_obj <- list(global_dims = list(Age = 1:2),
                   dictionary  = list(lambda = "Age",
                                      S      = "Age"))

  aux_obj <- list(name     = "S_to_I",
                  equation = "lambda*S")

  actual <- array_equations(aux_obj, dims_obj, dim_names, vendor)

  expected <- list(equations  = c("lambda_1*S_1", "lambda_2*S_2"),
                   are_const  = c(FALSE, FALSE),
                   elems      = c("1", "2"))

  expect_equal(actual, expected)
})

test_that("devectorise_equation() returns the expected output", {

  dims_list     <- list(Region = c("Westeros", "Essos"),
                       Age    = c("Young", "Old"))

  raw_equation <- "Population[Region,Age] * growth_rate[Region,Age]"
  actual       <- devectorise_equation(raw_equation, dims_list)

  expected <- c("Population_Westeros_Young * growth_rate_Westeros_Young",
                "Population_Westeros_Old * growth_rate_Westeros_Old",
                "Population_Essos_Young * growth_rate_Essos_Young",
                "Population_Essos_Old * growth_rate_Essos_Old")

  expect_equal(actual, expected)
})

# dimension_extensions()--------------------------------------------------------

test_that("dimension_extensions() returns simple extension for one dimension", {

  dims_obj  <- list(global_dims = list(Happiness = c("content", "elated")),
                    dictionary  = list(Sam = c("Happiness")))

  expected <- tibble::tibble(name=c("Sam", "Sam"), ext=c("content", "elated"))
  actual <- dimension_extensions(dims_obj)
  expect_equal(expected, actual)
})

test_that("dimension_extensions() handles multiple dimensions and respects order", {

  dims_obj  <- list(global_dims = list(Happiness = c("content", "elated"),
                                       Health = c("sick", "well")),
                    dictionary  = list(Sam = c("Happiness", "Health"),
                                       Jo = c("Health", "Happiness")))

  expected <- tibble::tibble(
    name=c("Sam", "Sam", "Sam", "Sam", "Jo", "Jo", "Jo", "Jo"),
    ext=c("content_sick", "content_well", "elated_sick", "elated_well",
          "sick_content", "sick_elated", "well_content", "well_elated"))
  actual <- dimension_extensions(dims_obj)
  expect_equal(expected, actual)
})

test_that("dimension_extensions() handles nodes with different numbers of dimensions", {

  dims_obj  <- list(global_dims = list(Happiness = c("content"),
                                       Health = c("sick", "well"),
                                       Height = c("short", "medium", "tall")),
                    dictionary  = list(Sam = c("Happiness"),
                                       Jo = c("Health", "Happiness"),
                                       Sal = c("Happiness", "Height", "Health")))

  expected <- tibble::tibble(
    name=c("Sam", "Jo", "Jo", "Sal", "Sal", "Sal", "Sal", "Sal", "Sal"),
    ext=c("content",
          "sick_content", "well_content",
          "content_short_sick", "content_short_well", "content_medium_sick",
          "content_medium_well", "content_tall_sick", "content_tall_well"))
  actual <- dimension_extensions(dims_obj)
  expect_equal(expected, actual)
})

test_that("dimension_extensions() returns empty tibble if input is null", {
  expected <- tibble::tibble(name=character(), ext=character())
  actual <- dimension_extensions(NULL)
  expect_equal(expected, actual)
})

test_that("dimension_extensions() returns empty tibble if input missing dictionary", {
  dims_obj  <- list(global_dims = list(Happiness = c("content", "elated")))
  expected <- tibble::tibble(name=character(), ext=character())
  actual <- dimension_extensions(dims_obj)
  expect_equal(expected, actual)
})

test_that("dimension_extensions() returns empty tibble if input missing global_dims", {
  dims_obj  <- list(dictionary  = list(Sam = c("Happiness")))
  expected <- tibble::tibble(name=character(), ext=character())
  actual <- dimension_extensions(dims_obj)
  expect_equal(expected, actual)
})

# add_equation_extensions()-----------------------------------------------------
test_that("add_equation_extensions() handles different variable names in dmx", {
  dmx <- tibble::tibble(
    name=c("v1", "v1", "v2", "v2", "v3", "v4", "v4", "v4"),
    ext=c("e1", "e2", "e1", "e2", NA, "e1", "e2", "e3")
  )
  input <- tibble::tibble(
    name=c("vx1", "vx1", "vx2", "vx2", "vx3", "vx4", "vx4", "vx4"),
    ext=c("e1", "e2", "e1", "e2", NA, "e1", "e2", "e3"),
    eqn=c("0", "1", "v1", "v1", "2", "v1*v2+v3", "v1*v2+v3", "v1*v2+v3")
  )

  expected <- tibble::tibble(
    name=c("vx1", "vx1", "vx2", "vx2", "vx3", "vx4", "vx4", "vx4"),
    ext=c("e1", "e2", "e1", "e2", NA, "e1", "e2", "e3"),
    eqn=c("0", "1", "v1_e1", "v1_e2", "2", "v1_e1*v2_e1+v3", "v1_e2*v2_e2+v3", "v1*v2+v3")
  )

  actual <- input %>% add_equation_extensions(eqn, dmx)
  expect_equal(expected, actual)
})

test_that("add_equation_extensions() handles no dmx", {
  input <- tibble::tibble(
    name=c("v1", "v1", "v2", "v2", "v3", "v4", "v4", "v4"),
    ext=c("e1", "e2", "e1", "e2", NA, "e1", "e2", "e3"),
    eqn=c("0", "1", "v1", "v1", "2", "v1*v2+v3", "v1*v2+v3", "v1*v2+v3")
  )

  expected <- tibble::tibble(
    name=c("v1", "v1", "v2", "v2", "v3", "v4", "v4", "v4"),
    ext=c("e1", "e2", "e1", "e2", NA, "e1", "e2", "e3"),
    eqn=c("0", "1", "v1_e1", "v1_e2", "2", "v1_e1*v2_e1+v3", "v1_e2*v2_e2+v3", "v1*v2+v3")
  )

  actual <- input %>% add_equation_extensions(eqn)
  expect_equal(expected, actual)
})
