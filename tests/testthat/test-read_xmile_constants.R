context("Read xmile file for constants")

test_model <-
  '<root>
      <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
        <header>
		      <vendor>isee systems, inc.</vendor>
		    </header>
	      <sim_specs>
	        <start>0</start>
		      <stop>4</stop>
		      <dt reciprocal="true">4</dt>
	      </sim_specs>
	   	  <variables>
			    <stock name="population">
				    <eqn>100</eqn>
				    <inflow>net_growth</inflow>
			    </stock>
			    <flow name="net growth">
				    <eqn>population * growth_rate</eqn>
			    </flow>
			    <aux name="growth rate">
				    <eqn>0.01</eqn>
				    <doc>Growth Rate Constant</doc>
			    </aux>
			    <aux name="growth rate proxy">
				    <eqn>growth_rate</eqn>
			    </aux>
        </variables>
      </doc1>
    </root>'

test_that("the output from read_xmile_constants() is a data frame", {
  expect_s3_class(read_xmile_constants(test_model), "data.frame")
})

test_that("read_xmile_constants() column names are correct", {
  expect_named(
    read_xmile_constants(test_model),
    c("name", "dimensions", "subscript", "value", "units", "doc"))
})

test_that("read_xmile_constants() does not pick up variable", {
  expect_equal(nrow(read_xmile_constants(test_model)), 1)
})
