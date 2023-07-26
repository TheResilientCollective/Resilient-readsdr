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
			    </aux>
			    <aux name="growth rate proxy">
				    <eqn>growth_rate</eqn>
			    </aux>
			    <aux name="dimensional">
    				<doc>A dimensional constant</doc>
		    		<dimensions>
				    	<dim name="Type Of Dimension"/>
				    </dimensions>
    				<element subscript="Dimension One">
		    			<eqn>7</eqn>
				    </element>
    				<element subscript="Dimension Two">
    					<eqn>23</eqn>
		    		</element>
    				<units>Some Units</units>
    			</aux>
        </variables>
      </doc1>
    </root>'

test_that("read_xmile_constants() returns a list", {
  expect_type(read_xmile_constants(test_model), "list")
})

test_that("read_xmile_constants() list has names xmile and constants", {
  expect_named(read_xmile_constants(test_model),c("xmile", "constants"))
})

test_that("read_xmile_constants() returned xmile object is xml", {
  expect_s3_class(read_xmile_constants(test_model)$xmile, "xml_document")
})

test_that("read_xmile_constants() returned constants object is data frame", {
  expect_s3_class(read_xmile_constants(test_model)$constants, "data.frame")
})

test_that("read_xmile_constants() does not pick up variable or stock", {
  expect_equal(nrow(read_xmile_constants(test_model)$constants), 3)
})

test_that("read_xmile_constants() extracts single constant", {
  const <- read_xmile_constants(test_model)$constants %>%
    dplyr::filter(name == "growth rate")
  expect_equal(0.01, const$value[1])
  expect_true(is.na(const$dimensions))
  expect_true(is.na(const$subscript))
  expect_true(is.na(const$units))
  expect_true(is.na(const$doc))
})

test_that("read_xmile_constants() extracts dimensions", {
  df <- data.frame(list(name="dimensional",
                        dimensions="Type Of Dimension",
                        subscript=c("Dimension One", "Dimension Two"),
                        value=c(7, 23),
                        units="Some Units",
                        doc="A dimensional constant"))
  const <- read_xmile_constants(test_model)$constants
  expect_equal(df, data.frame(const %>% dplyr::filter(name == "dimensional")))
})
