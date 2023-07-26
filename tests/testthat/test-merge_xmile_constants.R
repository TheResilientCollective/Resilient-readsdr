context("Merge constants back into xmile")

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

test_that("merge_xmile_constants() does nothing with no change", {
  parsed <- read_xmile_constants(test_model)
  check_model <- as.character(parsed$xmile)
  expect_equal(
    check_model,
    as.character(merge_xmile_constants(parsed$xmile, parsed$constants))
  )
})

test_that("merge_xmile_constants() merges a single constant", {
  parsed <- read_xmile_constants(test_model)
  check_model <- sub("0.01", "1.23", as.character(parsed$xmile))
  df_mod <- parsed$constants %>%
    dplyr::mutate(value = ifelse(name=="growth rate", "1.23", value))
  expect_equal(
    check_model,
    as.character(merge_xmile_constants(parsed$xmile, df_mod))
  )
})


test_that("merge_xmile_constants() merges a single dimension subscript", {
  parsed <- read_xmile_constants(test_model)
  check_model <- sub("23", "17", as.character(parsed$xmile))
  df_mod <- parsed$constants %>%
    dplyr::mutate(
      value = ifelse(
        !is.na(subscript) & (subscript=="Dimension Two"), "17", value
      )
    )
  expect_equal(
    check_model,
    as.character(merge_xmile_constants(parsed$xmile, df_mod))
  )
})
