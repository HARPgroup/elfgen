context("elfgen-example")

# This is an example test
test_that(
	"Checking input of 10",
	{
		expect_equal(elfgen_example(10), 20)
	}
)
