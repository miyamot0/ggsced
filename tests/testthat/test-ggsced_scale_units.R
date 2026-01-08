test_that("multiplication works", {
  session_num = 5
  domain_size = c(0, 20)

  expect_equal(ggsced_scale_units(session_num, domain_size),
               0.25)
})
