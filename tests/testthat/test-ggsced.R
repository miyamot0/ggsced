test_that("Asserts empty", {
  library(ggplot2)

  test_data_frame = data.frame(
    Participant = c("A", "B", "C"),
    x = c(1, 2, 3),
    y = c(0, 0, 0)
  )

  p = ggplot(test_data_frame, aes(x, y)) +
    facet_wrap(~Participant)

  staggered_pls = list(
    '1' = c(1.5,  2.5, 3.5)
  )

  expect_no_error(gg_sced(p, legs = staggered_pls))
  expect_no_warning(gg_sced(p, legs = staggered_pls))
  expect_no_message(gg_sced(p, legs = staggered_pls))
  expect_message(gg_sced(p, legs = staggered_pls, verbose = TRUE))

})
