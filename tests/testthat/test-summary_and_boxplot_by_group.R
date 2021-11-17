test_data <- tibble::tibble(group = c("A", "B", "C", "A", "C", "A", "B"),
                     value = c(1, 2, 3, 4, 5, NA, 6),
                     id = c("1", "2", "3", "4", "5", "6", "7"))
# test 1
test_that("Test if summary statistics are correct when removing NA values", {
          expected_1 <- tibble::tibble(group = c("A", "B", "C"),
                                       min = c(1, 2, 3),
                                       max = c(4, 6, 5),
                                       mean = c(2.5, 4.0, 4.0),
                                       median = c(2.5, 4.0, 4.0),
                                       n = c(3, 2, 2))
          expect_equal(summary_and_boxplot_by_group(test_data, group, value)[[1]], expected_1)})

#  test 2
test_that("Test if summary statistics are correct when keeping NA values", {
          expected_2 <- tibble::tibble(group = c("A", "B", "C"),
                                       min = c(NA, 2, 3),
                                       max = c(NA, 6, 5),
                                       mean = c(NA, 4.0, 4.0),
                                       median = c(NA, 4.0, 4.0),
                                       n = c(3, 2, 2))
          expect_equal(summary_and_boxplot_by_group(test_data, group, value, drop_na = FALSE)[[1]], expected_2)})

# test 3
test_that("Test if there is an error when the type of argument is wrong", {
  # `var` is not numeric
  expect_error(summary_and_boxplot_by_group(test_data, group, id))
  # `drop_na` is not logical
  expect_error(summary_and_boxplot_by_group(test_data, group, value, drop_na = 0))
  # `alpha` is not numeric
  expect_error(summary_and_boxplot_by_group(test_data, group, value, alpha = "0.5"))
  # `scale_y` is not logical
  expect_error(summary_and_boxplot_by_group(test_data, group, value, scale_y = 1))
})

# test 4
test_that("Test if boxplot is correct", {
  plot <- summary_and_boxplot_by_group(test_data, group, value, alpha = 0.6)[[2]]
  # test if x-axis is `group`
  expect_true(as.character(rlang::get_expr(plot$mapping$x)) == "group" ||
                as.character(rlang::get_expr(plot$layers[[1]]$mapping$x)) == "group")
  # test if y-axis is `value`
  expect_true(as.character(rlang::get_expr(plot$mapping$y)) == "value" ||
                as.character(rlang::get_expr(plot$layers[[1]]$mapping$y)) == "value")
  # test if it is boxplot
  expect_true("GeomBoxplot" %in% class(plot$layers[[1]]$geom))
  # test alpha
  expect_equal(plot$layers[[1]]$aes_params$alpha, 0.6)
})

rm(test_data)
