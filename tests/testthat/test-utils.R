test_that("cms",{
  expect_equal(dtq_cms_to_cfs(1:2), c(35.31467, 70.62933),
                   tolerance = 1e-06)
  expect_identical(dtq_cfs_to_cms(dtq_cms_to_cfs(1:2)), c(1,2))
  expect_identical(dtq_kcfs_to_cms(dtq_cms_to_kcfs(1:2)), c(1,2))
  expect_identical(dtq_cms_to_kcfs(1), dtq_cms_to_cfs(1/1000))
})