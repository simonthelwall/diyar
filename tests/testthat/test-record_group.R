context("testing record_group function")

library(testthat)

# Test 1
df <- data.frame(
  cri_1 = c("A","C","B","C","A"),
  r_id = c(1:5)
)

test_1 <- cbind(df, record_group(df,r_id, cri_1))

test_that("tests that row positions of the resulting dataframe are the same as supplied", {
  expect_equal(test_1$sn, test_1$r_id)
})

test_that("tests that test record identifier is as expected for one criteria", {
  expect_equal(test_1$pid, c(1,2,3,2,1))
  expect_equal(test_1$pid_cri, c("Criteria 1","Criteria 1","None","Criteria 1","Criteria 1"))
})

# Test 2
df_2b <- df_2a <- df
df_2a$cri_1 <- ifelse(df_2a$cri_1=="A", NA, df_2a$cri_1 )
df_2b$cri_1 <- ifelse(df_2b$cri_1=="A", "", df_2b$cri_1 )

test_2a <- cbind(df_2a, record_group(df_2a,r_id, cri_1))
test_2b <- cbind(df_2b, record_group(df_2b,r_id, cri_1))

test_that("tests that test blank space or NAs criteria are treated as unique record groups", {
  expect_equal(test_2a$pid, c(1,2,3,2,5))
  expect_equal(test_2b$pid, c(1,2,3,2,5))

})

# Test 3
df_3a <- data.frame(
  cri_1 = c("A","C","Z","V","F","G","G"),
  cri_2 = c("CC","AA","CC","VV","AA","CB","CC"),
  r_id = c(1:7),

  stringsAsFactors = FALSE
)

df_3b <- df_3a

df_3b$cri_1 <- ifelse(df_3b$r_id==7, NA, df_3b$cri_1 )

test_3a <- cbind(df_3a, record_group(df_3a,r_id, c(cri_1, cri_2) ))
test_3b <- cbind(df_3b, record_group(df_3b,r_id, c(cri_1, cri_2) ))

test_that("tests that record grouping with >1 criteria follows order of decreasing certaintity", {
  expect_equal(test_3a$pid, c(6,2,6,4,2,6,6))
  expect_equal(test_3a$pid_cri, c("Criteria 2","Criteria 2","Criteria 2", "None","Criteria 2","Criteria 1", "Criteria 1"))
  expect_equal(test_3b$pid, c(1,2,1,4,2,6,1))
  expect_equal(test_3b$pid_cri, c("Criteria 2","Criteria 2","Criteria 2", "None","Criteria 2","None", "Criteria 2"))

})

