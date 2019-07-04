context("testing episode_group function")

library(testthat)
library(diyar)
library(dplyr)
library(lubridate)


# Test 1
data <- data.frame(date = seq.Date(dmy("01/04/2018"), dmy("31/05/2018"), by="3 days"))
data$pid <- "Patient 1"
data$episode_len <- 7
data <- mutate(data, rd_id = row_number())

test_1 <- episode_group(head(data,10), sn=rd_id, strata = pid, date = date, episode_length = episode_len)

test_that("test that row positions of the resulting dataframe are the same as supplied", {
  expect_equal(test_1$sn, head(data,10)$rd_id)
})

test_that("test that test episode identifier is as expected for static episode type", {
  expect_equal(test_1$epid, c(1,1,1,4,4,4,7,7,7,10))
  expect_equal(test_1$case_nm, rep(c("Case",rep("Duplicate",2)),4)[1:10] )
})

# Test 2
data_2 <- mutate(head(data,10), episode_len_s=13)
test_2 <-
cbind(data_2,
      select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE, from_last = FALSE), -sn, epid.1=epid, case.1=case_nm),
      select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE, from_last = TRUE), -sn, epid.2=epid, case.2=case_nm)
)

test_that("test reverse episode grouping", {
  expect_equal(test_2$epid.1, c(rep(1,5),rep(6,5)))
  expect_equal(test_2$epid.2, c(rep(5,5),rep(10,5)))
  expect_equal(test_2$case.1, rep(c("Case",rep("Duplicate",4)),2))
  expect_equal(test_2$case.2, rep(c(rep("Duplicate",4),"Case"),2))

})

# Test 3
test_3 <- cbind(data_2,
      select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE, from_last = FALSE), -sn, epid.1=epid, case.1=case_nm),
      select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE, from_last = TRUE), -sn, epid.2=epid, case.2=case_nm)
      )

test_that("test rolling/recurring episodes", {
  expect_equal(test_3$epid.1, rep(1,10))
  expect_equal(test_3$epid.2, rep(10,10))
  expect_equal(test_3$case.1, c("Case",rep("Duplicate",4),"Recurrent",rep("Duplicate",3),"Recurrent"))
  expect_equal(test_3$case.2, rev(c("Case",rep("Duplicate",4),"Recurrent",rep("Duplicate",3),"Recurrent")))

})

data_4 <- mutate(data_2, recurrence=4)
test_4 <- cbind(data_4,
      select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
      select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 1,  display = FALSE), -sn, epid.2=epid, case.2=case_nm)
      )

test_that("test custom rc_length and roll_max 1", {
  expect_equal(test_4$epid.1, rep(1,10))
  expect_equal(test_4$epid.2, c(rep(1,6), rep(7,4)))
  expect_equal(test_4$case.1, c("Case",rep("Duplicate",4),rep("Recurrent",5)))
  expect_equal(test_4$case.2, c("Case",rep("Duplicate",4),"Recurrent","Case",rep("Duplicate",3)))

})

# Test 5
test_5 <- cbind(data_4,
      select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, episodes_max = 1, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
      select(episode_group(data_4, sn=rd_id, strat = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, episodes_max = 2,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
      )

test_that("testing episodes_max 1", {
  expect_equal(test_5$epid_l, c(rep(1,5),6:10))
  expect_equal(test_5$epid_s, c(rep(1,5), rep(6,5)))
  expect_equal(test_5$case_l, c("Case",rep("Duplicate",4),rep("Case",5)))
  expect_equal(test_5$case_s, rep(c("Case",rep("Duplicate",4)),2))

})

# Test 6
data_6 <-  mutate(data_4, recurrence=3)
data_6$dataset <- paste("DS",c(1:3, rep(c(1:2),2), rep(3,3)), sep="")

test_6 <- cbind(data_6,
                select(episode_group(data_6, sn=rd_id, strata = pid, date = date, episode_length = episode_len, episode_type ="rolling", rc_episode_length = recurrence, data_source = dataset, display = FALSE), -sn, epid_l=epid, case_l=case_nm, epid_grp_l=epid_grp)
      )

test_that("testing epid_grp", {
  expect_equal(test_6$epid_l, c(rep(1,3),rep(4,3),rep(7,3),10))
  expect_equal(test_6$case_l, rep(c("Case",rep("Duplicate",2)),4)[1:10])
  expect_equal(test_6$epid_grp_l, c(rep("DS1,DS2,DS3",3),rep("DS1,DS2",3),rep("DS2,DS3",3),"DS3" ))
})
