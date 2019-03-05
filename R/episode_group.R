#' @title Episode groupping
#'
#' @description This function assigns records from unique groups into unique episodes
#'
#'
#' @param df A dataframe
#' @param sn A column of unique indentifiers for each record in 'df'
#' @param group_id Character/Numeric vector - A column of unique identifiers for each grouped set of records
#' @param strata Character/Numeric vector - A column of unique strata's within each group
#' @param date Date vector - Date of the event's occurrence
#' @param episode_length Numerical vector - A column indicating how long an episode is considered to last (calendar days)
#' @param windows_max Numerical vector - The number of episode windows to track. Should be > 1. Default is Inf.
#' @param episode_type Character - "static" or "rolling". Indicates if the episode is a fixed length from the first incident or from the most recent incident respectively.
#' @param rolls_max Numerical vector - The number of times to check for a recurring record before proceeding to a new case. Should be >1. Default is Inf.
#' @param rc_episode_length Numerical vector - A column indicating how long a reocurring episode should last (calendar days). Only used if episode_type is "static".
#' @param source Character/Numerical vector - A column of unique flags indicating the source of each dataset
#' @param from_last Logical - if TRUE, epiosde groupping will be backwards in time from the most recent record. If FALSE, it'll be forward in time from the earliest record. Default is FALSE
#'
#'
#' @return Dataframe with a unique episode identifier, type of record based on the epiosde length and type, and if selected,
#' a list of datasets where each episode was identified
#'
#' @examples
#'
#' library(lubridate)
#' library(dplyr)
#'
#' data <- data.frame(
#'   date = seq.Date(dmy("01/04/2018"), dmy("31/05/2018"), by="3 days")
#' )
#'
#' data$pid <- "Patient 1"
#' data$episode_len <- 7
#' data <- mutate(data, rd_id = row_number())
#'
#' # The result will include a unique episode ID matching the criteria selected.
#' # For each episode - `epid`, the record where episode tracking began is flaged as the "Case" in `case_nm`, while the others are "Duplicate" cases.
#'
#' cbind(data,
#'       episode_group(data, sn=rd_id, group_id = pid, date = date, episode_length = episode_len)
#' )
#'
#' # The progress message can be turned off with the `display` argument
#' cbind(data,
#'       episode_group(data, sn=rd_id, group_id = pid, date = date, episode_length = episode_len, display = FALSE)
#' )
#'
#' #A longer `episode_length` will yield less cases and vice versa
#' data_2 <- mutate(data, episode_len_s=13)
#'
#' cbind(data_2,
#'       select(episode_group(data_2, sn=rd_id, group_id = pid, date = date, episode_length = episode_len, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       select(episode_group(data_2, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # By default, episode tracking starts from the earliest record of each group and proceeds forward in time. For the reverse use the `from_last` argument
#' # Below is an example of episode tracking from both directions with an `episode_length` of 13 calendar days
#' data_3 <- data_2 %>% head(19)
#'
#' cbind(data_3,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_3, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_3, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, display = FALSE, from_last = TRUE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # By deafult, episodes are defined within a fixed window period of `episode_length` in calendar days. Any record after this window period is assigned to a new episode
#' # Changing `episode_type` to "rolling" will change this behaviour to account for recurring cases.
#' # Recurring cases occur after the episode `episode_length` of the primary "Case" but within `rc_episode_length` of the last "duplicate" or "recurrent" case
#' # The primary "case" and all subsequent "recurring" cases are assigned to the same episode. If the next chronological record is not a "recurring" case, it's assigned to a new epsiode.
#' # The example below demonstrates this
#'
#' cbind(data_3,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_3, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_3, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE, from_last = TRUE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # By default, `episode_length` is for `rc_episode_length`. A different window period for recurrent records can be specied using the `rc_episode_length` argument.
#' # The example below demonsrates the difference between an `episode_length` and `rc_episode_length` of 13 calendar days compared to an `episode_length` of 13 calendar days and `rc_episode_length` of 60 calendar days
#' # Both example yield one episode (`epid`) however, the first has 1 primary "case" and 4 "recurrent" records while the second has 1 primary "Case" and 2 recurrent "records"
#'
#' data_4 <- mutate(data_3, recurrence=30)
#'
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # By default the function will continue checking for recurring cases until the last record (or first if `from_last` is TRUE By default, `episode_length` is for `rc_episode_length`.
#' # You can specify how many times to check for recurring records with the `rolls_max` argument
#' # When the rolling window is closed by `rolls_max` subsequent records after the last "Recurring" case are assigned to new episodes
#' # The example below demonsrates in output is when `episode_length` is 13 calendar deys, `rc_episode_length` is 30 calendar days and rolls_max is Inf (default) or 1.
#'
#' data_4 <- mutate(data_3, recurrence=4)
#'
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 1,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # Note that chosing `rolls_max` = 0 will be the same as a "static" `episode_type`
#'
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 0,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # You can also choose a maximum number of episode windows per group using `windows_max`. The default value is Inf which will access the maximum number of windows available from the dataset
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, windows_max = 1, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, windows_max = 2,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # Note that if `episode_type` is "static" then `windows_max` =2 means a check for the 1st record and its duplicates and another check for second case and it's duplicates.
#' # However if `episode_type` is "rolling" then `windows_max` =2 means a check for the 1st record and its duplicates and a 2nd check for the next recurring case and its duplicates.
#' # If `episode_length` and `rc_episode_length` are not the same this will give you different results accordingly
#'
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, windows_max = 2, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, windows_max = 2,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # Also note that `rolls_max` and `windows_max` controls different aspects of episode tracking hence their behaviour are not interchangeble
#' # For instance, the examples below gives the same result
#' data_5 <- mutate(data_4, recurrence=13)
#'
#' cbind(data_5,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_5, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, windows_max = 2, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_5, sn=rd_id, group_id = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 2,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' @export

episode_group <- function(df, sn, group_id, strata = NA,
                          date, episode_length, episode_type="static", windows_max = Inf,
                          rc_episode_length = NA, rolls_max =Inf, data_source = NA, from_last=FALSE, display=TRUE){

  #Later, add data validations for arguments - assert that

  if(is.na(data_source)){
    df$source <- "A"
  }else{
    df <- dplyr::rename(df, source= !!enquo(data_source))
  }

  if(is.null(df[[paste(enquo(rc_episode_length))[2]]])){
    df <- dplyr::mutate(df, rc_len= !!enquo(episode_length))
  }else{
    df <- dplyr::rename(df, rc_len= !!enquo(rc_episode_length))
  }

  if(all(is.na(strata))){
    df <- dplyr::mutate(df, cri= !!enquo(group_id))
  }else{
    df <- tidyr::unite(df, cri= c(!!enquo(group_id),!!enquo(strata)), remove=FALSE)
  }


  #fields of interest
  T1 <- df %>%
    dplyr::select(sn=!!enquo(sn), group_id=!!enquo(group_id),
                  spec_dt=!!enquo(date), epi_len=!!enquo(episode_length), rc_len, source, cri) %>%
    dplyr::mutate(tag = 0, epid = 0, case_nm="", pr_sn = row_number(), roll=0)

  if(from_last==TRUE){
    T1$ord <- abs(max(T1$spec_dt) - T1$spec_dt)
  }else if (from_last==FALSE){
    T1$ord <- abs(min(T1$spec_dt) - T1$spec_dt)
  }

  c <- 1
  min_tag <- 0
  while (min_tag != 2 & c <= windows_max){
    if(display){print(paste("Episode window",c), sep=" ")}
    TR <- T1 %>%
      #preference to those tagged already i.e. exisitng episodes
      dplyr::arrange(cri, desc(tag), ord, sn) %>%
      #pid ids of those tagged before
      #among cases not tagged
      dplyr::filter(tag%in%c(0,1)) %>%
      #check for duplicates. single records will come up as false
      dplyr::filter(duplicated(cri) == FALSE) %>%
      #info to be inherited
      dplyr::select(sn,cri,spec_dt,epid,tag,case_nm) %>%
      dplyr::rename_at(vars(sn,spec_dt,epid,tag,case_nm), funs(paste("tr_",.,sep="")))

    #transfering epid to matching record
    ##do not over write existing epids
    T1 <- T1 %>%
      dplyr::left_join(TR, by= "cri") %>%
      dplyr::mutate(
        #
        epid = ifelse(
          tag==0 & tr_tag==0 & abs(spec_dt-tr_spec_dt) <= (epi_len-1) ,
          tr_sn, epid
        )
        ,
        case_nm = ifelse(
          tag==0 & tr_tag==0 & epid ==tr_sn,
          ifelse(tr_sn == sn, "Case", "Duplicate"),
          case_nm
        ),
        #
        epid = ifelse(
          tag==0 & tr_tag==1 & abs(spec_dt-tr_spec_dt) <= (rc_len-1),
          tr_epid, epid
        ),

        case_nm = ifelse(tag==0 & tr_tag==1 & epid ==tr_epid,"Recurrent", case_nm)
      )

    #number of tagged records for print output
    tagged_1 <- subset(T1$epid, !T1$epid %in% c(0,NA) & T1$tag ==0 ) %>%  length()
    total_1 <- subset(T1$cri, T1$tag ==0 ) %>%  length()

    fmt <- function(g) formatC(g, format="d", big.mark=",")

    if(display){
      print(
        paste(fmt(tagged_1)," of ", fmt(total_1),
              " record(s) grouped into episodes. ", fmt(total_1-tagged_1),
              " records not yet grouped.", sep ="")
      )
    }
    T1 <- T1 %>%
      dplyr::arrange(cri, ord, sn) %>%
      dplyr::mutate(
        roll = ifelse(tr_tag==1, roll + 1, roll),
        #
        mrk_c = paste(epid,case_nm,tag,sep="_"),
        case_nm = ifelse(duplicated(mrk_c) & case_nm =="Recurrent" & tag==0, "Duplicate", case_nm),

        tag = ifelse(!epid %in% c(0,NA),2, 0),

        mrk=paste(cri,tag,sep="_"),
        tag=ifelse(episode_type == "rolling" & roll < rolls_max &
                     tag==2 & !duplicated(mrk, fromLast=TRUE) & sn!=tr_sn, 1, tag)
      ) %>%
      dplyr::select( -starts_with("tr"), -mrk, -mrk_c)

    min_tag <- min(T1$tag)

    c = c+1
  }

  T1 <- T1 %>%
    dplyr::mutate(
      case_nm= ifelse(epid==0, "Case", case_nm),
      epid= ifelse(epid==0, sn, epid)
    )

  sourc_list <- as.character(sort(unique(T1$source)))

  grps <-T1 %>%
    dplyr::select(epid, source) %>%
    unique() %>%
    dplyr::mutate(val=source) %>%
    dplyr::arrange(source) %>%
    tidyr::spread(key=source, value=val) %>%
    tidyr::unite(epid_grp,sourc_list, sep=",") %>%
    dplyr::mutate(epid_grp = stringr::str_replace_all(epid_grp,"NA,|,NA|^NA$",""))

  T1 <- T1 %>%
    dplyr::left_join(grps, by="epid") %>%
    dplyr::arrange(pr_sn)

  if(all(is.na(data_source))){
    T1 <- dplyr::select(T1,sn,epid,case_nm)
  }else{
    T1 <- dplyr::select(T1,sn,epid,case_nm,epid_grp)
  }

  print("Episode ID complete")

  T1
}
