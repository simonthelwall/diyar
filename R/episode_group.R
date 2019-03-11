#' @title Episode groupping
#'
#' @description This function assigns records into unique chronological episodes
#'
#'
#' @param df A dataframe.
#' @param sn Unique identifier for each record in `df`.
#' @param strata A collection of column names forming to form stratas that will be tracked for episodes separately.
#' @param date A column of type `date` with date of when each record occured.
#' @param episode_length A column with ineger values between `0` and `Inf` for each record. This indicates episode duration or window in calendar days.
#' @param episode_type "static" or "rolling". If "static", an episode will only records within a fixed period from the inital record. If "rolling", in addition to those within the fixed period, an episode will inlcude all records within a fixed period of the last "duplicate" or "recurrent" record. Here, the initial record is refered to as the "Case". "Duplicate" are records within a fixed period fo the "Case". "Recurrent" are records after the fixed period of the "Case" but within a fixed period of the last "Duplicate" or a previous "Recurrent" record.
#' @param rc_episode_length A column with ineger values between `0` and `Inf` for each record. This indicates duration or window recurrence in calendar days. Recurrence here refers to records ocurring after episode_window of the first record but withing Only used if `episode_type` is "rolling".
#' @param rolls_max An integer between `0` and `Inf`  indicating the maximum number of "Recurring" each episode can have. Only used if `episode_type` is "rolling".
#' @param episodes_max An integer between `0` and `Inf` indicating the number of episodes to track.
#' @param data_source A colum identifier indicating the source of each record. Usefull when tracking episodes across mutliple datasets.
#' @param from_last If TRUE, epiosde tracking will be backwards in time starting from the most recent record to. If FALSE, it'll be forward in time starting from the earliest record. Default is FALSE.
#'
#'
#' @return Dataframe with a unique episode identifier, type of record based on the episode length and type, and if selected,
#' a list of datasets where each episode was identified
#'
#' @examples
#'
#' library(lubridate)
#' library(dplyr)
#'
#' data <- data.frame(date = seq.Date(dmy("01/04/2018"), dmy("31/05/2018"), by="3 days"))
#' data$pid <- "Patient 1"
#' data$episode_len <- 7
#' data <- mutate(data, rd_id = row_number())
#'
#' # The result will include a unique episode ID matching the criteria selected.
#' # For each episode - `epid`, the record where episode tracking began is flaged as the "Case" in `case_nm`, while the others are "Duplicate" cases.
#' cbind(head(data,10),
#'       episode_group(head(data,10), sn=rd_id, strata = pid, date = date, episode_length = episode_len)
#' )
#' # The progress message can be turned off with the `display` argument
#' cbind(head(data,10),
#'       episode_group(head(data,10), sn=rd_id, strata = pid, date = date, episode_length = episode_len, display = FALSE)
#' )
#' #A longer `episode_length` will yield less cases and vice versa
#' data_2 <- mutate(head(data,10), episode_len_s=13)
#' cbind(data_2,
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE), -sn, epid.2.2=epid, case.1=case_nm)
#' )
#' # By default, episode tracking starts from the earliest record of each group and proceeds forward in time. For the reverse use the `from_last` argument
#' # Below is an example of episode tracking from both directions with an `episode_length` of 13 calendar days
#' cbind(data_2,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE, from_last = TRUE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # By deafult, episodes are defined within a fixed window period of `episode_length` in calendar days. Any record after this window period is assigned to a new episode
#' # Changing `episode_type` to "rolling" will change this behaviour to account for "Recurring" recording.
#' # "Recurring" records occur after the episode `episode_length` of the primary "Case" but within `rc_episode_length` of the last "duplicate" or "recurrent" case
#' # The primary "case" and all subsequent "Recurring" cases are assigned to the same episode. If the next chronological record is not a "Recurring" case, it's assigned to a new epsiode.
#' # The example below demonstrates this
#' cbind(data_2,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE, from_last = TRUE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # By default, `episode_length` is for `rc_episode_length`. A different window period for recurrent records can be specied using the `rc_episode_length` argument.
#' # The example below demonsrates the difference between an `episode_length` and `rc_episode_length` of 13 calendar days compared to an `episode_length` of 13 calendar days and `rc_episode_length` of 60 calendar days
#' # Both example yield one episode (`epid`) however, the first has 1 primary "case" and 4 "recurrent" records while the second has 1 primary "Case" and 2 recurrent "records"
#' data_4 <- mutate(data_2, recurrence=30)
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len, episode_type ="rolling", display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len, episode_type ="rolling", rc_episode_length = recurrence, display = FALSE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # By default the function will continue checking for recurring cases until the last record (or first if `from_last` is TRUE By default, `episode_length` is for `rc_episode_length`.
#' # You can specify how many times to check for recurring records with the `rolls_max` argument
#' # When the rolling window is closed by `rolls_max` subsequent records after the last "Recurring" case are assigned to new episodes
#' # The example below demonsrates in output is when `episode_length` is 13 calendar deys, `rc_episode_length` is 30 calendar days and rolls_max is Inf (default) or 1.
#' data_4 <- mutate(data_4, recurrence=4)
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 1,  display = FALSE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # Note that if `rolls_max` is "0" the function will have the same behaviour as when `episode_type` is "static" .
#'
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 0,  display = FALSE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # You can also choose a maximum of episodes to track within each `strata` using `episodes_max`. The default value is Inf which will access the maximum number of windows available from the dataset
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, episodes_max = 1, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strat = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, episodes_max = 2,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # Also note that `rolls_max` and `episodes_max` controls different aspects of episode tracking hence their behaviour are not interchangeble
#' # For instance, the examples below gives the same result
#' data_5 <- mutate(data_4, recurrence=13)
#'
#' cbind(data_5,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_5, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, episodes_max = 2, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_5, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 2,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#' @export

episode_group <- function(df, sn = NA, strata = NA,
                          date, episode_length, episode_type="static", episodes_max = Inf,
                          rc_episode_length = NA, rolls_max =Inf, data_source = NA, from_last=FALSE, display=TRUE){

  #Later, add data validations for arguments - assert that
  enq_vr <- function(x){
    x <- paste(dplyr::enquo(sn))[2]
    x <- stringr::str_replace_all(x," ","")
    x <- stringr::str_replace_all(x,"\\(","")
    x <- stringr::str_replace_all(x,"\\)","")
    x <- stringr::str_replace_all(x,"c","")
    x <- stringr::str_split(x,",")[[1]]
    return(x)
  }

  if((is.null(df[[enq_vr(dplyr::enquo(sn))]]))){
    df <- dplyr::mutate(df, sn= dplyr::row_number())
  }else{
    df <- dplyr::rename(df, sn= !!dplyr::enquo(sn))
  }

  if((is.null(df[[enq_vr(dplyr::enquo(data_source))]]))){
    df$source <- "A"
  }else{
    df <- dplyr::rename(df, source= !!dplyr::enquo(data_source))
  }

  if((is.null(df[[enq_vr(dplyr::enquo(rc_episode_length))]]))){
    df <- dplyr::mutate(df, rc_len= !!dplyr::enquo(episode_length))
  }else{
    df <- dplyr::rename(df, rc_len= !!dplyr::enquo(rc_episode_length))
  }

  if((is.null(df[[enq_vr(dplyr::enquo(strata))]]))){
    df$cri <- "A"
  }else{
    df <- tidyr::unite(df, cri, c(!!dplyr::enquo(strata)), remove=FALSE)
  }


  #fields of interest
  T1 <- df %>%
    dplyr::select(sn, spec_dt=!!dplyr::enquo(date), epi_len=!!dplyr::enquo(episode_length), rc_len, source, cri) %>%
    dplyr::mutate(tag = 0, epid = 0, case_nm="", pr_sn = row_number(), roll=0, episodes=0)

  if(from_last==TRUE){
    T1$ord <- abs(max(T1$spec_dt) - T1$spec_dt)
  }else if (from_last==FALSE){
    T1$ord <- abs(min(T1$spec_dt) - T1$spec_dt)
  }

  c <- 1
  min_episodes_nm <- min_tag <- 0
  while (min_tag != 2 & min_episodes_nm <= episodes_max ){
    if(display){print(paste("Episode window",c), sep=" ")}
    TR <- T1 %>%
      #preference to those tagged already i.e. exisitng episodes
      dplyr::arrange(cri, desc(tag), ord, sn) %>%
      #exclude records that will create 1 episode more than episodes_max
      dplyr::filter(!(tag==0 & episodes + 1 > episodes_max )) %>%
      #pid ids of those tagged before
      #among cases not tagged
      dplyr::filter(tag%in%c(0,1)) %>%
      #check for duplicates. single records will come up as false
      dplyr::filter(duplicated(cri) == FALSE) %>%
      #info to be inherited
      dplyr::select(sn,cri,spec_dt,epid,tag,case_nm) %>%
      dplyr::rename_at(vars(sn,spec_dt,epid,tag,case_nm), funs(paste("tr_",.,sep="")))

    if(nrow(TR)==0){
      break
    }

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
        episodes = ifelse(tr_tag==0, episodes + 1, episodes),
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
    min_roll <- min(T1$roll)
    min_episodes_nm <- min(T1$episodes)


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

  print("Episode tracking complete")

  T1
}
