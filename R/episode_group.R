#' @title Episode grouping
#'
#' @description This function assigns records into unique chronological episodes
#'
#'
#' @param df Dataframe. One or more datasets appened together.
#' @param sn Unique record indentifier for the dataframe
#' @param strata Column names. Episode grouping will be straified by these columns and will be unique to each strata
#' @param date Record date. A column of \code{date} type
#' @param episode_length Episode duration/window in calendar days. Integer values between \code{0} and \code{Inf}, and should be unique to each \emph{strata}.
#' @param episodes_max Maximum number of episodes permitted in each strata. An integer between \code{0} and \code{Inf}.
#' @param episode_type \emph{"static"} or \emph{"rolling"}. If \emph{"static"}, each episode will only include records within a fixed period \code{episode_length} from the inital record (case). If \emph{"rolling"}, will include recurrent records. \strong{See \code{rc_episode_length}}
#' @param rc_episode_length Period of recurrence in calendar days. Recurrence here refers to records ocurring after \code{episode_length} of the first record but within the \code{rc_episode_length} of the last duplicate record. Only used if \code{episode_type}`is \emph{"rolling"}. Should also be unique to each strata. Integer value between \code{0} and \code{Inf}
#' @param rolls_max Maximum number of recurrence within each episode. Integer between \code{0} and \code{Inf}. Only used if \code{episode_type} is \emph{"rolling"}.
#' @param data_source Unique dataset indentifier for the dataframe. Usefull when dataframe contains multiple datsets.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting from the most recent record to the earliest. If \code{FALSE}, it'll be forward in time - starting from the earliest record to most recent.
#' @param display If \code{TRUE}, progress status at each stage of episode grouping is displayed on screen
#'
#' @return Dataframe with a unique episode identifier, type of record based on the episode length and type, and if selected,
#' a list of datasets where each episode was identified
#'
#' @examples
#'
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
#' # For each episode - `epid`, the record where episode tracking began is flaged as the \emph{"Case"} in `case_nm`, while the others are \emph{"Duplicate"} cases.
#' cbind(head(data,10),
#'       episode_group(head(data,10), sn=rd_id, strata = pid, date = date, episode_length = episode_len)
#' )
#' # The progress message can be turned off with the \code{display} argument
#' cbind(head(data,10),
#'       episode_group(head(data,10), sn=rd_id, strata = pid, date = date, episode_length = episode_len, display = FALSE)
#' )
#' #A longer \code{episode_length} will yield less cases and vice versa
#' data_2 <- mutate(head(data,10), episode_len_s=13)
#' cbind(data_2,
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # By default, episode tracking starts from the earliest record of each group and proceeds forward in time. For the reverse use the \code{from_last} argument
#' # Below is an example of episode tracking from both directions with an \code{episode_length} of 13 calendar days
#' cbind(data_2,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, display = FALSE, from_last = TRUE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # By deafult, episodes are defined within a fixed window period of \code{episode_length} in calendar days. Any record after this window period is assigned to a new episode
#' # Changing \code{episode_type} to \emph{"rolling"} will change this behaviour to account for \emph{"Recurring"} recording.
#' # \emph{"Recurring"} records occur after the episode \code{episode_length} of the primary \emph{"Case"} but within \code{rc_episode_length} of the last \emph{"Duplicate"} or \emph{"Recurrent"} case
#' # The primary \emph{"Case"} and all subsequent \emph{"Recurring"} cases are assigned to the same episode. If the next chronological record is not a \emph{"Recurring"} case, it is assigned to a new epsiode.
#' # The example below demonstrates this
#' cbind(data_2,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_2, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", display = FALSE, from_last = TRUE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # By default, \code{episode_length} is for \code{rc_episode_length}. A different window period for recurrent records can be specied using the \code{rc_episode_length} argument.
#' # The example below demonsrates the difference between an \code{episode_length} and \code{rc_episode_length} of 13 calendar days compared to an \code{episode_length} of 13 calendar days and \code{rc_episode_length} of 60 calendar days
#' # Both example yield one episode (`epid`) however, the first has 1 primary \emph{"Case"} and 4 \emph{recurrent}records while the second has 1 primary \emph{"Case"} and 2 recurrent "records"
#' data_4 <- mutate(data_2, recurrence=30)
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len, episode_type ="rolling", display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len, episode_type ="rolling", rc_episode_length = recurrence, display = FALSE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # By default the function will continue checking for recurring cases until the last record (or first if \code{from_last} is TRUE By default, \code{episode_length} is for \code{rc_episode_length}.
#' # You can specify how many times to check for recurring records with the \code{rolls_max} argument
#' # When the rolling window is closed by \code{rolls_max} subsequent records after the last "Recurring" case are assigned to new episodes
#' # The example below demonsrates in output is when \code{episode_length} is 13 calendar deys, \code{rc_episode_length} is 30 calendar days and rolls_max is Inf (default) or 1.
#' data_4 <- mutate(data_4, recurrence=4)
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 1,  display = FALSE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # Note that if \code{rolls_max} is "0" the function will have the same behaviour as when \code{episode_type} is \emph{"static"} .
#'
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, display = FALSE), -sn, epid.1=epid, case.1=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="rolling", rc_episode_length = recurrence, rolls_max = 0,  display = FALSE), -sn, epid.2=epid, case.2=case_nm)
#' )
#' # You can also choose a maximum of episodes to track within each \code{strata} using \code{episodes_max}. The default value is Inf which will access the maximum number of windows available from the dataset
#' cbind(data_4,
#'       #episode tracking starts from the earlist record in each group and proceeds forward in time
#'       select(episode_group(data_4, sn=rd_id, strata = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, episodes_max = 1, display = FALSE), -sn, epid_l=epid, case_l=case_nm),
#'       #episode tracking starts from the most recent record in each group and proceeds backward in time
#'       select(episode_group(data_4, sn=rd_id, strat = pid, date = date, episode_length = episode_len_s, episode_type ="static", rc_episode_length = recurrence, episodes_max = 2,  display = FALSE), -sn, epid_s=epid, case_s=case_nm)
#' )
#'
#' # Also note that \code{rolls_max} and \code{episodes_max} controls different aspects of episode tracking hence their behaviour are not interchangeble
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

episode_group <- function(df, sn = NULL, strata = NULL,
                          date, episode_length, episode_type="static", episodes_max = Inf,
                          rc_episode_length = NULL, rolls_max =Inf, data_source = NULL,
                          source_sort = FALSE, from_last=FALSE, display=TRUE){

  #Later, add data validations for arguments - assert that
  enq_vr <- function(x, vr){
    x <- names(dplyr::select(x, !!vr))

    if(length(x)==0){
      x <- NULL
    }else{
      x
    }
    return(x)
  }

  rd_sn <- enq_vr(df, dplyr::enquo(sn))
  ds <- enq_vr(df, dplyr::enquo(data_source))
  epl <- enq_vr(df, dplyr::enquo(episode_length))
  r_epl <- enq_vr(df, dplyr::enquo(rc_episode_length))
  st <- enq_vr(df, dplyr::enquo(strata))

  df_list <- names(df)

  if(is.null(rd_sn)){
    df <- dplyr::mutate(df, sn= dplyr::row_number())
  }else{
    df <- dplyr::rename(df, sn= !!dplyr::enquo(sn))
  }

  if(is.null(ds)){
    df$source <- "A"
  }else{
    df <- dplyr::rename(df, source = !!dplyr::enquo(data_source))
  }

  if(is.null(r_epl)){
    df <- dplyr::mutate(df, rc_len= !!dplyr::enquo(episode_length))
  }else{
    df <- dplyr::rename(df, rc_len= !!dplyr::enquo(rc_episode_length))
  }

  if(is.null(st)){
    df$cri <- "A"
  }else{
    df <- tidyr::unite(df, "cri", c(!!dplyr::enquo(strata)), remove=FALSE)
  }

  #fields of interest
  df <- df %>%
    dplyr::select(sn, spec_dt=!!dplyr::enquo(date), epi_len=!!dplyr::enquo(episode_length), .data$rc_len, .data$source, .data$cri) %>%
    dplyr::mutate(tag = 0, epid = 0, case_nm="", pr_sn = dplyr::row_number(), roll=0, episodes=0)

  if(from_last==TRUE){
    df$ord <- abs(max(df$spec_dt) - df$spec_dt)
  }else if (from_last==FALSE){
    df$ord <- abs(min(df$spec_dt) - df$spec_dt)
  }

  if(source_sort==TRUE){
    df$s_ord <- df$source
  }else if (source_sort==FALSE){
    df$s_ord <- 1
  }

  c <- 1
  min_episodes_nm <- min_tag <- 0
  while (min_tag != 2 & min_episodes_nm <= episodes_max ){
    if(display){print(paste("Episode window",c), sep=" ")}
    TR <- df %>%
      #preference to those tagged already i.e. exisitng episodes
      dplyr::arrange(.data$cri, .data$s_ord, dplyr::desc(.data$tag), .data$ord, .data$sn) %>%
      #exclude records that will create 1 episode more than episodes_max
      dplyr::filter(!(.data$tag==0 & .data$episodes + 1 > episodes_max )) %>%
      #pid ids of those tagged before
      #among cases not tagged
      dplyr::filter(.data$tag %in% c(0,1)) %>%
      #check for duplicates. single records will come up as false
      dplyr::filter(duplicated(.data$cri) == FALSE) %>%
      #info to be inherited
      dplyr::select(.data$sn, .data$cri, .data$spec_dt, .data$epid, .data$tag, .data$case_nm) %>%
      dplyr::rename_at(dplyr::vars(.data$sn, .data$spec_dt, .data$epid, .data$tag, .data$case_nm), dplyr::funs(paste("tr_",.,sep="")))

    if(nrow(TR)==0){
      break
    }

    #


    #transfering epid to matching record
    ##do not over write existing epids
    df <- dplyr::left_join(df, TR, by= "cri")

    if (from_last==FALSE){
      df$day_diff <- -(df$tr_spec_dt - df$spec_dt)
    }else{
      df$day_diff <- (df$tr_spec_dt - df$spec_dt)
    }

    df <- df %>%
      dplyr::mutate(
        epid = ifelse(
          .data$tag==0 & .data$tr_tag==0 & !is.na(.data$tr_tag) & .data$day_diff >=0 & .data$day_diff <= (.data$epi_len-1) ,
          .data$tr_sn, .data$epid
        )
        ,
        case_nm = ifelse(
          .data$tag==0 & .data$tr_tag==0 & !is.na(.data$tr_tag) & .data$epid == .data$tr_sn & !is.na(.data$tr_sn),
          ifelse(.data$tr_sn == .data$sn, "Case", "Duplicate"),
          .data$case_nm
        ),
        #
        epid = ifelse(
          .data$tag==0 & .data$tr_tag==1 & .data$day_diff >=0 & .data$day_diff <= (.data$rc_len -1) & !is.na(.data$tr_tag) & !is.na(.data$tr_spec_dt) & !is.na(.data$tr_epid) ,
          .data$tr_epid, .data$epid
        ),

        case_nm = ifelse(.data$tag==0 & .data$tr_tag==1 & .data$epid == .data$tr_epid & !is.na(.data$tr_tag) & !is.na(.data$tr_epid),"Recurrent", .data$case_nm)
      )

    #number of tagged records for print output
    tagged_1 <- length(subset(df$epid, !df$epid %in% c(0,NA) & df$tag ==0 ))
    total_1 <- length(subset(df$cri, df$tag ==0))

    fmt <- function(g) formatC(g, format="d", big.mark=",")

    if(display){
      print(
        paste(fmt(tagged_1)," of ", fmt(total_1),
              " record(s) grouped into episodes. ", fmt(total_1-tagged_1),
              " records not yet grouped.", sep ="")
      )
    }

    df <- df %>%
      dplyr::arrange(.data$cri, .data$ord, .data$sn) %>%
      dplyr::mutate(
        roll = ifelse(.data$tr_tag==1 & !is.na(.data$tr_tag), .data$roll + 1, .data$roll),
        episodes = ifelse(.data$tr_tag==0 & !is.na(.data$tr_tag), .data$episodes + 1, .data$episodes),
        #
        mrk_c = paste(.data$epid, .data$case_nm, .data$tag, sep="_"),
        case_nm = ifelse(duplicated(.data$mrk_c) & .data$case_nm =="Recurrent" & .data$tag==0, "Duplicate", .data$case_nm),

        tag = ifelse(!.data$epid %in% c(0,NA),2, 0),

        mrk=paste(.data$cri, .data$tag,sep="_"),
        tag=ifelse(episode_type == "rolling" & .data$roll < rolls_max &
                     .data$tag==2 & !duplicated(.data$mrk, fromLast=TRUE) & .data$sn != .data$tr_sn, 1, .data$tag)
      ) %>%
      dplyr::select( -dplyr::starts_with("tr"), -.data$mrk, -.data$mrk_c)

    min_tag <- min(df$tag)
    min_roll <- min(df$roll)
    min_episodes_nm <- min(df$episodes)


    c = c+1
  }

  df <- df %>%
    dplyr::mutate(
      case_nm= ifelse(.data$epid==0, "Case", .data$case_nm),
      epid= ifelse(.data$epid==0, .data$sn, .data$epid)
    )

  sourc_list <- as.character(sort(unique(df$source)))

  grps <-df %>%
    dplyr::select(.data$epid, .data$source) %>%
    unique() %>%
    dplyr::mutate(val= .data$source) %>%
    dplyr::arrange(.data$source) %>%
    tidyr::spread(key="source", value="val") %>%
    tidyr::unite("epid_grp", sourc_list, sep=",") %>%
    dplyr::mutate(epid_grp = stringr::str_replace_all(.data$epid_grp,"NA,|,NA|^NA$",""))

  df <- df %>%
    dplyr::left_join(grps, by="epid") %>%
    dplyr::arrange(.data$pr_sn)

  if(is.null(ds)){
    df <- dplyr::select(df, .data$sn, .data$epid, .data$case_nm)
  }else{
    df <- dplyr::select(df, .data$sn, .data$epid, .data$case_nm, .data$epid_grp)
  }

  print("Episode grouping complete")

  df
}
