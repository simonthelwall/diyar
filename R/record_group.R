#' @title Deterministic linkage for epidemiological analysis
#'
#' @description This function links records from one or more datasets into a unique idenfier. Linkage is based on specified matching criteria.
#'
#' @param df Dataframe. One or more datasets appened together.
#' @param sn Unique record indentifier for the dataframe.
#' @param criteria Matching criteria. Records with matching values in these columns are grouped together. Records are grouped in order of decreasing certainty. For exampe, the first criteria is more certain than second therefore, records matching on the first criteria will be grouped together in preference to those matching on the second criteria.
#' @param sub_criteria Matching sub-criteria. If is not \code{NULL}, only records with matching values in the \code{criteria} and \code{sub_criteria} columns are grouped together. Usefull in providing alternative or less stringent matching criteria.
#' @param data_source Unique dataset indentifier for the dataframe. Useful when dataframe contains multiple datasets.
#' @param display If \code{TRUE}, progress status at each stage of record grouping is displayed on screen.
#'
#' @return Dataframe
#'
#' @examples
#'
#' library(dplyr)
#'
#' df <- data.frame(
#'   cri_1 = c("A","C","B","C","A"),
#'   r_id = c(1:5),
#'   stringsAsFactors = FALSE
#' )
#'
#' cbind(df, record_group(df,r_id, cri_1))
#'
#' # Records with NA or "" in a particular criteria are excluded from episode grouping on that criteria
#' data_2 <- df
#' data_2$cri_1 <- ifelse(data_2$cri_1=="A", NA, data_2$cri_1)
#' data_2$cri_1 <- ifelse(data_2$cri_1=="B", "", data_2$cri_1)
#'
#' cbind(data_2, record_group(data_2,r_id, cri_1, display = FALSE))
#'
#' # Record grouping on 2 criteria
#' data_3 <- data.frame(
#'   cri_1 = c("A","C","Z","V","F","G","G"),
#'   cri_2 = c("CC","AA","CC","VV","AA","CB","CC"),
#'   r_id = c(1:7),
#'
#'   stringsAsFactors = FALSE
#' )
#'
#' data_3$cri_1 <- ifelse(data_3$r_id==7, NA, data_3$cri_1 )
#' cbind(data_3, record_group(data_3,r_id, c(cri_1, cri_2), display = FALSE))
#'
#' # At each stage of episode grouping ("criteria"), additonal matching conditions ("sub_criteria") can be introduced with a list of sub-criteria.
#' # Each sub-criteria consists of character vectors which are column names within the dataframe.
#' # If records match on the "criteria" but do not match on ALL "sub_criteria", they are assigned to a new record group
#' # "sub_criteria" should be a list of character vectors. Each vector represents a sub-criteria
#' # Each sub-criteria should be name using this format; sXy, where "X" is linkage stage when the sub-criteria is to be introduced while "y" is a sufficient prefix to differentiate between the criteria
#'
#' data_4 <- data.frame(
#'   cri_1 = c(NA,NA,NA,NA,NA,2,2),
#'   cri_2 = rep("A1",7),
#'   cri_2a = c("A","B","D","X","F","G","G"),
#'   cri_2b = c("B","B","D","X","F","G","I"),
#'   cri_2c = c("A","C","A","A","F","G","I"),
#'   r_id = c(1:7),
#'
#'   stringsAsFactors = FALSE
#' )
#'
#' cbind(df_4a, record_group(data_4,r_id, c(cri_1, cri_2), list(s2a=c("cri_2a","cri_2b","cri_2c")) ))
#'
#' @export

record_group <- function(df, sn, criteria, sub_criteria=NULL, data_source = NULL, display=TRUE){

  ds <- enq_vr(df, dplyr::enquo(data_source))


  df_vars <- names(df)

  cri_lst <- enq_vr(df, dplyr::enquo(criteria))
  sub_cri_lst <- subset(unlist(sub_criteria, use.names = FALSE),unlist(sub_criteria, use.names = FALSE) %in% names(df))

  if(!is.null(ds)){
    df <- dplyr::rename(df, source= !!ds)
  }else{
    df$source <- "A"
  }

  T1 <- df %>%
    dplyr::select(sn=!!dplyr::enquo(sn), !!dplyr::enquo(criteria), sub_cri_lst, source) %>%
    dplyr::mutate_at(dplyr::vars(!!dplyr::enquo(criteria), sub_cri_lst), as.character) %>%
    dplyr::mutate_at(dplyr::vars(!!dplyr::enquo(criteria), sub_cri_lst), dplyr::funs(ifelse(is.na(.),"",.))) %>%
    dplyr::mutate(pr_sn = dplyr::row_number(), m_tag=0, tag = 0, pid = 0, pid_cri = "None")

  cri_no <- length(cri_lst)

  for(i in 1:cri_no){
    if(display) print(paste("Group criteria",i), sep=" ")

    T1$cri <- T1[[cri_lst[i]]]

    attr <- attributes(sub_criteria)[["names"]]
    attr <- subset(attr, stringr::str_detect(attr,paste("s",i,sep="")))

    curr_attr <- ifelse(length(attr)==0, FALSE, TRUE)

    if(curr_attr){
      func_1 <- function(x){paste("df$",x, "==", "df$tr_",x, sep="")}
      func_2 <- function(x){paste(x, collapse = " | ")}
      func_3 <- function(x){paste("(",x,")", sep="")}

      sub_crx_func <- lapply(sub_criteria[attr], func_1)
      sub_crx_func <- lapply(sub_crx_func, func_2)
      sub_crx_func <- lapply(sub_crx_func, func_3)
      sub_crx_func <- paste(sub_crx_func, collapse = " & ")

      sub_crx_func <- paste("function(df){",sub_crx_func,"}",sep="")
      sub_crx_func <- eval(parse(text = sub_crx_func))
      curr_sub_cri_lst <- unlist(sub_criteria[attr], use.names = FALSE)
    }else{
      sub_crx_func <- function(df){TRUE}
      curr_sub_cri_lst <- "sn"
    }

    T1$skip <- T1$m_tag <- c <- min_m_tag <- min_pid <- 0

    while (min_pid==0 | min_m_tag==-1) {
      T1$force_check <- ifelse(T1$m_tag==-1,2,T1$tag)
      if(c+1 >1 ){
        print(paste("Criteria",i,": iteration ",c+1), sep="")
      }

      TR <- T1 %>%
        dplyr::filter(!.data$cri %in% c("",NA))  %>%
        dplyr::arrange(.data$cri, .data$skip, dplyr::desc(.data$force_check), dplyr::desc(.data$tag), .data$m_tag, .data$sn) %>%
        dplyr::filter(duplicated(.data$cri) == FALSE) %>%
        dplyr::select_at(c("pid","m_tag","tag", "sn","pid_cri","cri",curr_sub_cri_lst)) %>%
        dplyr::rename_at(c("pid","m_tag", "tag", "sn","pid_cri",curr_sub_cri_lst), dplyr::funs(paste("tr_",.,sep="")))

      T1 <- dplyr::left_join(T1,TR, by="cri")

      #sub_cri matching
      T1$sub_cri_match <- ifelse(!sub_crx_func(T1) %in% c(NA, FALSE),1,0)
      T1$skip <- ifelse(T1$sub_cri_match==1,1,T1$skip)

      T1 <- T1 %>%
        dplyr::mutate(
          m_tag = ifelse(.data$m_tag==1 & .data$tr_pid ==0 & .data$sub_cri_match==1, -1, .data$m_tag),
          pid = ifelse(
            .data$tr_m_tag==-1 & .data$pid!=0 & !.data$tr_pid %in% c(0,NA) & .data$sub_cri_match==1,
            .data$tr_pid, .data$pid
          ),
          #inherit pid
          pid = ifelse(
            .data$pid==0 & !.data$tr_pid %in% c(0,NA) & .data$sub_cri_match==1,
            .data$tr_pid, .data$pid
          ),
          #assign new pid
          pid = ifelse(
            .data$pid==0 & .data$tr_pid == 0 & !is.na(.data$tr_pid) & .data$sub_cri_match==1,
            .data$tr_sn, .data$pid
          ),
          m_tag = ifelse(.data$pid !=0 & .data$m_tag != -1,1, .data$m_tag),
          m_tag = ifelse(.data$sn==.data$tr_sn & .data$m_tag ==-1, 1, .data$m_tag )
        )

      min_pid <- T1 %>%
        dplyr::filter(!.data$cri %in% c("",NA)) %>%
        dplyr::select(.data$pid) %>% min()

      min_m_tag <- T1 %>%
        dplyr::filter(!.data$cri %in% c("",NA)) %>%
        dplyr::select(.data$m_tag) %>% min()

      T1 <- dplyr::select(T1, .data$sn, .data$pr_sn, .data$pid, .data$pid_cri, .data$cri, cri_lst, sub_cri_lst,
                          .data$ tag, .data$m_tag, .data$skip, .data$source, .data$force_check)

      c <- c+1
    }

    tagged_1 <- length(subset(T1$pid, !T1$pid %in% c(0,NA) & T1$tag ==0 ))
    total_1 <- length(subset(T1$pid, T1$tag ==0 ))

    if(display) {
      print(
        paste(fmt(tagged_1)," of ", fmt(total_1),
              " record(s) grouped into groups. ", fmt(total_1-tagged_1),
              " records not yet grouped.", sep ="")
      )
    }

    # untagging record groups with containing only one record
    # will attempt to match based on the next criteria
    T1 <- T1 %>%
      dplyr::mutate(
        # keeping track of cases that have not been tagged for the print output
        tag = ifelse(.data$pid %in% c(0,NA),0,1),
        pid = ifelse(duplicated(.data$pid) == FALSE & duplicated(.data$pid, fromLast=TRUE) == FALSE,0,.data$pid))
    # number of records untagged
    removed <- length(subset(T1$pid, T1$pid %in% c(0,NA) & T1$tag ==1 ))

    T1 <- T1 %>%
      dplyr::mutate(
        tag = ifelse(.data$pid!=0,1,0),
        pid_cri = ifelse(
          .data$tag ==1 & .data$pid_cri == "None",
          paste("Criteria",i,sep=" "), .data$pid_cri
        )
      )

    if(display) {
      print(
        paste(fmt(removed), " single-record group(s) untagged for possible matching on the next criteria. Total records not yet grouped is now ", fmt(removed + (total_1-tagged_1)),".", sep ="")
      )
    }
  }

  T1 <- T1 %>%
    # records that have not yet matched anyother based on all criteria are each a considered a unique group
    dplyr::mutate(pid = ifelse(.data$pid==0, .data$sn, .data$pid)) %>%
    dplyr::arrange(.data$pr_sn) %>%
    dplyr::select(.data$sn, .data$pid, .data$pid_cri, .data$source)

  sourc_list <- as.character(sort(unique(T1$source)))

  grps <- T1 %>%
    dplyr::select(.data$pid, .data$source) %>%
    unique() %>%
    dplyr::mutate(val= .data$source) %>%
    dplyr::arrange(.data$source) %>%
    tidyr::spread(key= "source", value= "val") %>%
    tidyr::unite("pid_grp", sourc_list, sep=",") %>%
    dplyr::mutate(pid_grp = stringr::str_replace_all(.data$pid_grp,"NA,|,NA|^NA$",""))

  T1 <- dplyr::left_join(T1, grps, by="pid")

  if(is.null(ds)){
    T1 <- dplyr::select(T1, .data$sn, .data$pid, .data$pid_cri)
  }else{
    T1 <- dplyr::select(T1, .data$sn, .data$pid, .data$pid_cri, .data$pid_grp)
  }

  print(
    paste("Record grouping complete; " ,
          fmt(removed + (total_1-tagged_1))," group(s) contain only one record." , sep ="")
  )

  T1
}
