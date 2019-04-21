#' @title Deterministic linkage for epidemiological analysis
#'
#' @description This function links records from one ore more datasets on a unique idenfier using matching or partial matching criteria
#'
#'
#' @param df A dataframe
#' @param sn A column of unique indentifiers for each record in 'df'
#' @param criteria Character vector of columns in 'df' representing matching criteria
#' @param sub_criteria Character vector of columns in 'df' representing matching sub-criteria within each 'criteria'
#' @param data_source A colum identifier indicating the source of each record. Usefull when tracking record groups across mutliple datasets.
#' @param display If TRUE, progress status at each stage of record groupping is displayed on screen
#'
#' @return Dataframe with a unique group identifier based on the matching criteria and sub-criter
#'
#' @examples
#'
#'
#'
#' @export

record_group <- function(df, sn, criteria, sub_criteria=NULL, data_source = NULL, display=TRUE){
  #Later, add data validation to ensure -asser that
  #1. sn is length 1
  #2. criteria is > length 0

  enq_vr <- function(x, vr){
    x <- names(select(x, !!vr))

    if(length(x)==0){
      x <- NULL
    }else{
      x
    }
    return(x)
  }

  ds <- enq_vr(df, dplyr::enquo(data_source))



  df_vars <- names(df)

  #confirm fields names exist
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
    dplyr::mutate(pr_sn= dplyr::row_number(), m_tag=0, tag = 0, pid = 0, pid_cri = "None")

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
      if(c+1 >1 ){
        print(paste("Criteria",i,": iteration ",c+1), sep="")
      }

      TR <- T1 %>%
        dplyr::filter(!cri %in% c("",NA))  %>%
        dplyr::arrange(cri, skip, dplyr::desc(tag), m_tag, sn) %>%
        dplyr::filter(duplicated(cri) == FALSE) %>%
        dplyr::select_at(c("pid","m_tag","tag", "sn","pid_cri","cri",curr_sub_cri_lst)) %>%
        dplyr::rename_at(c("pid","m_tag", "tag", "sn","pid_cri",curr_sub_cri_lst), dplyr::funs(paste("tr_",.,sep="")))

      T1 <- dplyr::left_join(T1,TR, by="cri")

      #sub_cri matching
      T1$sub_cri_match <- ifelse(!sub_crx_func(T1) %in% c(NA, FALSE),1,0)
      T1$skip <- ifelse(T1$sub_cri_match==1,1,T1$skip)

      T1 <- T1 %>%
        dplyr::mutate(
          m_tag = ifelse(m_tag==1 & tr_pid ==0 & sub_cri_match==1, -1, m_tag),
          pid = ifelse(
            tr_m_tag==-1 & pid!=0 & !tr_pid %in% c(0,NA) & sub_cri_match==1,
            tr_pid, pid
          ),
          #inherit pid
          pid = ifelse(
            pid==0 & !tr_pid %in% c(0,NA) & sub_cri_match==1,
            tr_pid, pid
          ),
          #assign new pid
          pid = ifelse(
            pid==0 & tr_pid == 0 & !is.na(tr_pid) & sub_cri_match==1,
            tr_sn, pid
          )
          ,
          m_tag = ifelse(pid !=0 & m_tag != -1,1, m_tag),
          m_tag = ifelse(sn==tr_sn & m_tag ==-1, 1, m_tag )
        )

      min_pid <- T1 %>%
        dplyr::filter(!cri %in% c("",NA)) %>%
        dplyr::select(pid) %>% min()

      min_m_tag <- T1 %>%
        dplyr::filter(!cri %in% c("",NA)) %>%
        dplyr::select(m_tag) %>% min()

      T1 <- dplyr::select(T1, sn, pr_sn, pid, pid_cri, cri, cri_lst, sub_cri_lst, tag, m_tag, skip, source)

      c <- c+1
    }

    tagged_1 <- subset(T1$pid, !T1$pid %in% c(0,NA) & T1$tag ==0 ) %>%  length()
    total_1 <- subset(T1$pid, T1$tag ==0 ) %>%  length()

    fmt <- function(g) formatC(g, format="d", big.mark=",")

    if(display) {
          print(
            paste(fmt(tagged_1)," of ", fmt(total_1),
                  " record(s) grouped into groups. ", fmt(total_1-tagged_1),
                  " records not yet grouped.", sep ="")
          )
      }

    #untagging pid groups with only one record to match on the next criteria
    T1 <- T1 %>%
      dplyr::mutate(
        #keeping track of cases that have not been tagged for the print output
        tag = ifelse(pid %in% c(0,NA),0,1),
        #alternative to using group_by to figure it out
        pid = ifelse(
          duplicated(pid) == FALSE & duplicated(pid, fromLast=TRUE) == FALSE,
          0,pid
        ))
    #number of recordsd untagged
    removed <- subset(T1$pid, T1$pid %in% c(0,NA) & T1$tag ==1 ) %>%  length()
    T1 <- T1 %>%
      dplyr::mutate(
        #records tagged with Group id
        tag = ifelse(pid!=0,1,0),
        #matching criteria. numbered in the order provided
        pid_cri = ifelse(
          tag ==1 & pid_cri == "None",
          paste("Criteria",i,sep=" "), pid_cri
        )
      )

    if(display) {
        print(
          paste(fmt(removed), " unique group record(s) untagged for possible matching in the next round. Total records to be grouped is now ", fmt(removed + (total_1-tagged_1)),".", sep ="")
        )
    }
  }

  T1 <- T1 %>%
    #records that don't match anyother record on any of the criteria get unique ids
    dplyr::mutate(pid = ifelse(pid==0, sn,pid)) %>%
    #fields of interest
    dplyr::arrange(pr_sn) %>%
    dplyr::select(sn,pid,pid_cri,source)

  sourc_list <- as.character(sort(unique(T1$source)))

  grps <- T1 %>%
    dplyr::select(pid, source) %>%
    unique() %>%
    dplyr::mutate(val=source) %>%
    dplyr::arrange(source) %>%
    tidyr::spread(key=source, value=val) %>%
    tidyr::unite(pid_grp, sourc_list, sep=",") %>%
    dplyr::mutate(pid_grp = stringr::str_replace_all(pid_grp,"NA,|,NA|^NA$",""))

  T1 <- dplyr::left_join(T1, grps, by="pid")

  if(is.null(ds)){
    T1 <- dplyr::select(T1,sn,pid,pid_cri)
  }else{
    T1 <- dplyr::select(T1,sn,pid,pid_cri,pid_grp)
  }

  print(
    paste("Group ID complete; " ,
          fmt(removed + (total_1-tagged_1))," groups with one record." , sep ="")
  )

  T1
}
