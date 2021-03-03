load.audit.files <- function(directory.audits){
  audit.filenames <- list.files(directory.audits, pattern="audit.csv", recursive=TRUE, full.names=TRUE)
  print(paste("Loading",length(audit.filenames),"audit logs"))
  for (filename in audit.filenames){
    # get uuid from filename
    sp <- strsplit(filename, "\\/")[[1]]
    uuid <- sp[length(sp)-1]
    # load file
    audit <- read.csv(filename, stringsAsFactors = FALSE) %>% mutate(uuid=uuid, .before=1)
    if (filename==audit.filenames[1]) res <- audit
    else res <- rbind(res, audit)
  }
  res <- res %>% 
    mutate(duration=(end-start)/1000,
           question=sapply(str_split(node, '\\/'), function(x){
             return(x[length(x)])})) %>% 
    mutate(event=str_replace_all(event, " ", "."))
  return(res)
}

load.audit.files.msna <- function(directory.audits){
  audit.filenames <- list.files(directory.audits, pattern="audit.csv", recursive=TRUE, full.names=TRUE)
  print(paste("Loading",length(audit.filenames),"audit logs"))
  for (filename in audit.filenames){
    # get uuid from filename
    sp <- strsplit(filename, "\\/")[[1]]
    uuid <- sp[length(sp)-1]
    # load file
    audit <- read.csv(filename, stringsAsFactors = FALSE) %>% mutate(uuid=uuid, .before=1)
    if (filename==audit.filenames[1]) res <- audit
    else res <- rbind(res, audit)
  }
  res <- res %>% 
    mutate(duration=(end-start)/1000,
           question=sapply(str_split(node, '\\/'), function(x){
             return(x[length(x)])}),
           section=sapply(str_split(node, '\\/'), function(x){
             s <- x[3]
             return(ifelse(grepl("[", s, fixed = TRUE),
                           str_split(s, '\\[')[[1]][1],
                           s))})) %>% 
    mutate(question=ifelse(!is.na(section)&is.na(question), section, question)) %>% 
    mutate(event=str_replace_all(event, " ", "."))
  return(res)
}

load.survey.data <- function(directory.survey.data, col.uuid, col.enum){
  filename <- list.files(directory.survey.data, pattern=".xlsx|csv", all.files=TRUE, recursive=TRUE, full.names=TRUE)
  if (length(filename) > 1) stop("The directory survey_data must contain only 1 file.")
  
  if (grepl(".xlsx", filename)) survey.data <- read_xlsx(filename)
  else if (grepl(".csv", filename)) survey.data <- read_csv(filename)
  else stop("Survey data should either be a .xlsx or .csv file.")
  
  survey.data <- survey.data %>% 
    mutate(uuid=!!sym(col.uuid),
           enum=str_replace_all(tolower(!!sym(col.enum)), " ","")) %>% 
    select(uuid, enum) %>% distinct()
  return(survey.data)
}

get.summary.general <- function(audit){
  survey.duration <- audit %>% 
    group_by(uuid) %>% 
    summarise(event="survey.duration", n=(max(start)-min(start))/1000)
  
  survey.info.question <- audit %>% 
    filter(event %in% c("question", "group.questions")) %>% 
    group_by(uuid) %>% 
    summarise(survey.response.time=sum(duration, na.rm=TRUE),
              num.unique.questions=length(unique(question)),
              num.unique.nodes=length(unique(node)),
              avg.edits.per.question=round(n()/length(unique(question)), 2))
  survey.info.question.pivot <- pivot_longer(survey.info.question, 
                                             cols = c("survey.response.time", "num.unique.questions", "num.unique.nodes", "avg.edits.per.question"),
                                             names_to = "event",values_to = "n")
  
  summary.general <- audit %>% 
    group_by(uuid, event) %>% 
    summarise(n=n()) %>% 
    rbind(survey.duration, survey.info.question.pivot)
  return(summary.general)
}

get.summary.section <- function(audit){
  summary.section <- audit %>% filter(node!="") %>% 
    group_by(uuid, section) %>% summarise(tot.duration=sum(duration, na.rm=TRUE)) %>% 
    mutate(tot.duration.log10=log10(tot.duration)) %>%
    group_by(section) %>% 
    summarise(num.surveys=n(),
              mean=mean(tot.duration.log10), 
              sd=sd(tot.duration.log10),
              median.response.time=round(median(tot.duration),2)) %>% 
    mutate(section=factor(section, levels = as.character(section)))
  return(summary.section)
}

get.summary.question <- function(audit){
  summary.question <- audit %>% filter(node!="") %>% 
    group_by(uuid, question) %>% summarise(tot.duration=sum(duration, na.rm=TRUE)) %>% 
    mutate(tot.duration.log10=log10(tot.duration)) %>%
    group_by(question) %>% 
    summarise(num.surveys=n(),
              mean=mean(tot.duration.log10), 
              sd=sd(tot.duration.log10),
              median.response.time=round(median(tot.duration),2))
  return(summary.question)
}