validate_dbnp_features <- function(feature_file,data_file,fsep=",",dsep=",",check_features_in_data=FALSE) {
  
  ft_path <- file_path_info(feature_file)
  FEATURE_FILE_BASE_PATH <- file.path(ft_path$dir,ft_path$name)
  
  dt_path <- file_path_info(data_file)
  DATA_FILE_BASE_PATH <- file.path(dt_path$dir,dt_path$name)
  
  # check names of features in feature_file
  #"" load feature file
  if(fsep==",") {
    features <- read.csv(feature_file,stringsAsFactors=FALSE)
  } else if (fsep==";") {
    features <- read.csv2(feature_file,stringsAsFactors=FALSE)
  } else {
    features <- read.table(feature_file,sep=fsep,stringsAsFactors=FALSE)
  }

  feature_names <- features[,1]
  
  ## check for duplicate features:
  if(any(table(feature_names)>1)) {
    dup_fnames <- names(which(table(feature_names)>1))
    dup_features <- features[which(features[,1] %in% dup_fnames),1:3]
    
    dup_features[,"row"] <- rownames(dup_features)

    # save duplicates to file (sort by feature name)
    write.csv2(dup_features[order(dup_features[,1]),],paste0(FEATURE_FILE_BASE_PATH,"_duplicate_feature_names.csv"),row.names=FALSE,na="")
    cat("There are duplicate feature names. Duplicates have been saved to '",paste0(FEATURE_FILE_BASE_PATH,"_duplicate_feature_names.csv"),"'\n",sep="")
  }
  
  ## load data (only first 10 rows, need column headers and a few rows to guard against errors in header)
  if(dsep==",") {
    data <- read.csv(data_file,nrows=10,stringsAsFactors=FALSE)
  } else if (dsep==";") {
    data <- read.csv2(data_file,nrows=10,stringsAsFactors=FALSE)
  } else {
    data <- read.table(data_file,sep=dsep,header=TRUE,nrows=10,stringsAsFactors=FALSE)
  }
  
  dnames <- names(data)
  
  ## check for duplicate variable names
  if(any(table(dnames)>1)) {
    dup_dnames <- as.data.frame(varnames=names(which(table(dnames)>1)),stringsAsFactors=FALSE)
    # save duplicates to file (sort by feature name)
    write.csv2(dup_dnames,paste0(DATA_FILE_BASE_PATH,"_duplicate_varnames.csv"),row.names=FALSE,na="")
    cat("There are duplicate variable names in data. The duplicate names have been saved to '",paste0(DATA_FILE_BASE_PATH,"_duplicate_varnames.csv"),"'\n",sep="")
  }
  
  ## any feature names not in data (not necessarily a problem)
  if(check_features_in_data && any(!(feature_names %in% dnames))) {
    feature_miss <- as.data.frame(list(feature_names=feature_names[which(!(feature_names %in% dnames))]),stringsAsFactors=FALSE)
    write.csv2(feature_miss,paste0(FEATURE_FILE_BASE_PATH,"_features_not_in_data.csv"),row.names=FALSE,na="")
    cat("Some features are not in the data file. The missing features have been saved to '",paste0(FEATURE_FILE_BASE_PATH,"_features_not_in_data.csv"),"'\n",sep="")
  }
  
  ## any varnames not in feature list - problematic
  if(any(!(dnames %in% feature_names))) {
    dname_miss <- dnames[which(!(dnames %in% feature_names))]
    print(dname_miss)
    dname_miss_df <- as.data.frame(list(varnames=dname_miss),stringsAsFactors=FALSE)
    print(head(dname_miss_df))
    write.csv2(dname_miss_df,paste0(DATA_FILE_BASE_PATH,"varnames_not_in_features.csv"),row.names=FALSE,na="")
    cat("Some variable names are not described in the the feature file. Missing variable names have been saved to '",paste0(DATA_FILE_BASE_PATH,"varnames_not_in_features.csv"),"'\n",sep="")
  }
}
