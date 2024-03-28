split_multicell <- function(df, col, pattern){
  

  df_1 <- df[0,]
  if (length(col) == 1) {
    # Iterate 
    for (i in 1:nrow(df)) {
      # When cell has comma
      if(str_detect(df[i, col], pattern = pattern)){
        print(c(i, "row dup"))
        nc <- unlist(str_split(df[i, col], pattern))
        d_temp <- df[rep(i, length(nc)),] # Duplicate rows
        d_temp[, col] <- nc # Replace
        print(c(colnames(d_temp)))
        print(c(colnames(df_1)))
        df_1 <- rbind(df_1, d_temp) # Save in new df
      }
      else {
        # Else append row
        df_1 <- rbind(df_1, df[i, ])
      }
      
    }
    return(df_1)
  }
  # Multiple columns
  if (length(col) > 1) {
    
    for (i in 1:nrow(df)) {
      # Count commas in by col in row i.
      count_s <- str_count(df[i, col], pattern)
      # Cols with more than one comma
      col_comma <- col[which(count_s > 0)]
      count_s1 <- count_s[which(count_s > 0)]
      # If any col with more than one comma
      if(length(col_comma)>0){
        # All columns same number of commas
        if(all(count_s1 == count_s1[1])){
          # For each column: split and save in df
          print(count_s1[1])
          # Repeat rows the number of commas + 1. i.e. one comma --> two obs
          d_temp <- df[rep(i, count_s1[1] + 1),]
          for (j in 1:length(col_comma)) {
            nc <- unlist(str_split(df[i, col_comma[j]], pattern))
            print(nc)
            print(d_temp[, col_comma[j]])
            d_temp[, col_comma[j]] <- nc
          }
          # Append to df
          df_1 <- rbind(df_1, d_temp)
        }
        
        # Columns different amount of commas
        if(any(count_s1 != count_s1[1])){
          # Repeat max amount of commas
          d_temp <- df[rep(i, which.max(count_s1) + 1),]
          for (j in 1:length(col_comma)){
            nc <- unlist(str_split(df[i, col_comma[j]], pattern))
            nc <- rep(nc, length.out = which.max(count_s1) + 1)
            d_temp[, col_comma[j]] <- nc
          }
          df_1 <- rbind(df_1, d_temp)
          
        }
        
      }
      else {
        # Else append row
        df_1 <- rbind(df_1, df[i, ])
      }
      
      
    }
    return(df_1)
    
  }
  
} 

clean_string <- function(x){
  y <- x %>% str_c(collapse = "---") %>% 
    str_replace_all(c("ú" = "u","é" = "e","ü" = "u", 
                      "á" = "a", "ó" = "o", "\u0081" = "", "\u008d" = "",
                      "ñ" = "n",  "�" = "i", "\t" = "", "i" = "o")) %>% 
    strsplit(split = "---") %>% unlist() %>% tolower()
  return(y)
}

min_dist_str <- function(x,y){
  dist <- stringdist(x,  y, method = "lv")
  m_dist <- min(dist)
  match_n <- y[dist == m_dist]
  return(match_n)
}

summarize_data_count <- function(df, cols, new_names){

  df_new <- expand.grid(unique(df$year), unique(df$codmpio))
  colnames(df_new) <- c("year", "codmpio")
  for (i in 1:length(cols)) {
    print(new_names[i])
    # Filter col == 1.
    temp <- df[df[cols[i]] == 1,]
    # Summarize 
    temp <- temp %>% ungroup() %>%  group_by(year, codmpio) %>% summarise(a = sum(.data[[cols[i]]]))
    colnames(temp) <- c("year", "codmpio", new_names[i])
    print(temp)
    df_new <- df_new %>% full_join(temp, by = c("year", "codmpio"))
  }
  df_new <- df_new %>% mutate(across(lid_asis:vio, ~ replace_na(.x, 0)))
  
  return(df_new)
  
}


summarize_data_count_2 <- function(df, cols, new_names){
  
  df_new <- expand.grid(unique(df$year), unique(df$codmpio), unique(df$month), unique(df$day))
  colnames(df_new) <- c("year", "codmpio", "month", "day")
  for (i in 1:length(cols)) {
    print(new_names[i])
    # Filter col == 1.
    temp <- df[df[cols[i]] == 1,]
    # Summarize 
    var <- cols[i]
    temp <- temp %>% ungroup() %>%  group_by(year, codmpio, month, day) %>% summarise(a = sum(.data[[cols[i]]]))
    colnames(temp) <- c("year", "codmpio", "month", "day", new_names[i])
    print(temp)
    df_new <- df_new %>% full_join(temp, by = c("year", "codmpio", "month", "day"))
  }
  df_new <- df_new %>% mutate(across(lid_asis:vio, ~ replace_na(.x, 0)))
  
  return(df_new)
  
}



election_share <- function(df, ideol, elec_years){
  
  mayor <- df %>% filter(curules == 1) %>% 
    select(codmpio, tradicional, ideologia, ano) %>% 
    rename(
      tradicional_incum = "tradicional",
      ideol_incum = "ideologia") %>% 
    # Select only 2011 and 2015
    filter(ano %in% (elec_years -4)) %>% 
    # Set next election year to merge. 
    mutate(ano = ano + 4)
  
  # Filter first two candidates
  df <- df %>% group_by(codmpio, ano) %>% 
    mutate(vote_rank = rank(-votos, ties.method = "first")) %>% 
    filter(vote_rank <= 2.5) 
  # Filter elections with only one candidate for specific ideol.
  df <- df %>% group_by(codmpio, ano) %>% 
    filter(length(which(ideologia == ideol)) == 1)
  
  # Join and clean
  df <- df %>% filter(ano %in% elec_years) %>% 
    left_join_rep(mayor, by = c("codmpio", "ano")) %>% 
     select(ano, coddpto, codmpio, 
           votos, censoe_total, tradicional, tradicional,
           ideologia, paso_ideologia, vote_rank, ideol_incum, 
           tradicional_incum, part_camara, part_concejo, part_gobernacion, 
           part_senado)
  
  # Create running variable. 
  df <- df %>% group_by(ano, codmpio) %>% 
    mutate(total_votes = sum(votos)) %>% ungroup()%>% 
    mutate(vote_share = votos/total_votes, 
           right_ideol = case_when(ideologia == ideol ~ 1, 
                                   ideologia != ideol ~ 0, 
                                   is.na(ideologia) ~ 0))
  ## Running variable
  df <- df  %>%  pivot_wider(
    id_cols = ano:codmpio,
    names_from = right_ideol, 
    values_from = c(votos, ideologia, paso_ideologia, vote_share, vote_rank, 
                    tradicional, ideol_incum, tradicional_incum, part_camara, 
                    part_concejo, part_gobernacion, part_senado)
  )
  print(head(df %>% select(vote_share_1, vote_share_0)))
  var_name <- paste0("share_diff", as.character(ideol))
  df <- df %>% mutate(!!(var_name) := vote_share_1-vote_share_0) %>% 
    select(-c(ideol_incum_0, tradicional_incum_0)) %>% 
    rename(ideol_incum = "ideol_incum_1", 
           traditional_incum = "tradicional_incum_1")
  
  
  return(df)
}

left_join_rep <- function(df1, df2, by){
  merged_dat <- left_join(df1, df2, by = by)
  inner <- inner_join(df1, df2, by = by)
  op_mrge <- left_join(df2, df1, by = by)
  
  mas_non_matchs <- as.character(dim(merged_dat)[1] - dim(inner)[1])
  ext_non_match <- as.character(dim(op_mrge)[1] - dim(inner)[1])
  
  print(paste0("There are ", mas_non_matchs, " non matched rows in master"))
  print(paste0("There are ", ext_non_match, " non matched rows from external"))
  return(merged_dat)
}

