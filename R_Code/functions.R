split_multicell <- function(df, col, pattern){
  
  df_1 <- df[0,]
  if (length(col) == 1) {
    # Iterate 
    for (i in 1:nrow(df)) {
      # When cell has comma
      if(str_detect(df[i, col], pattern = pattern)){
        nc <- unlist(str_split(df[i, col], pattern))
        d_temp <- df[rep(i, length(nc)),] # Duplicate rows
        d_temp[, col] <- nc # Replace
        df_1 <- rbind(df_1, d_temp) # Save in new df
      }
      else {
        # Else append row
        df_1 <- rbind(df_1, df[i, ])
      }
      
    }
    return(df_1)
  }
  
  ###################################################
  
  # Multiple columns
  if (length(col) > 1) {
    
    for (i in 1:nrow(df)) {
      # Count commas in by col in row i.
      count_s <- str_count(df[i, col], pattern)
      # Cols with more than one comma
      col_comma <- col[which(count_s > 0)]
      count_s1 <- count_s[which(count_s > 0)]
      
      # No commas
      if(length(col_comma) == 0){
        d_temp <- df[i,] # 
        df_1 <- rbind(df_1, d_temp) # Save in new df
      }
      
      
      # If any col with more than one comma
      if(length(col_comma)>0){
        
        # Columns different amount of commas. 
        # Usually a multiple of victims
        if(any(count_s1 != count_s1[1])){
          nc_type <- unlist(str_split(df[i, col_comma[1]], pattern))
          nc_type_unique <- unique(nc_type) 
          nc_vict <- unlist(str_split(df[i, col_comma[2]], pattern))
          # If only one type. 
          if(length(nc_type_unique) == 1){
            d_temp <- df[rep(i, length(nc_vict)),] # Duplicate rows # of victims
            d_temp[, col_comma[2]] <- nc_vict # Replace victims
            d_temp[, col_comma[1]] <- rep(nc_type_unique, length(nc_vict)) # Replace type.
            df_1 <- rbind(df_1, d_temp) # Save in new df
          }
          # When number of types multiple of victims, get unique and assign it.
          
          if(length(nc_type) %% length(nc_vict) == 0){
            
            rem = length(nc_type)/length(nc_vict)
            
            if(rem == length(nc_type_unique)){
              d_temp <- df[rep(i, length(nc_vict)),] # Duplicate rows # of victims
              d_temp[, col_comma[2]] <- nc_vict # Replace victims
              d_temp[, col_comma[1]] <- rep(paste(nc_type_unique, collapse = ", "), length(nc_vict)) # Replace type.
              df_1 <- rbind(df_1, d_temp) # Save in new df
            }
            
          }
          
          else{
            d_temp <- df[i,] # 
            d_temp$non_match_comma <- 1
            df_1 <- rbind(df_1, d_temp) # Save in new df
          }
          
        }

        # All columns same number of commas
        if(all(count_s1 == count_s1[1])){
          # For each column: split and save in df
          # Repeat rows the number of commas + 1. i.e. one comma --> two obs
          d_temp <- df[rep(i, count_s1[1] + 1),]
          d_temp$non_match_comma <- 0
          for (j in 1:length(col_comma)) {
            nc <- unlist(str_split(df[i, col_comma[j]], pattern))
            d_temp[, col_comma[j]] <- nc
          }
          # Append to df
          df_1 <- rbind(df_1, d_temp)
        }
        
      }
      
      
    }
    return(df_1)
    
  }
  
}

#Repeat max amount of commas
# d_temp <- df[rep(i, which.max(count_s1) + 1),]
# for (j in 1:length(col_comma)){
#   nc <- unlist(str_split(df[i, col_comma[j]], pattern))
#   nc <- rep(nc, length.out = which.max(count_s1) + 1)
#   d_temp[, col_comma[j]] <- nc
# }



clean_string <- function(x){
  y <- x %>% str_c(collapse = "---") %>% 
    str_replace_all(c("ú" = "u","é" = "e","ü" = "u", "�???" = "e", 
                      "á" = "a", "ó" = "o", "\u0081" = "", "\u008d" = "",
                      "ñ" = "n","�"" = "o",  "�" = "i", "\t" = "")) %>% 
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
    df_new <- df_new %>% mutate(across(!year & !codmpio, ~ replace_na(.x, 0)))

  }
  
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


reg_tab <- function(out_vars, controls_list, subset_logic, running_var, digits, 
                    out_vars_names, ...){

  col_num <- length(controls_list)*2 + 2
  
  right_results <- matrix(nrow = 2*length(out_vars), ncol = col_num)
  right_results <- data.frame(right_results)
  
  i <- 1
  for (out_v in out_vars) {
    j <- 2
    print(out_v)
    for (pol in c(1,2)){
      for (c in controls_list) {
        if (c[1] == "") {
          mod <- rdrobust(RD_baseline[[out_v]], RD_baseline[[running_var]], p = pol, 
                          all = T, subset = subset_logic, ...)
          beta <- round(mod$coef["Robust",], digits)
          se <- round(mod$se["Robust",], digits)
          bwd <- mod$bws[2,2]
          obs <- sum(mod$N_h)
          p_val <- mod$pv["Robust",]
        }
        else {
          mod <- rdrobust(RD_baseline[[out_v]], RD_baseline[[running_var]], p = pol, 
                          all = T, covs = RD_baseline[c], subset = subset_logic, ...)
          beta <- round(mod$coef["Robust",], digits)
          se <- round(mod$se["Robust",], digits)
          bwd <- mod$bws[2,2]
          obs <- sum(mod$N_h)
          p_val <- mod$pv["Robust",]
        }
        
        if (p_val < 0.01){
          beta <- paste0(beta, "***")
        } else if (p_val < 0.05){
          beta <- paste0(beta, "**")
        } else if (p_val < 0.1){
          beta <- paste0(beta, "*")
        }
        
        right_results[2*i - 1,j] <- beta
        right_results[2*i ,j] <- paste0("(", se, ")")
        j <- j + 1
        if(j == col_num){
          right_results[2*i-1,col_num] <- obs
          right_results[2*i,col_num] <- ""
        }
        
      }
      
      
    }
    i <- i + 1
  }
  ## Add covariates rows
    # First columns
  
  g <- c("")
  s <- c("")
  p <- c("")
  inc <- c("")
  controls <- c("")
  j <- 2
  col_n <-  c("Dependent Variable")
  for (pol in c(1,2)){
    for (i in 1:length(controls_list)){
      p[j] <- pol
      if(names(controls_list)[i] == "nc"){
        g[j] <- "NO"
        s[j] <- "NO"
        inc[j] <- "NO"
      }
      
      if(names(controls_list)[i] == "gc"){
        g[j] <- "YES"
        s[j] <- "NO"
        inc[j] <- "NO"
      }
      
      if(names(controls_list)[i] == "sc"){
        g[j] <- "NO"
        s[j] <- "YES"
        inc[j] <- "NO"
      }
      
      if(names(controls_list)[i] == "gsc"){
        g[j] <- "YES"
        s[j] <- "YES"
        inc[j] <- "NO"
      }
      
      if(names(controls_list)[i] == "gsci"){
        g[j] <- "YES"
        s[j] <- "YES"
        inc[j] <- "YES"
      }
      col_n <- c(col_n, paste0("(", j-1 ,")"))
      
      j <- j + 1
      
    }
  }
  # Add last columns
  g <- c(g, "")
  s <- c(s, "")
  p <- c(p, "")
  inc <- c(inc, "")
  col_n <- c(col_n, "N")
  out_vars_names_1 <- out_vars_names
  out_num <- 1:length(out_vars_names)
  out_vars_names_1[2*out_num] <- ""
  out_vars_names_1[2*out_num - 1] <- out_vars_names
  out_names <- c(out_vars_names_1, "Geo Controls", "Socioeconomic Controls", "Incumbency controls",
                         "Polynomial")
  right_results <- right_results %>% rbind(g,s,inc, p)
  colnames(right_results) <- col_n
  right_results[,1] <- out_names
  return(right_results)
}



