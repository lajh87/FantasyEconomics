check_previous <- function(data_in, game_round){
  if(game_round == 1) return(TRUE)
  if(game_round == "Casino") return(TRUE)

  check <- purrr::map_df(1:nrow(data_in), function(i){

    policy <- data_in[i, "key_short_name"]
    policy_key <- policies %>% dplyr::filter(key_short_name == policy) %>%
      dplyr::select(key) %>% dplyr::pull()

    previous_policies <- unlist(stringr::str_split(data_in[i, "previous_policies"], ", "))
    check <- policy_key %in% previous_policies

    data.frame(team_name = data_in[i, "team_name"], key_short_name = policy,
               check, stringsAsFactors = FALSE)
  }) %>%
    dplyr::select(check) %>%
    dplyr::pull()

  if(game_round >1 & game_round < 5){

    return(!TRUE %in% check)

  } else{

    check2 <- purrr::map_df(1:nrow(data_in), function(i){

      policy <- data_in[i, "policy2"]
      policy_key <- policies %>% dplyr::filter(key_short_name == policy) %>%
        dplyr::select(key) %>% dplyr::pull()

      previous_policies <- unlist(stringr::str_split(data_in[i, "previous_policies"], ", "))
      check <- policy_key %in% previous_policies

      data.frame(team_name = data_in[i, "team_name"], key_short_name = policy,
                 check, stringsAsFactors = FALSE)
    }) %>%
      dplyr::select(check) %>%
      dplyr::pull()

    return(!TRUE %in% check & !TRUE %in% check2)
  }

}

check_both_policies <- function(data_in){
  check <- data_in %>%
    dplyr::mutate(check = key_short_name != policy2) %>%
    dplyr::select(check) %>%
    dplyr::pull()

  return(!FALSE %in% check)
}

previousPolicyWarning <- function(){
  showModal(modalDialog(size = "s",
    title = list(icon("warning"), "Error"),
    "Policy Cannot be the Same as a Previous Policy"
  ))
}

duplicatePolicyWarning <- function(){
  showModal(modalDialog(size = "s",
    title = list(icon("warning"), "Error"),
    "Both Policies Must Be Unique"
  ))
}
