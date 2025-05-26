get_labels_fixed <- function(base_url,
                             initial_cursor_value,
                             throttle_settings = c(requests_before_pause = 100,
                                                   pause_duration_sec = 5)) {
  # Ensure httr2 and purrr are loaded
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required but not installed.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required but not installed.")
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("Package 'glue' is required but not installed.")
  }
  
  # Configuration for requests
  request_limit <- 250  # Number of items per page
  current_api_cursor <- initial_cursor_value
  
  # List to accumulate all fetched labels (or full page data)
  all_fetched_data_pages <- list()
  
  # Iteration counter for throttling and indexing
  iteration_num <- 0
  
  # Base httr2 request object
  base_api_request <- httr2::request(base_url)
  
  # Define a function to safely perform the API request
  safely_perform_request <- purrr::safely(function(req_obj,
                                                   cursor_str,
                                                   limit_val,
                                                   current_iter,
                                                   throttle_conf) {
    # Throttling logic
    if (throttle_conf$requests_before_pause > 0 &&
        current_iter > 0 &&
        (current_iter %% throttle_conf$requests_before_pause == 0)) {
      message(
        glue::glue(
          "Throttling: Iteration {current_iter}. Pausing for {throttle_conf$pause_duration_sec} seconds."
        )
      )
      Sys.sleep(throttle_conf$pause_duration_sec)
    }
    
    # Perform the request
    response <- httr2::req_url_query(
      req_obj,
      uriPatterns = "*",
      # Parameter specific to com.atproto.label.queryLabels
      cursor = cursor_str,
      limit = limit_val
    ) |>
      httr2::req_perform()  |>
      httr2::resp_body_json() # Parses JSON response into an R list
    
    return(response)
  })
  
  # Main pagination loop
  repeat {
    iteration_num <- iteration_num + 1
    message(glue::glue(
      "Fetching page {iteration_num} with cursor: {current_api_cursor}"
    ))
    
    # Attempt the API call
    api_call_outcome <- safely_perform_request(
      req_obj = base_api_request,
      cursor_str = current_api_cursor,
      limit_val = request_limit,
      current_iter = iteration_num,
      throttle_conf = list(
        requests_before_pause = throttle_settings[[1]],
        pause_duration_sec = throttle_settings[[2]]
      )
    )
    
    # 1. CRITICAL: Check for errors from `safely_perform_request` first
    if (!is.null(api_call_outcome$error)) {
      warning(
        glue::glue(
          "API request failed on iteration {iteration_num} (cursor: {current_api_cursor}). Error: {api_call_outcome$error$message}"
        )
      )
      break # Exit loop on error
    }
    
    # 2. Extract the actual data from the successful call
    current_page_content <- api_call_outcome$result
    
    # 3. Validate the structure of `current_page_content`
    if (is.null(current_page_content)) {
      warning(
        glue::glue(
          "API call on iteration {iteration_num} was successful but returned NULL content."
        )
      )
      break # Exit if content is unexpectedly NULL
    }
    
    # Store the received page data (this will be a list, likely containing 'labels' and 'cursor')
    all_fetched_data_pages[[iteration_num]] <- current_page_content
    
    # Log how many labels were fetched in this page, assuming 'labels' is the key
    if (!is.null(current_page_content$labels)) {
      message(
        glue::glue(
          "Fetched {length(current_page_content$labels)} labels on page {iteration_num}."
        )
      )
    } else {
      message(
        glue::glue(
          "Page {iteration_num} content did not contain a 'labels' field as expected."
        )
      )
    }
    
    # 4. Determine the next cursor and loop termination conditions
    next_api_cursor <- current_page_content$cursor
    
    if (is.null(next_api_cursor) ||
        !nzchar(as.character(next_api_cursor))) {
      message(
        glue::glue(
          "No new cursor provided in response on page {iteration_num}. Assuming end of data."
        )
      )
      
      readr::write_file(as.character(as.numeric(current_api_cursor) + 1), fs::path_wd("cursor.txt"))
      break # No further cursor, so stop
    }
    
    if (identical(as.character(next_api_cursor),
                  as.character(current_api_cursor))) {
      message(
        glue::glue(
          "New cursor ('{next_api_cursor}') is identical to the current cursor. Stopping to prevent infinite loop."
        )
      )
      break # Cursor hasn't changed, stop
    }
    
    # Optional: Stop if no labels are returned (even if a cursor is present, though unusual)
    # This depends on API behavior; some APIs might return a cursor with an empty list of items for the last page.
    if (exists("labels", where = current_page_content) &&
        length(current_page_content$labels) == 0) {
      message(
        glue::glue(
          "No labels returned on page {iteration_num}, though a new cursor '{next_api_cursor}' was provided. Assuming end of effective data."
        )
      )
      break
    }
    
    
    # Update cursor for the next iteration
    current_api_cursor <- next_api_cursor
    
  } # End of repeat loop
  
  message(
    glue::glue(
      "Finished fetching data. Total pages retrieved: {length(all_fetched_data_pages)}."
    )
  )
  
  # You might want to combine the 'labels' from all pages:
  # all_labels <- purrr::map(all_fetched_data_pages, "labels") |> purrr::list_flatten()
  # return(all_labels)
  
  return(all_fetched_data_pages) # Returns a list of page data structures
}

if (file.exists(fs::path_wd("cursor.txt"))) {
  cursor <- readr::read_file(fs::path_wd("cursor.txt"))
}

results <- get_labels_fixed(
  base_url = "https://ozone.skywatch.blue/xrpc/com.atproto.label.queryLabels",
  initial_cursor_value = cursor,
  # Ensure this is the correct format (string/numeric) for the API
  throttle_settings = c(
    requests_before_pause = 50,
    pause_duration_sec = 10
  ) # Adjust as needed
)

# To extract just the labels from the results:
all_labels_extracted <- purrr::map(results, "labels") |> purrr::list_flatten() |>
  dplyr::bind_rows() |> 
  readr::write_rds("all_labels.rds")
