generate_bingo_card <- function() {
  # Define column ranges
  col_ranges <- list(
    1:9, 10:19, 20:29, 30:39, 40:49,
    50:59, 60:69, 70:79, 80:90
  )
  
  repeat {
    # Create an empty card with NA values
    card <- matrix(NA, nrow = 3, ncol = 9)
    positions <- matrix(0, nrow = 3, ncol = 9)
    
    # Step 1: Randomly assign 5 filled positions per row
    for (i in 1:3) {
      positions[i, sample(1:9, 5)] <- 1
    }
    
    # Step 2: Ensure every column has at least one number
    cols_with_numbers <- colSums(positions)
    if (all(cols_with_numbers > 0)) break
  }
  
  # Step 3: Fill in numbers from correct column ranges
  for (col in 1:9) {
    n_numbers <- sum(positions[, col])
    numbers <- sort(sample(col_ranges[[col]], n_numbers))
    rows <- which(positions[, col] == 1)
    for (i in seq_along(rows)) {
      card[rows[i], col] <- numbers[i]
    }
  }
  
  card
}

format_bingo_card <- function(card) {
  apply(card, 1, function(row) {
    paste(sprintf("%2s", ifelse(is.na(row), ".", row)), collapse = " ")
  })
}

generate_and_save_bingo_cards <- function(n_cards = 3, filename = "bingo_tickets.txt") {
  lines <- c()
  for (i in 1:n_cards) {
    card <- generate_bingo_card()
    lines <- c(lines, sprintf("Card %d:", i))
    lines <- c(lines, format_bingo_card(card))
    lines <- c(lines, "")  # Blank line between cards
  }
  writeLines(lines, con = filename)
  message("Bingo cards saved to ", filename)
}

# Run it
set.seed(42)
generate_and_save_bingo_cards(n_cards = 3)
