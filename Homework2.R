# Homework 2
# Run from top to bottom in a fresh R session.

library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

# =============================================================================
# Problem 1: Clean, validate, and summarize
# =============================================================================

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# ---- Part A: Text and numerical transformations -----------------------------

sales_clean <- sales_raw |>
  mutate(
    # str_squish() trims both ends AND collapses inner runs of spaces to one
    item_clean    = str_to_lower(str_squish(item_raw)),
    amount        = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount    = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20  ~ "large",
      .default          = "small"
    )
  )

sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size) |>
  print()

# Output:
#   sale_id item_clean amount discount_rate net_amount size
#         1 pen            10          0          10   small
#         2 pen            20          0.1        18   small
#         3 notebook       NA          0.05       NA   unrecorded
#         4 pen            15          0          15   small
#         5 folder         30          0.2        24   large
#         6 folder         25          0.1        22.5 large
#         7 eraser         NA          0          NA   unrecorded
#         8 pen            20          0          20   large
#
# Explanation: str_squish() removes leading/trailing spaces and collapses
# repeated inner spaces, and str_to_lower() makes " Pen", "PEN " and " pen "
# all "pen". parse_number() drops "$" and "%", so the discount is divided by
# 100 to become a proportion. A missing amount gives a missing net_amount
# (NA * anything is NA), so sales 3 and 7 stay NA and are labelled
# "unrecorded"; the is.na() condition is listed first in case_when() so NA is
# never compared against 20. All 8 rows and all original columns are kept.

# ---- Part B: Validate codes and split strings -------------------------------

sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
print(invalid_codes)

# Output (invalid_codes):
#   sale_id code_raw code_clean
#         5 bad      BAD
#         7 ON-107X  ON-107X
#
# Explanation: the pattern ^(ST|ON)-[0-9]{3}$ is anchored at both ends, so the
# whole code must be ST or ON, a hyphen, and exactly three digits. "BAD" fails
# entirely and "ON-107X" fails because of the trailing X. "on-108" becomes
# valid after str_to_upper(). sales_codes still has all 8 sales.

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  select(sale_id, code_clean) |>
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number"),
                       cols_remove = FALSE) |>
  select(sale_id, code_clean, channel, number)
print(valid_code_parts)

nrow(sales_clean)   # still 8 -- invalid rows were not removed from sales_clean

# Output (valid_code_parts):
#   sale_id code_clean channel number
#         1 ST-101     ST      101
#         2 ON-102     ON      102
#         3 ST-103     ST      103
#         4 ON-104     ON      104
#         6 ST-106     ST      106
#         8 ON-108     ON      108
# nrow(sales_clean) = 8, so no invalid rows were deleted from sales_clean.

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
print(sale_tags)

# Output (sale_tags): 11 rows
#   1 gift, 1 urgent, 2 online, 3 gift, 4 member, 5 online, 5 member,
#   6 gift, 7 member, 8 gift, 8 member
#
# Explanation: separate_wider_delim() splits one column into several new
# COLUMNS (the row count stays the same), while separate_longer_delim() splits
# a value into several ROWS (one row per piece), so sales with two tags appear
# twice.

# ---- Part C: Grouped proportions and factor categories ----------------------

customer_summary <- sales_clean |>
  summarise(
    sales_n     = n(),
    known_n     = sum(!is.na(net_amount)),
    large_n     = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(known_n > 0, large_n / known_n, NA_real_),
    .by = customer_id
  ) |>
  arrange(customer_id)
print(customer_summary)

# Output (customer_summary):
#   customer_id sales_n known_n large_n large_share
#   C1                2       1       0       0
#   C2                3       3       2       0.667
#   C3                1       1       0       0
#   C4                1       1       1       1
#   C5                1       0       0      NA
#
# C1's large_share: the numerator is large_n = 0 (C1 has no known net amount
# of at least 20; its one known sale nets $10). The denominator is known_n = 1,
# not sales_n = 2, because sale 3 has a missing amount and is excluded. So
# large_share = 0 / 1 = 0. C5 has no known amounts, so large_n = 0 and
# large_share = NA (0/0 is undefined, not zero).

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group  = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

levels(sales_factors$item_factor)
count(sales_factors, item_factor)
levels(sales_factors$item_group)
count(sales_factors, item_group)

# Output:
#   levels(item_factor): "pen" "folder" "eraser" "notebook"
#   counts: pen 4, folder 2, eraser 1, notebook 1
#   levels(item_group):  "folder" "pen" "Other"
#   counts: folder 2, pen 4, Other 2
#
# Explanation: fct_infreq() only REORDERS the levels (most frequent first);
# all four categories and their counts are unchanged. fct_lump_n() COMBINES
# categories: it keeps the 2 most frequent (pen, folder) and merges eraser and
# notebook into "Other", so the number of categories drops from 4 to 3 and
# "Other" has count 2. Lumping does not reorder the kept levels, which is why
# item_group stays in alphabetical order (folder, pen) with Other last.

# =============================================================================
# Problem 2: Missing Values and Joins
# =============================================================================

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# ---- Part A: Clean and add missing reports ----------------------------------

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount        = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount) |>
  print()

# Output:
#   form line customer_id round primary_amount backup_amount amount
#   A       1 C1              1             10            NA     10
#   A       2 C1              3           -999            15     15
#   A       3 C2              1             20            25     20
#   B       1 C3              1             NA            NA     NA
#   B       2 C3              2              0            99      0
#   B       3 C4              1             30            NA     30
#
# Explanation: fill() runs within each form, so A line 2 gets C1 and B line 2
# gets C3; IDs never cross from form A to form B. coalesce() takes the first
# non-missing value, so the backup is used only when primary_clean is NA (A2,
# where -999 was turned into NA). A recorded primary value, including 0, is a
# real measurement of the same quantity, so it is kept: replacing B2's 0 with
# the backup 99 would overwrite real data. B1 has neither value, so it stays
# NA.

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3,
           fill = list(recorded_row = FALSE))
print(reports_complete)

cat("Original rows:", sum(reports_complete$recorded_row), "\n")
cat("Added rows:   ", sum(!reports_complete$recorded_row), "\n")
cat("Missing amounts before completion:", sum(is.na(reports_clean$amount)), "\n")
cat("Missing amounts after completion: ", sum(is.na(reports_complete$amount)), "\n")

# Output: 12 rows (4 customers x 3 rounds)
#   C1: r1 10 TRUE,  r2 NA FALSE, r3 15 TRUE
#   C2: r1 20 TRUE,  r2 NA FALSE, r3 NA FALSE
#   C3: r1 NA TRUE,  r2  0 TRUE,  r3 NA FALSE
#   C4: r1 30 TRUE,  r2 NA FALSE, r3 NA FALSE
# Original rows: 6, added rows: 6
# Missing amounts: 1 before completion, 7 after completion
#
# Explicit missing (before completion): C3 round 1 -- the row exists but its
# amount is NA.
# Implicit missing (before completion): C1 round 2 -- there is no row at all
# for that customer-round; complete() makes it visible as an NA row with
# recorded_row = FALSE. recorded_row was set to TRUE before complete() and the
# new rows were filled with FALSE, so the two kinds of NA can be told apart.

# ---- Part B: Join and check -------------------------------------------------

# Duplicate / missing IDs in customers
customers |> count(customer_id) |> filter(n > 1)
sum(is.na(customers$customer_id))

# Repeated customer_id-round combinations in reports_clean
reports_clean |> count(customer_id, round) |> filter(n > 1)

# Output: no duplicated customer_id in customers (0 rows), 0 missing IDs,
# and no customer_id-round combination appears more than once in
# reports_clean (0 rows). So customers can safely be the "one" side of a
# many-to-one join.

customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(customers_marked,
            by = join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") |>
  mutate(customer_found = coalesce(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found) |>
  print()

# Output:
#   customer_id round amount region customer_found
#   C1              1     10 North  TRUE
#   C1              3     15 North  TRUE
#   C2              1     20 South  TRUE
#   C3              1     NA <NA>   TRUE
#   C3              2      0 <NA>   TRUE
#   C4              1     30 <NA>   FALSE

# IDs only in reports_clean
reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

# IDs only in customers
customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

# Output: only in reports_clean: C4. Only in customers: C5.
#
# Explanation: C3 and C4 both have a missing region, but for different
# reasons. C3 has customer_found = TRUE: the customer IS in the metadata table,
# its region just wasn't recorded. C4 has customer_found = FALSE: there is no
# metadata record at all, so the NA region comes from the failed join. A
# missing region alone cannot distinguish these two cases.

# ---- Part C: Check and reshape reports --------------------------------------

# NOTE: customers_bad is not defined in the assignment handout. It is defined
# here as the customers table with a conflicting duplicate record for C2
# (two different regions), which is what the questions below describe.
customers_bad <- tibble(
  customer_id = c("C1", "C2", "C2", "C3", "C5"),
  region      = c("North", "South", "East", NA, "West")
)

reports_duplicated <- reports_clean |>
  left_join(customers_bad, by = join_by(customer_id),
            relationship = "many-to-many")

cat("Rows before:", nrow(reports_clean), " Rows after:", nrow(reports_duplicated), "\n")
cat("Total before:", sum(reports_clean$amount, na.rm = TRUE),
    " Total after:", sum(reports_duplicated$amount, na.rm = TRUE), "\n")

reports_duplicated |>
  count(customer_id, round) |>
  filter(n > 1)

# Output: rows 6 -> 7; total amount (ignoring NA) 75 -> 95.
# Repeated report: customer_id C2, round 1.
#
# Explanation: C2 has two rows in customers_bad (South and East), so its one
# report matches twice and is duplicated. Its amount of 20 is now counted
# twice, which inflates the total by exactly 20 (75 + 20 = 95).
#
# With relationship = "many-to-one", left_join() would stop with an error,
# because C2 matches more than one row in customers_bad -- which is the point
# of the check. Allowing "many-to-many" does NOT fix anything: it only
# silences the error. The conflicting region for C2 is still there and the
# report is double-counted; the fix is to resolve the duplicate in the
# customer table before joining.

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, values_from = amount,
              names_prefix = "round_")
print(reports_wide)

# Check the prediction
reports_wide |>
  pivot_longer(starts_with("round_"), names_to = "round",
               values_to = "amount", values_drop_na = TRUE) |>
  nrow()

# Output (reports_wide):
#   customer_id round_1 round_2 round_3
#   C1               10      NA      15
#   C2               20      NA      NA
#   C3               NA       0      NA
#   C4               30      NA      NA
#
# Prediction: 12 cells - 7 NA cells = 5 rows would remain after pivoting
# back to long form with values_drop_na = TRUE (confirmed above: 5).
#
# Explanation: dropping NAs simply removes those rows; it does not bring back
# a value. C3 round 1 was a real report whose amount is unknown, and that
# information is lost too -- it is dropped just like the implicit gaps. The
# zero for C3 round 2 is kept because 0 is a value, not NA.

# =============================================================================
# Problem 3: Two-sample t-tests and simulation
# =============================================================================

# ---- Part A: Write a t-test function ----------------------------------------

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  n_x <- length(x);  n_y <- length(y)
  xbar <- mean(x);   ybar <- mean(y)
  s2_x <- var(x);    s2_y <- var(y)

  if (equal_var) {
    method <- "Pooled"
    s2_p <- ((n_x - 1) * s2_x + (n_y - 1) * s2_y) / (n_x + n_y - 2)
    se   <- sqrt(s2_p * (1 / n_x + 1 / n_y))
    df   <- n_x + n_y - 2
  } else {
    method <- "Welch"
    v_x <- s2_x / n_x
    v_y <- s2_y / n_y
    se  <- sqrt(v_x + v_y)
    df  <- (v_x + v_y)^2 / (v_x^2 / (n_x - 1) + v_y^2 / (n_y - 1))
  }

  t_stat  <- (xbar - ybar) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)

  list(method  = method,
       t_stat  = t_stat,
       df      = df,
       p_value = p_value,
       reject  = p_value < alpha)
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

res_welch  <- two_sample_t(x_check, y_check)
res_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)
str(res_welch)
str(res_pooled)

# Reference calculations; compare with your own function.
ref_welch  <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)

verify <- tibble(
  method   = c("Welch", "Pooled"),
  t_diff   = abs(c(res_welch$t_stat  - ref_welch$statistic,
                   res_pooled$t_stat - ref_pooled$statistic)),
  df_diff  = abs(c(res_welch$df      - ref_welch$parameter,
                   res_pooled$df     - ref_pooled$parameter)),
  p_diff   = abs(c(res_welch$p_value - ref_welch$p.value,
                   res_pooled$p_value - ref_pooled$p.value))
) |>
  mutate(all_below_1e8 = t_diff < 1e-8 & df_diff < 1e-8 & p_diff < 1e-8)
print(verify)

# Output:
#   Welch:  t_stat = 3.4,  df = 7.68 (unrounded), p_value = 0.00991, reject = TRUE
#   Pooled: t_stat = 3.53, df = 11,               p_value = 0.00474, reject = TRUE
#
# Verification against t.test():
#   method   t_diff    df_diff   p_diff    all_below_1e8
#   Welch    4.44e-16  2.66e-15  5.20e-18  TRUE
#   Pooled   0         0         0         TRUE
# All differences in t_stat, df, and p_value are smaller than 1e-8 (they are
# only floating-point rounding), so the function matches t.test() for both
# methods. Both tests reject H0 at alpha = 0.05.

# ---- Part B: Simulation loop ------------------------------------------------

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Pre-allocate storage: 4 scenarios x B replicates x 2 methods
n_rows  <- nrow(scenarios) * B * 2
sim_scenario  <- character(n_rows)
sim_replicate <- integer(n_rows)
sim_method    <- character(n_rows)
sim_t         <- numeric(n_rows)
sim_df        <- numeric(n_rows)
sim_p         <- numeric(n_rows)
sim_reject    <- logical(n_rows)

# Run once immediately before the simulation, not inside a loop.
set.seed(2026)

row <- 0
for (s in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])

    for (eq in c(FALSE, TRUE)) {
      res <- two_sample_t(x, y, equal_var = eq, alpha = alpha)
      row <- row + 1
      sim_scenario[row]  <- scenarios$scenario[s]
      sim_replicate[row] <- b
      sim_method[row]    <- res$method
      sim_t[row]         <- res$t_stat
      sim_df[row]        <- res$df
      sim_p[row]         <- res$p_value
      sim_reject[row]    <- res$reject
    }
  }
}

simulation_results <- data.frame(
  scenario  = sim_scenario,
  replicate = sim_replicate,
  method    = sim_method,
  t_stat    = sim_t,
  df        = sim_df,
  p_value   = sim_p,
  reject    = sim_reject
)

head(simulation_results)
nrow(simulation_results)

# Output (first six rows):
#     scenario replicate method     t_stat       df    p_value reject
#   equal_null         1  Welch -1.6408015 36.27031 0.10948361  FALSE
#   equal_null         1 Pooled -1.6719503 58.00000 0.09992119  FALSE
#   equal_null         2  Welch  0.4498986 43.85631 0.65499951  FALSE
#   equal_null         2 Pooled  0.4268900 58.00000 0.67103998  FALSE
#   equal_null         3  Welch  1.5837255 30.84146 0.12345809  FALSE
#   equal_null         3 Pooled  1.7224891 58.00000 0.09030984  FALSE
# Total rows: 8000 (4 scenarios x 1000 replicates x 2 methods)

simulation_summary <- simulation_results |>
  summarise(
    mean_t         = mean(t_stat),
    sd_t           = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  mutate(across(c(mean_t, sd_t, rejection_rate), \(v) round(v, 3)))
print(simulation_summary)

# Output (simulation_summary):
#        scenario method mean_t  sd_t rejection_rate
#      equal_null  Welch -0.019 1.038          0.044
#      equal_null Pooled -0.018 1.024          0.038
#    unequal_null  Welch  0.036 1.075          0.061
#    unequal_null Pooled  0.048 1.402          0.139
#     equal_shift  Welch -3.768 1.116          0.956
#     equal_shift Pooled -3.757 1.078          0.957
#   unequal_shift  Welch -1.542 1.048          0.282
#   unequal_shift Pooled -2.019 1.357          0.497

# Rejection rates by scenario and method (dashed line = alpha)
rate_mat <- simulation_summary |>
  select(scenario, method, rejection_rate) |>
  pivot_wider(names_from = scenario, values_from = rejection_rate)
barplot(as.matrix(rate_mat[, -1]), beside = TRUE,
        legend.text = rate_mat$method, ylim = c(0, 1),
        ylab = "Rejection rate", main = "Rejection rate by scenario and method")
abline(h = alpha, lty = 2)

# Short report:
# Under equal variances both null rejection rates (Welch 0.044, Pooled 0.038)
# are close to 0.05 given simulation error (about +/- 0.014 for B = 1000), but
# with unequal variances and the smaller sample having the larger SD, the
# pooled test's Type I error inflates to 0.139 while Welch stays near 0.05
# (0.061). When the means differ, power is nearly identical with equal
# variances (about 0.96 for both), but in the unequal_shift scenario the
# pooled test appears more powerful (0.497 vs 0.282). That extra "power" is
# not trustworthy because the same test rejects a true null almost three
# times as often as it should, so a test must first hold its Type I error
# rate near alpha before its power can be compared fairly -- making Welch the
# safer choice here.
