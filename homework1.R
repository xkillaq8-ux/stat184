# =============================================================================
# Homework 1
# Base R only
# =============================================================================


# =============================================================================
# Problem 1: Objects, data structures, and subsetting
# =============================================================================

# ---- Setup code ----
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section    <- c("A", "B", "A", "B", "A", "B")
quiz1      <- c(82, 91, 76, 88, 95, 69)
quiz2      <- c(85, 89, 80, 92, 94, 74)
passed     <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# ---- Part A: Building data structures ----

# Convert section to a factor with levels in the order "A", "B"
section <- factor(section, levels = c("A", "B"))

# Data frame containing all five vectors
students <- data.frame(student_id, section, quiz1, quiz2, passed)

# Matrix: students as rows, quizzes as columns, IDs as row names
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

# List with course name, the students data frame, and a named cutoff vector
course_record <- list(
  course  = "R Programming",
  scores  = students,
  cutoffs = c(pass = 70, excellent = 90)
)

# Inspect the objects
typeof(section); class(section); length(section); str(section)
typeof(students); class(students); length(students); dim(students); str(students)
typeof(score_matrix); class(score_matrix); length(score_matrix); dim(score_matrix); str(score_matrix)
typeof(course_record); class(course_record); length(course_record); str(course_record)

# Comparison (<= 4 sentences):
# A vector holds elements of a single type in one dimension, while a list is
# also one-dimensional but can hold elements of different types and lengths
# (even other lists or data frames). A matrix is a vector with a dim attribute
# (rows x columns) that must hold a single type, whereas a factor is an integer
# vector with labeled levels used for categorical data. A data frame is a list
# of equal-length columns, so it looks like a table but each column can have a
# different type.


# ---- Part B: Subsetting ----

# S04's second quiz score
score_matrix["S04", "quiz2"]

# First two rows, preserving matrix dimensions (drop = FALSE keeps it a matrix)
score_matrix[1:2, , drop = FALSE]

# Extract 'course' from course_record three ways
course_record["course"]     # [ ]  -> returns a list of length 1 containing course
course_record[["course"]]   # [[ ]] -> returns the element itself (character string)
course_record$course       # $    -> returns the element itself (by name)

# Difference between [ , [[ and $:
# '[' returns a subset of the same type as the original object (a sub-list
# from a list, a sub-matrix from a matrix), so it can select several elements.
# '[[' extracts exactly one element and drops the container, returning the
# element itself. '$' is shorthand for '[[' by name, and it works on lists and
# data frames only (with partial matching), which makes it convenient to type.


# ---- Part C: Vectorized calculations (no loops) ----

# Row-wise mean of the two quizzes
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

# TRUE when average >= 90
students$excellent <- students$average >= 90

# Section A students with average at least 80
sectionA_80 <- students[students$section == "A" & students$average >= 80, ]

# Return only student_id, section, and average for that subset
sectionA_80[, c("student_id", "section", "average")]

# Named numeric vector of all averages, using student IDs as names
avg_vec <- setNames(students$average, students$student_id)
avg_vec

students

# Why these calculations are vectorized:
# rowMeans(), the comparison >=, the logical & and the indexing all operate
# on whole vectors/columns at once, so R applies the operation to every
# element internally without an explicit for/while loop.


# =============================================================================
# Problem 2: Importing, cleaning, and summarizing data
# =============================================================================

# ---- Setup code ----
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

# ---- Part A: Importing data ----

# Treat both blank fields and the text "NA" as missing
measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))

# Inspect
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# Count missing values in each column
colSums(is.na(measurements))

# Keep only rows with no missing values
measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete

# Sample IDs removed by the complete-case filter
removed_ids <- measurements$sample_id[!complete.cases(measurements)]
removed_ids   # "M02" "M03" "M06"

# Why x == NA is not a valid missing-value test:
# NA means "unknown", so any comparison with an unknown value is also unknown.
# x == NA therefore returns NA (not TRUE or FALSE) for every element, even
# when x itself is NA, so it can't be used to detect missing values.
# Use is.na(x) instead, which correctly returns TRUE/FALSE.
c(1, NA, 3) == NA
is.na(c(1, NA, 3))


# ---- Part B: Transform, filter, and summarize ----

# Convert site and status to factors and report their levels
measurements$site   <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

# Fahrenheit temperature
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

# pH below 7, preserving missing pH values (NA < 7 stays NA)
measurements$ph_below_7 <- measurements$ph < 7

# Complete observations from North or South whose status is "ok",
# returning only the requested columns
selected <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")
]
selected

# Overall mean Celsius temperature and the South-site mean (ignoring NA)
mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)


# ---- Part C: Matrix operations ----

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise <- A * B      # element-wise (Hadamard) product
matmul      <- A %*% B    # matrix multiplication

elementwise
matmul
dim(elementwise)   # 2 2
dim(matmul)        # 2 2

# Explanation:
# A * B multiplies matching entries (A[i, j] * B[i, j]) and requires the
# matrices to have the same dimensions. A %*% B is true matrix multiplication:
# each entry is the dot product of a row of A with a column of B, and it
# requires ncol(A) == nrow(B). Both results are 2 x 2 here, but the values
# differ (e.g., element [1,1] is 1*5 = 5 for *, but 1*5 + 3*6 = 23 for %*%).


# =============================================================================
# Problem 3: Functions
# =============================================================================

# ---- Setup code ----
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

# ---- Part A: Write a grading function ----

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    NA_character_
  } else if (score >= a_min) {
    "A"
  } else if (score >= b_min) {
    "B"
  } else if (score >= c_min) {
    "C"
  } else if (score >= d_min) {
    "D"
  } else {
    "F"
  }
}

# Tests
grade_one(NA)   # NA
grade_one(90)   # "A"  (90 meets a_min because the comparison is >=)
grade_one(80)   # "B"
grade_one(85)   # "B"
grade_one(74)   # "C"


# ---- Part B: Applying functions with a loop ----

grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades

# In the loop, i is the current position (index) in scores, running from 1 to
# length(scores), so scores[i] is the i-th score and grades[i] is where its
# grade is stored.


# ---- Part C-1: Function behavior ----

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    n       = length(x),
    missing = sum(is.na(x)),
    mean    = round(mean(x, na.rm = na.rm), digits),
    sd      = round(sd(x, na.rm = na.rm), digits),
    min     = round(min(x, na.rm = na.rm), digits),
    max     = round(max(x, na.rm = na.rm), digits)
  )
}

# Once with defaults
summarize_scores(scores)

# Once with named arguments
summarize_scores(x = scores, na.rm = TRUE, digits = 2)

# Prediction: total count = 8, missing = 1, and the mean/sd/min/max are
# computed from the 7 non-missing scores. With digits = 2 the mean and sd
# show two decimals (79.43 and 13.28), and min/max are 59 and 95.


# ---- Part C-2: Function behavior ----

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19,
            xlab = "Position", ylab = "Score", main = "Student Scores")

# Prediction/explanation: the plot shows scores (y) against their position
# 1-8 (x) with points joined by lines (type = "b") and solid circles
# (pch = 19). The NA at position 3 is not drawn, so there is a gap in the line
# there. The '...' passes the extra arguments (type, pch, labels, title)
# straight through to plot(), so plot_scores() doesn't need to list them.
