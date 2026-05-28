# ============================================================
#  KVASIR PORTED-TEST RATIO ANALYSIS
#
#  Purpose:
#    Analyze every branch where at least one ported test executed.
#    This is a distance-from-passing view, not a strict behavioural
#    equivalence verdict analysis.
#
#  Cohort D:
#    porting.execution.tests_executed > 0
#
#  Main outcomes:
#    passed      = executed - failed - errors - skipped
#    pass_rate   = passed / executed
#    fail_rate   = failed / executed
#    error_rate  = errors / executed
#    skip_rate   = skipped / executed
#    active_pass = passed / (executed - skipped)
# ============================================================

find_input <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
  } else {
    getwd()
  }
  
  candidates <- c(
    file.path(script_dir, "..", "data", "exported", "kvasir_stats.csv"),
    file.path(getwd(), "replication-package", "data", "exported", "kvasir_stats.csv"),
    "/Users/oleremidahl/Documents/Master/replication-package/data/exported/kvasir_stats.csv"
  )
  candidates <- normalizePath(candidates, mustWork = FALSE)
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop("Could not find kvasir_stats.csv. Tried:\n",
         paste(candidates, collapse = "\n"))
  }
  hit[1]
}

rate_summary <- function(x) {
  c(mean = round(mean(x), 3),
    median = round(median(x), 3),
    q25 = round(unname(quantile(x, 0.25)), 3),
    q75 = round(unname(quantile(x, 0.75)), 3),
    n = length(x))
}

count_rate_summary <- function(x) {
  c(k = sum(x), n = length(x), p = round(mean(x), 3))
}

print_test <- function(expr) {
  print(tryCatch(expr, error = function(e) e))
}

df <- read.csv(find_input(), stringsAsFactors = FALSE)

df$variant <- factor(df$variant, levels = c("generated", "v2", "v3"))
df$agent <- factor(df$agent, levels = c("claude", "codex"))

df$verdict <- df$result.verdict
df$base_orig_ok <- df$baselines.original.status == "pass"
df$tests_exec <- df$porting.execution.tests_executed
df$tests_failed <- df$porting.execution.tests_failed
df$tests_errors <- df$porting.execution.tests_errors
df$tests_skipped <- df$porting.execution.tests_skipped

df$tests_passed <- df$tests_exec - df$tests_failed -
  df$tests_errors - df$tests_skipped
df$tests_active <- df$tests_exec - df$tests_skipped

df$pass_rate <- ifelse(df$tests_exec > 0, df$tests_passed / df$tests_exec, NA)
df$fail_rate <- ifelse(df$tests_exec > 0, df$tests_failed / df$tests_exec, NA)
df$error_rate <- ifelse(df$tests_exec > 0, df$tests_errors / df$tests_exec, NA)
df$skip_rate <- ifelse(df$tests_exec > 0, df$tests_skipped / df$tests_exec, NA)
df$nonpass_rate <- ifelse(df$tests_exec > 0,
                          (df$tests_failed + df$tests_errors +
                             df$tests_skipped) / df$tests_exec, NA)
df$active_pass_rate <- ifelse(df$tests_active > 0,
                              df$tests_passed / df$tests_active, NA)

df$strict_decisive_comparable <- df$base_orig_ok &
  !(df$verdict %in% c("skipped", "no_test_signal")) &
  df$verdict %in% c("no_difference_detected", "difference_detected")

d <- df[df$tests_exec > 0, ]
d$variant <- droplevels(d$variant)
d$agent <- droplevels(d$agent)
d$evidence_group <- ifelse(d$strict_decisive_comparable,
                           "strict-decisive-comparable",
                           "executed-noncomparable")
d$evidence_group <- factor(d$evidence_group,
                           levels = c("strict-decisive-comparable",
                                      "executed-noncomparable"))

cat("\n===== KVASIR PORTED-TEST RATIO ANALYSIS =====\n")
cat("Input:", find_input(), "\n")
cat("All rows:", nrow(df), "\n")
cat("Executed rows (Cohort D):", nrow(d), "\n")

cat("\nExecuted rows by agent x variant:\n")
print(table(d$agent, d$variant))

cat("\nExecuted rows by evidence group:\n")
print(table(d$evidence_group))

cat("\nExecuted rows by verdict:\n")
print(table(d$verdict, useNA = "ifany"))

cat("\nExecuted rows by verdict x original baseline passed:\n")
print(addmargins(table(d$verdict, d$base_orig_ok, useNA = "ifany")))

if (any(d$tests_passed < 0, na.rm = TRUE)) {
  cat("\nWARNING: at least one row has negative derived passed tests.\n")
}

# ============================================================
#  BLOCK 1 - POOLED TEST-CASE COMPOSITION
# ============================================================
cat("\n\n========================================================\n")
cat("  BLOCK 1: pooled test-case composition\n")
cat("========================================================\n")
cat("Pooled percentages are descriptive. They count individual test cases,\n")
cat("so repositories with more executed tests contribute more weight.\n")

pooled <- aggregate(cbind(tests_exec, tests_passed, tests_failed,
                          tests_errors, tests_skipped) ~ agent + variant,
                    data = d, FUN = sum)
pooled_n <- aggregate(repo_slug ~ agent + variant, data = d, FUN = length)
names(pooled_n)[3] <- "n_branches"
pooled <- merge(pooled_n, pooled, by = c("agent", "variant"))
pooled$pass_pct <- round(100 * pooled$tests_passed / pooled$tests_exec, 1)
pooled$fail_pct <- round(100 * pooled$tests_failed / pooled$tests_exec, 1)
pooled$error_pct <- round(100 * pooled$tests_errors / pooled$tests_exec, 1)
pooled$skip_pct <- round(100 * pooled$tests_skipped / pooled$tests_exec, 1)
print(pooled[order(pooled$agent, pooled$variant), ], row.names = FALSE)

cat("\nPooled active (non-skipped) composition:\n")
pooled_active <- aggregate(cbind(tests_active, tests_passed, tests_failed,
                                 tests_errors) ~ agent + variant,
                           data = d, FUN = sum)
pooled_active_n <- aggregate(repo_slug ~ agent + variant, data = d, FUN = length)
names(pooled_active_n)[3] <- "n_branches"
pooled_active <- merge(pooled_active_n, pooled_active,
                       by = c("agent", "variant"))
pooled_active$pass_pct <- round(100 * pooled_active$tests_passed /
                                  pooled_active$tests_active, 1)
pooled_active$fail_pct <- round(100 * pooled_active$tests_failed /
                                  pooled_active$tests_active, 1)
pooled_active$error_pct <- round(100 * pooled_active$tests_errors /
                                   pooled_active$tests_active, 1)
print(pooled_active[order(pooled_active$agent, pooled_active$variant), ],
      row.names = FALSE)

cat("\nPooled composition by evidence group x verdict:\n")
pooled_verdict <- aggregate(cbind(tests_exec, tests_passed, tests_failed,
                                  tests_errors, tests_skipped) ~
                              evidence_group + verdict,
                            data = d, FUN = sum)
pooled_verdict_n <- aggregate(repo_slug ~ evidence_group + verdict,
                              data = d, FUN = length)
names(pooled_verdict_n)[3] <- "n_branches"
pooled_verdict <- merge(pooled_verdict_n, pooled_verdict,
                        by = c("evidence_group", "verdict"))
pooled_verdict$pass_pct <- round(100 * pooled_verdict$tests_passed /
                                   pooled_verdict$tests_exec, 1)
pooled_verdict$fail_pct <- round(100 * pooled_verdict$tests_failed /
                                   pooled_verdict$tests_exec, 1)
pooled_verdict$error_pct <- round(100 * pooled_verdict$tests_errors /
                                    pooled_verdict$tests_exec, 1)
pooled_verdict$skip_pct <- round(100 * pooled_verdict$tests_skipped /
                                   pooled_verdict$tests_exec, 1)
print(pooled_verdict[order(pooled_verdict$evidence_group,
                           pooled_verdict$verdict), ],
      row.names = FALSE)

# ============================================================
#  BLOCK 2 - BRANCH-LEVEL RATIO SUMMARIES
# ============================================================
cat("\n\n========================================================\n")
cat("  BLOCK 2: branch-level ratio summaries\n")
cat("========================================================\n")
cat("Branch-level ratios use each reconstruction branch as one observation.\n")

cat("\nBy agent x variant:\n")
print(aggregate(cbind(tests_exec, pass_rate, fail_rate, error_rate,
                      skip_rate, nonpass_rate) ~
                  agent + variant,
                data = d, FUN = rate_summary))

cat("\nActive pass rate by agent x variant (excludes rows with no active tests):\n")
d_active <- d[!is.na(d$active_pass_rate), ]
print(aggregate(cbind(tests_active, active_pass_rate) ~ agent + variant,
                data = d_active, FUN = rate_summary))

cat("\nBy evidence group x verdict:\n")
print(aggregate(cbind(tests_exec, pass_rate, fail_rate, error_rate,
                      skip_rate, nonpass_rate) ~
                  evidence_group + verdict,
                data = d, FUN = rate_summary))

cat("\nActive pass rate by evidence group x verdict (excludes rows with no active tests):\n")
print(aggregate(cbind(tests_active, active_pass_rate) ~ evidence_group + verdict,
                data = d_active, FUN = rate_summary))

cat("\nBy evidence group x agent x variant:\n")
print(aggregate(cbind(tests_exec, pass_rate, fail_rate, error_rate,
                      skip_rate, nonpass_rate) ~
                  evidence_group + agent + variant,
                data = d, FUN = rate_summary))

cat("\nActive pass rate by evidence group x agent x variant (excludes rows with no active tests):\n")
print(aggregate(cbind(tests_active, active_pass_rate) ~
                  evidence_group + agent + variant,
                data = d_active, FUN = rate_summary))

# ============================================================
#  BLOCK 3 - NEAR-PASS THRESHOLDS
# ============================================================
cat("\n\n========================================================\n")
cat("  BLOCK 3: near-pass thresholds\n")
cat("========================================================\n")
d$pass_ge_50 <- as.integer(d$pass_rate >= 0.50)
d$pass_ge_75 <- as.integer(d$pass_rate >= 0.75)
d$pass_ge_90 <- as.integer(d$pass_rate >= 0.90)
d$pass_eq_100 <- as.integer(d$pass_rate == 1.00)

cat("\nThreshold counts by agent x variant:\n")
print(aggregate(cbind(pass_ge_50, pass_ge_75, pass_ge_90, pass_eq_100) ~
                  agent + variant,
                data = d, FUN = count_rate_summary))

cat("\nThreshold counts by evidence group x agent x variant:\n")
print(aggregate(cbind(pass_ge_50, pass_ge_75, pass_ge_90, pass_eq_100) ~
                  evidence_group + agent + variant,
                data = d, FUN = count_rate_summary))

cat("\nPass-rate bins by agent x variant:\n")
d$pass_rate_bin <- cut(d$pass_rate,
                       breaks = c(-Inf, 0.50, 0.75, 0.90, 1.00, Inf),
                       labels = c("<50%", "50-75%", "75-90%",
                                  "90-100%", "100%"),
                       right = FALSE)
print(ftable(d$agent, d$variant, d$pass_rate_bin))

# ============================================================
#  BLOCK 4 - INFERENCE ON ALL EXECUTED BRANCHES
# ============================================================
cat("\n\n========================================================\n")
cat("  BLOCK 4: branch-level inference on all executed branches\n")
cat("========================================================\n")
cat("These tests use branch-level ratios. Pooled test-case counts above are\n")
cat("descriptive only and are not treated as independent test observations.\n")

outcomes <- list(
  pass_rate = "passed / executed",
  fail_rate = "failed / executed",
  error_rate = "errors / executed",
  nonpass_rate = "(failed + errors + skipped) / executed",
  active_pass_rate = "passed / (executed - skipped)"
)

for (o in names(outcomes)) {
  dd <- d[!is.na(d[[o]]), ]
  y <- dd[[o]]
  cat("\n\n--- Outcome:", o, "-", outcomes[[o]], "---\n")
  cat("\nANOVA: y ~ variant * agent\n")
  print(anova(lm(y ~ dd$variant * dd$agent)))
  cat("ANOVA: y ~ agent\n")
  print(anova(lm(y ~ dd$agent)))
  cat("Kruskal-Wallis: y ~ variant\n")
  print(kruskal.test(y ~ dd$variant))
  
  cat("\nVariant contrasts within agent:\n")
  for (ag in levels(dd$agent)) {
    cat("  agent =", ag, "\n")
    sub <- dd[dd$agent == ag, ]
    for (pair in list(c("generated", "v2"),
                      c("generated", "v3"),
                      c("v2", "v3"))) {
      s <- sub[sub$variant %in% pair, ]
      cat("   ", pair[1], "vs", pair[2], "\n")
      print_test(wilcox.test(s[[o]] ~ droplevels(s$variant), exact = FALSE))
    }
  }
  
  cat("\nClaude vs Codex within variant:\n")
  for (v in levels(dd$variant)) {
    sub <- dd[dd$variant == v, ]
    cat("  variant =", v, "\n")
    print_test(wilcox.test(sub[[o]] ~ droplevels(sub$agent), exact = FALSE))
  }
}

# ============================================================
#  BLOCK 5 - CLEAN TO V3 WITHIN-REPO CHANGE
# ============================================================
cat("\n\n========================================================\n")
cat("  BLOCK 5: clean-to-v3 within-repo change\n")
cat("========================================================\n")
cat("For pass-like outcomes, drop = generated - v3.\n")
cat("For failure-like outcomes, increase = v3 - generated.\n")

wide_change <- function(data, outcome, direction = c("drop", "increase")) {
  direction <- match.arg(direction)
  w <- reshape(data[, c("repo_slug", "agent", "variant", outcome)],
               idvar = c("repo_slug", "agent"),
               timevar = "variant",
               direction = "wide")
  names(w) <- sub(paste0("^", outcome, "\\."), "", names(w))
  if (!all(c("generated", "v3") %in% names(w))) return(NULL)
  ok <- complete.cases(w[, c("generated", "v3")])
  w <- w[ok, ]
  if (direction == "drop") {
    w$change <- w$generated - w$v3
  } else {
    w$change <- w$v3 - w$generated
  }
  w
}

change_specs <- list(
  pass_rate = "drop",
  active_pass_rate = "drop",
  fail_rate = "increase",
  error_rate = "increase",
  nonpass_rate = "increase"
)

for (o in names(change_specs)) {
  cat("\n# Outcome:", o, "(", change_specs[[o]], ")\n")
  w <- wide_change(d, o, change_specs[[o]])
  if (is.null(w) || nrow(w) == 0) next
  print(aggregate(change ~ agent, data = w,
                  FUN = function(x) c(mean = round(mean(x), 3),
                                      median = round(median(x), 3),
                                      q25 = round(unname(quantile(x, 0.25)), 3),
                                      q75 = round(unname(quantile(x, 0.75)), 3),
                                      n = length(x))))
  print_test(wilcox.test(change ~ agent, data = w, exact = FALSE))
}

cat("\n===== DONE =====\n")
