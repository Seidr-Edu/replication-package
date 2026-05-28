# ============================================================
#  KVASIR FUNCTIONAL-BEHAVIOUR ANALYSIS
#
#  Cohort (analysis cohort):
#    porting.execution.tests_executed > 0
#    AND diagnostics.write_scope.violation_count == 0
#
#  Branch-level outcomes:
#    pass_rate         = passed  / executed
#    fail_rate         = failed  / executed
#    error_rate        = errors  / executed
#    skip_rate         = skipped / executed
#    nonpass_rate      = (failed + errors + skipped) / executed
#    active_pass_rate  = passed  / (executed - skipped)
#
#  Derived artifacts (written to data/derived/kvasir/):
#    kvasir_funnel.csv                   -- cohort assembly (transparency)
#    kvasir_retention_descriptive.csv    -- retention as validity indicator
#    kvasir_pooled_composition.csv       (T1)
#    kvasir_pooled_composition.pdf       (F1)
#    kvasir_branch_rates.csv             (T2)
#    kvasir_pass_rate_boxplot.pdf        (F2)
#    kvasir_threshold_shares.csv         (T3)
#    kvasir_clean_to_degraded_change.csv -- clean->v2 and clean->v3
# ============================================================

# ---- locate input and output ----------------------------------
script_dir <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) {
    dirname(normalizePath(sub("^--file=", "", f[1]), mustWork = FALSE))
  } else {
    getwd()
  }
})()

find_path <- function(candidates, label, must_exist = TRUE) {
  candidates <- normalizePath(candidates, mustWork = FALSE)
  hit <- candidates[file.exists(candidates) | dir.exists(candidates)]
  if (length(hit) == 0) {
    if (must_exist) stop("Could not find ", label, ". Tried:\n",
                         paste(candidates, collapse = "\n"))
    return(candidates[1])
  }
  hit[1]
}

input_csv <- find_path(c(
  file.path(script_dir, "..", "data", "exported", "kvasir_stats.csv"),
  file.path(script_dir, "..", "replication-package", "data", "exported", "kvasir_stats.csv"),
  file.path(getwd(), "replication-package", "data", "exported", "kvasir_stats.csv"),
  "/Users/oleremidahl/Documents/Master/replication-package/data/exported/kvasir_stats.csv"
), "kvasir_stats.csv")

derived_dir <- find_path(c(
  file.path(script_dir, "..", "data", "derived", "kvasir"),
  file.path(script_dir, "..", "replication-package", "data", "derived", "kvasir"),
  file.path(getwd(), "replication-package", "data", "derived", "kvasir"),
  "/Users/oleremidahl/Documents/Master/replication-package/data/derived/kvasir"
), "data/derived/kvasir/", must_exist = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

cat("Input  :", input_csv,   "\n")
cat("Output :", derived_dir, "\n\n")

# ---- helpers --------------------------------------------------
rate_summary <- function(x) {
  c(mean   = round(mean(x), 3),
    median = round(median(x), 3),
    q25    = round(unname(quantile(x, 0.25)), 3),
    q75    = round(unname(quantile(x, 0.75)), 3),
    n      = length(x))
}

descriptive_summary <- function(x) {
  c(mean   = round(mean(x), 3),
    median = round(median(x), 3),
    min    = round(min(x), 3),
    max    = round(max(x), 3),
    n      = length(x))
}

print_test <- function(expr) {
  print(tryCatch(expr, error = function(e) e))
}

flatten_aggregate <- function(agg, value_cols) {
  out <- agg[, !(names(agg) %in% value_cols), drop = FALSE]
  for (v in value_cols) {
    m <- agg[[v]]
    if (is.matrix(m)) {
      for (col in colnames(m)) out[[paste(v, col, sep = ".")]] <- m[, col]
    } else {
      out[[v]] <- m
    }
  }
  out
}

# ---- load and derive ------------------------------------------
df <- read.csv(input_csv, stringsAsFactors = FALSE)

df$variant <- factor(df$variant, levels = c("generated", "v2", "v3"))
df$agent   <- factor(df$agent,   levels = c("claude", "codex"))

df$tests_exec    <- df$porting.execution.tests_executed
df$tests_failed  <- df$porting.execution.tests_failed
df$tests_errors  <- df$porting.execution.tests_errors
df$tests_skipped <- df$porting.execution.tests_skipped
df$ws_viol       <- df$diagnostics.write_scope.violation_count
df$ret_ratio     <- df$evidence.retention.retention_ratio

df$tests_passed  <- df$tests_exec - df$tests_failed -
                    df$tests_errors - df$tests_skipped
df$tests_active  <- df$tests_exec - df$tests_skipped

df$pass_rate        <- ifelse(df$tests_exec > 0,
                              df$tests_passed / df$tests_exec, NA)
df$fail_rate        <- ifelse(df$tests_exec > 0,
                              df$tests_failed / df$tests_exec, NA)
df$error_rate       <- ifelse(df$tests_exec > 0,
                              df$tests_errors / df$tests_exec, NA)
df$skip_rate        <- ifelse(df$tests_exec > 0,
                              df$tests_skipped / df$tests_exec, NA)
df$nonpass_rate     <- ifelse(df$tests_exec > 0,
                              (df$tests_failed + df$tests_errors +
                                 df$tests_skipped) / df$tests_exec, NA)
df$active_pass_rate <- ifelse(df$tests_active > 0,
                              df$tests_passed / df$tests_active, NA)

# ---- cohort filter --------------------------------------------
df$exclude_reason <- with(df, ifelse(
  tests_exec == 0,                       "zero_signal",
  ifelse(tests_exec > 0 & ws_viol > 0,   "ws_viol_executed",
                                         "analysis_cohort")))

d <- df[df$exclude_reason == "analysis_cohort", ]
d$variant <- droplevels(d$variant)
d$agent   <- droplevels(d$agent)

cat("===== FUNNEL =====\n")
cat("Total Kvasir reports:                          ", nrow(df), "\n")
cat("Excluded — no executable test signal:          ",
    sum(df$exclude_reason == "zero_signal"), "\n")
cat("Excluded — write-scope violation (executed):   ",
    sum(df$exclude_reason == "ws_viol_executed"), "\n")
cat("Analysis cohort:                               ", nrow(d), "\n")
cat("\nCohort by agent x variant:\n")
print(table(d$agent, d$variant))

if (any(d$tests_passed < 0, na.rm = TRUE)) {
  cat("\nWARNING: at least one row has negative derived passed tests.\n")
}

# ============================================================
#  FUNNEL — cohort assembly by agent x variant (transparency)
# ============================================================
cat("\n\n========================================================\n")
cat("  Cohort assembly by agent x variant\n")
cat("========================================================\n")

funnel_tab <- as.data.frame.matrix(
  table(interaction(df$agent, df$variant, drop = TRUE),
        df$exclude_reason))
funnel_tab$cell    <- rownames(funnel_tab)
funnel_tab$agent   <- sub("\\..*$", "", funnel_tab$cell)
funnel_tab$variant <- sub("^[^.]*\\.", "", funnel_tab$cell)
funnel_tab$reports <- funnel_tab$analysis_cohort +
                      funnel_tab$ws_viol_executed +
                      funnel_tab$zero_signal
funnel <- funnel_tab[, c("agent", "variant", "reports",
                         "zero_signal", "ws_viol_executed",
                         "analysis_cohort")]
funnel$agent   <- as.character(funnel$agent)
funnel$variant <- as.character(funnel$variant)
funnel <- funnel[order(factor(funnel$agent, levels = c("claude", "codex")),
                       factor(funnel$variant, levels = c("generated", "v2", "v3"))), ]
funnel_variant <- aggregate(cbind(reports, zero_signal, ws_viol_executed,
                                  analysis_cohort) ~ variant,
                            data = funnel, FUN = sum)
funnel_variant$agent <- "All"
funnel_variant <- funnel_variant[, names(funnel)]
funnel_variant <- funnel_variant[order(factor(funnel_variant$variant,
                                             levels = c("generated", "v2", "v3"))), ]
funnel_total <- data.frame(agent = "Total", variant = "All",
                           reports = sum(funnel$reports),
                           zero_signal = sum(funnel$zero_signal),
                           ws_viol_executed = sum(funnel$ws_viol_executed),
                           analysis_cohort = sum(funnel$analysis_cohort))
funnel <- rbind(funnel, funnel_variant, funnel_total)
print(funnel, row.names = FALSE)

write.csv(funnel,
          file.path(derived_dir, "kvasir_funnel.csv"),
          row.names = FALSE)
cat("\nSaved: kvasir_funnel.csv\n")

# ============================================================
#  RETENTION — descriptive validity indicator
# ============================================================
cat("\n\n========================================================\n")
cat("  Retention ratio (descriptive only)\n")
cat("========================================================\n")

ret_desc <- aggregate(ret_ratio ~ agent + variant,
                      data = d[!is.na(d$ret_ratio), ],
                      FUN = descriptive_summary)
ret_desc <- flatten_aggregate(ret_desc, "ret_ratio")
ret_desc <- ret_desc[order(ret_desc$agent, ret_desc$variant), ]
print(ret_desc, row.names = FALSE)

write.csv(ret_desc,
          file.path(derived_dir, "kvasir_retention_descriptive.csv"),
          row.names = FALSE)
cat("\nSaved: kvasir_retention_descriptive.csv\n")

# ============================================================
#  T1, F1 — pooled test-case composition
# ============================================================
cat("\n\n========================================================\n")
cat("  T1, F1: pooled test-case composition (test-case weighted)\n")
cat("========================================================\n")

pooled_n <- aggregate(repo_slug ~ agent + variant, data = d, FUN = length)
names(pooled_n)[3] <- "n_branches"
pooled   <- aggregate(cbind(tests_exec, tests_passed, tests_failed,
                            tests_errors, tests_skipped) ~ agent + variant,
                      data = d, FUN = sum)
pooled <- merge(pooled_n, pooled, by = c("agent", "variant"))
pooled$pass_pct  <- round(100 * pooled$tests_passed  / pooled$tests_exec, 1)
pooled$fail_pct  <- round(100 * pooled$tests_failed  / pooled$tests_exec, 1)
pooled$error_pct <- round(100 * pooled$tests_errors  / pooled$tests_exec, 1)
pooled$skip_pct  <- round(100 * pooled$tests_skipped / pooled$tests_exec, 1)
pooled$agent   <- as.character(pooled$agent)
pooled$variant <- as.character(pooled$variant)
pooled <- pooled[order(factor(pooled$agent, levels = c("claude", "codex")),
                       factor(pooled$variant, levels = c("generated", "v2", "v3"))), ]
pooled_cells <- pooled
pooled_variant_n <- aggregate(repo_slug ~ variant, data = d, FUN = length)
names(pooled_variant_n)[2] <- "n_branches"
pooled_variant <- aggregate(cbind(tests_exec, tests_passed, tests_failed,
                                  tests_errors, tests_skipped) ~ variant,
                            data = d, FUN = sum)
pooled_variant <- merge(pooled_variant_n, pooled_variant, by = "variant")
pooled_variant$agent <- "All"
pooled_variant$pass_pct  <- round(100 * pooled_variant$tests_passed  / pooled_variant$tests_exec, 1)
pooled_variant$fail_pct  <- round(100 * pooled_variant$tests_failed  / pooled_variant$tests_exec, 1)
pooled_variant$error_pct <- round(100 * pooled_variant$tests_errors  / pooled_variant$tests_exec, 1)
pooled_variant$skip_pct  <- round(100 * pooled_variant$tests_skipped / pooled_variant$tests_exec, 1)
pooled_variant <- pooled_variant[, names(pooled_cells)]
pooled_variant <- pooled_variant[order(factor(pooled_variant$variant,
                                             levels = c("generated", "v2", "v3"))), ]
pooled <- rbind(pooled_cells, pooled_variant)
print(pooled, row.names = FALSE)

write.csv(pooled,
          file.path(derived_dir, "kvasir_pooled_composition.csv"),
          row.names = FALSE)

pdf(file.path(derived_dir, "kvasir_pooled_composition.pdf"),
    width = 9, height = 4.5)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 7), xpd = TRUE)
for (ag in levels(d$agent)) {
  sub <- pooled_cells[pooled_cells$agent == ag, ]
  mat <- t(as.matrix(sub[, c("pass_pct", "fail_pct", "error_pct", "skip_pct")]))
  colnames(mat) <- as.character(sub$variant)
  rownames(mat) <- c("passed", "failed", "errored", "skipped")
  barplot(mat,
          main = paste("Pooled composition —", ag),
          col  = c("forestgreen", "firebrick", "darkorange", "gray60"),
          ylab = "share of executed test cases (%)",
          ylim = c(0, 100))
  legend("topright", inset = c(-0.32, 0),
         legend = rownames(mat),
         fill   = c("forestgreen", "firebrick", "darkorange", "gray60"),
         cex    = 0.7, bty = "n")
}
par(op); dev.off()
cat("\nSaved: kvasir_pooled_composition.csv, kvasir_pooled_composition.pdf\n")

# ============================================================
#  T2, F2 — branch-level rate summaries
#
#  Note: active_pass_rate is aggregated separately because it is
#  NA when a branch executed only skipped tests. Joining it into
#  the same cbind() call as the other rates would drop the
#  affected branch from every column of the combined aggregate.
# ============================================================
cat("\n\n========================================================\n")
cat("  T2, F2: branch-level rate outcomes\n")
cat("========================================================\n")

main_rates <- c("tests_exec", "pass_rate", "fail_rate", "error_rate",
                "skip_rate", "nonpass_rate")
branch_main <- aggregate(as.formula(
                  paste("cbind(", paste(main_rates, collapse = ", "),
                        ") ~ agent + variant")),
                data = d, FUN = rate_summary)
branch_main <- flatten_aggregate(branch_main, main_rates)

# Separate aggregate for active_pass_rate so that the NA row for the
# Codex generated branch with 3 executed-and-skipped tests does not
# remove it from the other-rate columns.
d_active <- d[!is.na(d$active_pass_rate), ]
branch_active <- aggregate(active_pass_rate ~ agent + variant,
                           data = d_active, FUN = rate_summary)
branch_active <- flatten_aggregate(branch_active, "active_pass_rate")

branch_summary_cells <- merge(branch_main, branch_active,
                              by = c("agent", "variant"), all = TRUE)
branch_summary_cells$agent   <- as.character(branch_summary_cells$agent)
branch_summary_cells$variant <- as.character(branch_summary_cells$variant)
branch_summary_cells <- branch_summary_cells[
  order(factor(branch_summary_cells$agent, levels = c("claude", "codex")),
        factor(branch_summary_cells$variant, levels = c("generated", "v2", "v3"))), ]

branch_main_variant <- aggregate(as.formula(
                         paste("cbind(", paste(main_rates, collapse = ", "),
                               ") ~ variant")),
                       data = d, FUN = rate_summary)
branch_main_variant <- flatten_aggregate(branch_main_variant, main_rates)
branch_active_variant <- aggregate(active_pass_rate ~ variant,
                                   data = d_active, FUN = rate_summary)
branch_active_variant <- flatten_aggregate(branch_active_variant,
                                           "active_pass_rate")
branch_summary_variant <- merge(branch_main_variant, branch_active_variant,
                                by = "variant", all = TRUE)
branch_summary_variant$agent <- "All"
branch_summary_variant <- branch_summary_variant[, names(branch_summary_cells)]
branch_summary_variant <- branch_summary_variant[
  order(factor(branch_summary_variant$variant,
               levels = c("generated", "v2", "v3"))), ]

branch_summary <- rbind(branch_summary_cells, branch_summary_variant)
print(branch_summary, row.names = FALSE)

write.csv(branch_summary,
          file.path(derived_dir, "kvasir_branch_rates.csv"),
          row.names = FALSE)

d$cell <- factor(paste(d$agent, d$variant, sep = "_"),
                 levels = c("claude_generated", "codex_generated",
                            "claude_v2",        "codex_v2",
                            "claude_v3",        "codex_v3"))

pdf(file.path(derived_dir, "kvasir_pass_rate_boxplot.pdf"),
    width = 8, height = 4.5)
op <- par(mar = c(5, 4, 3, 6), xpd = TRUE)
boxplot(pass_rate ~ cell, data = d,
        at      = c(1, 2, 4, 5, 7, 8),
        col     = c("steelblue", "tomato"),
        names   = rep(c("claude", "codex"), 3),
        ylim    = c(0, 1),
        ylab    = "branch pass_rate (passed / executed)",
        xlab    = "",
        main    = "Per-branch pass_rate by reconstruction agent and variant",
        las     = 1,
        outline = TRUE)
abline(h = c(0.5, 0.75, 0.9, 1.0), col = "gray80", lty = 3)
axis(1, at = c(1.5, 4.5, 7.5),
     labels = c("generated", "v2", "v3"),
     tick = FALSE, line = 1.8)
legend("topright", inset = c(-0.20, 0),
       legend = c("claude", "codex"),
       fill   = c("steelblue", "tomato"),
       bty = "n", cex = 0.85)
par(op); dev.off()
cat("\nSaved: kvasir_branch_rates.csv, kvasir_pass_rate_boxplot.pdf\n")

# ============================================================
#  T3 — pass-rate threshold shares
# ============================================================
cat("\n\n========================================================\n")
cat("  T3: pass-rate threshold shares\n")
cat("========================================================\n")

d$pass_ge_50  <- as.integer(d$pass_rate >= 0.50)
d$pass_ge_75  <- as.integer(d$pass_rate >= 0.75)
d$pass_ge_90  <- as.integer(d$pass_rate >= 0.90)
d$pass_eq_100 <- as.integer(d$pass_rate == 1.00)

thr_n   <- aggregate(repo_slug ~ agent + variant, data = d, FUN = length)
names(thr_n)[3] <- "n_branches"
thr_sum <- aggregate(cbind(pass_ge_50, pass_ge_75, pass_ge_90, pass_eq_100) ~
                       agent + variant, data = d, FUN = sum)
thr <- merge(thr_n, thr_sum, by = c("agent", "variant"))
thr$pct_ge_50  <- round(100 * thr$pass_ge_50  / thr$n_branches, 1)
thr$pct_ge_75  <- round(100 * thr$pass_ge_75  / thr$n_branches, 1)
thr$pct_ge_90  <- round(100 * thr$pass_ge_90  / thr$n_branches, 1)
thr$pct_eq_100 <- round(100 * thr$pass_eq_100 / thr$n_branches, 1)
thr$agent   <- as.character(thr$agent)
thr$variant <- as.character(thr$variant)
thr <- thr[order(factor(thr$agent, levels = c("claude", "codex")),
                 factor(thr$variant, levels = c("generated", "v2", "v3"))), ]
thr_cells <- thr
thr_variant_n <- aggregate(repo_slug ~ variant, data = d, FUN = length)
names(thr_variant_n)[2] <- "n_branches"
thr_variant_sum <- aggregate(cbind(pass_ge_50, pass_ge_75, pass_ge_90,
                                   pass_eq_100) ~ variant, data = d, FUN = sum)
thr_variant <- merge(thr_variant_n, thr_variant_sum, by = "variant")
thr_variant$agent <- "All"
thr_variant$pct_ge_50  <- round(100 * thr_variant$pass_ge_50  / thr_variant$n_branches, 1)
thr_variant$pct_ge_75  <- round(100 * thr_variant$pass_ge_75  / thr_variant$n_branches, 1)
thr_variant$pct_ge_90  <- round(100 * thr_variant$pass_ge_90  / thr_variant$n_branches, 1)
thr_variant$pct_eq_100 <- round(100 * thr_variant$pass_eq_100 / thr_variant$n_branches, 1)
thr_variant <- thr_variant[, names(thr_cells)]
thr_variant <- thr_variant[order(factor(thr_variant$variant,
                                        levels = c("generated", "v2", "v3"))), ]
thr <- rbind(thr_cells, thr_variant)
print(thr, row.names = FALSE)
write.csv(thr,
          file.path(derived_dir, "kvasir_threshold_shares.csv"),
          row.names = FALSE)
cat("\nSaved: kvasir_threshold_shares.csv\n")

# ============================================================
#  T6 — clean-to-v2 and clean-to-v3 within-repo change
# ============================================================
cat("\n\n========================================================\n")
cat("  T6: per-repo clean-to-degraded within-repo change\n")
cat("========================================================\n")

wide_change <- function(data, outcome, from, to,
                        direction = c("drop", "increase")) {
  direction <- match.arg(direction)
  w <- reshape(data[, c("repo_slug", "agent", "variant", outcome)],
               idvar = c("repo_slug", "agent"),
               timevar = "variant",
               direction = "wide")
  names(w) <- sub(paste0("^", outcome, "\\."), "", names(w))
  if (!all(c(from, to) %in% names(w))) return(NULL)
  ok <- complete.cases(w[, c(from, to)])
  w <- w[ok, ]
  if (direction == "drop") {
    w$change <- w[[from]] - w[[to]]
  } else {
    w$change <- w[[to]] - w[[from]]
  }
  w
}

change_specs <- list(
  pass_rate    = "drop",
  fail_rate    = "increase",
  error_rate   = "increase",
  nonpass_rate = "increase"
)

comparisons <- list(
  list(name = "generated_to_v2", from = "generated", to = "v2"),
  list(name = "generated_to_v3", from = "generated", to = "v3")
)

change_rows <- list()
for (cmp in comparisons) {
  for (o in names(change_specs)) {
    cat("\n# Comparison:", cmp$name,
        "| Outcome:", o,
        "(", change_specs[[o]], ")\n")
    w <- wide_change(d, o, cmp$from, cmp$to, change_specs[[o]])
    if (is.null(w) || nrow(w) == 0) next
    agg <- aggregate(change ~ agent, data = w,
                     FUN = function(x) c(mean   = round(mean(x), 3),
                                         median = round(median(x), 3),
                                         q25    = round(unname(quantile(x, 0.25)), 3),
                                         q75    = round(unname(quantile(x, 0.75)), 3),
                                         n      = length(x)))
    agg <- flatten_aggregate(agg, "change")
    test <- tryCatch(wilcox.test(change ~ agent, data = w, exact = FALSE),
                     error = function(e) e)
    print(agg, row.names = FALSE)
    print(test)
    agg$comparison <- cmp$name
    agg$outcome    <- o
    agg$direction  <- change_specs[[o]]
    agg$wilcox_p   <- if (inherits(test, "htest")) round(test$p.value, 4) else NA
    change_rows[[paste(cmp$name, o, sep = "__")]] <- agg
  }
}
change_table <- do.call(rbind, change_rows)
ordered_cols <- c("comparison", "outcome", "direction", "agent",
                  setdiff(names(change_table),
                          c("comparison", "outcome", "direction", "agent")))
change_table <- change_table[, ordered_cols]
write.csv(change_table,
          file.path(derived_dir, "kvasir_clean_to_degraded_change.csv"),
          row.names = FALSE)
cat("\nSaved: kvasir_clean_to_degraded_change.csv\n")

# ============================================================
#  INFERENCE (console only; results reported in prose)
# ============================================================
cat("\n\n========================================================\n")
cat("  INFERENCE: variant and agent effects on branch outcomes\n")
cat("========================================================\n")

outcomes <- list(
  pass_rate    = "passed / executed",
  fail_rate    = "failed / executed",
  error_rate   = "errors / executed",
  nonpass_rate = "(failed + errors + skipped) / executed"
)

for (o in names(outcomes)) {
  dd <- d[!is.na(d[[o]]), ]
  y  <- dd[[o]]
  cat("\n--- Outcome:", o, "—", outcomes[[o]], "---\n")
  cat("ANOVA: y ~ variant * agent\n")
  print(anova(lm(y ~ dd$variant * dd$agent)))
  cat("ANOVA: y ~ agent\n")
  print(anova(lm(y ~ dd$agent)))
  cat("Kruskal-Wallis: y ~ variant\n")
  print(kruskal.test(y ~ dd$variant))

  cat("\nVariant contrasts within agent (Wilcoxon):\n")
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

  cat("\nClaude vs Codex within variant (Wilcoxon):\n")
  for (v in levels(dd$variant)) {
    cat("  variant =", v, "\n")
    sub <- dd[dd$variant == v, ]
    print_test(wilcox.test(sub[[o]] ~ droplevels(sub$agent), exact = FALSE))
  }
}

# ============================================================
#  PAIRED AGENT CONTRAST WITHIN (repo, variant)
#  -- supports the "p approx .040" claim for fail_rate in the
#     Agent Robustness Under Degradation subsection of Results.
# ============================================================
cat("\n\n========================================================\n")
cat("  Paired Claude vs Codex within (repo, variant) (Wilcoxon)\n")
cat("========================================================\n")

paired_agent_test <- function(data, outcome) {
  w <- reshape(data[, c("repo_slug", "variant", "agent", outcome)],
               idvar = c("repo_slug", "variant"),
               timevar = "agent",
               direction = "wide")
  names(w) <- sub(paste0("^", outcome, "\\."), "", names(w))
  if (!all(c("claude", "codex") %in% names(w))) return(NULL)
  ok <- complete.cases(w[, c("claude", "codex")])
  w <- w[ok, ]
  list(w = w,
       test = if (nrow(w) >= 2)
                tryCatch(wilcox.test(w$claude, w$codex,
                                     paired = TRUE, exact = FALSE),
                         error = function(e) e)
              else NA)
}

for (o in names(outcomes)) {
  cat("\n# Outcome:", o, "\n")
  r <- paired_agent_test(d, o)
  if (is.null(r)) { cat("  no paired data\n"); next }
  cat("  n paired (repo, variant) pairs =", nrow(r$w), "\n")
  print(r$test)
}

# ============================================================
#  MIXED-EFFECTS MODEL with repository random effect
#  -- supports the "p approx .128" claim for fail_rate in the
#     Agent Robustness Under Degradation subsection of Results.
#     Requires the lmerTest package for Satterthwaite p-values;
#     falls back to lme4 (coefficient table only) if lmerTest is
#     not installed, and emits an install hint otherwise.
# ============================================================
cat("\n\n========================================================\n")
cat("  Mixed-effects: outcome ~ agent * variant + (1 | repo_slug)\n")
cat("========================================================\n")

fit_mixed <- function(outcome, data) {
  f <- as.formula(paste0(outcome, " ~ agent * variant + (1 | repo_slug)"))
  tryCatch(lmer(f, data = data, REML = FALSE), error = function(e) e)
}

if (requireNamespace("lmerTest", quietly = TRUE)) {
  suppressPackageStartupMessages(library(lmerTest))
  for (o in names(outcomes)) {
    cat("\n# Outcome:", o, "\n")
    m <- fit_mixed(o, d)
    if (inherits(m, "error")) {
      cat("  model failed: ", conditionMessage(m), "\n")
      next
    }
    cat("  Fixed-effect ANOVA (Satterthwaite df):\n")
    print(anova(m))
    cat("  Fixed-effect coefficient table:\n")
    print(summary(m)$coefficients)
  }
} else if (requireNamespace("lme4", quietly = TRUE)) {
  suppressPackageStartupMessages(library(lme4))
  cat("Note: lmerTest is not installed; lme4 alone does not produce ",
      "p-values for fixed effects.\n",
      "Install lmerTest for p-values: install.packages('lmerTest')\n",
      sep = "")
  for (o in names(outcomes)) {
    cat("\n# Outcome:", o, "\n")
    m <- fit_mixed(o, d)
    if (inherits(m, "error")) {
      cat("  model failed: ", conditionMessage(m), "\n")
      next
    }
    print(summary(m)$coefficients)
  }
} else {
  cat("Neither lmerTest nor lme4 is installed.\n",
      "Install lmerTest for the mixed-effects analysis: ",
      "install.packages('lmerTest')\n",
      sep = "")
}

cat("\n===== DONE =====\n")
