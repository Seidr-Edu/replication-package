# ============================================================
#  CROSS-DIMENSION ANALYSIS
#
#  Joins the three evaluation services on (repo_slug, agent, variant):
#    Mimir       structural similarity (exact, fuzzy)
#    Kvasir      functional behaviour (per-branch pass_rate)
#    Lidskjalv   code quality (Sonar-derived metrics, LOC ratio)
#
#  Joint analysis cohort:
#    Mimir row present
#    Sonar reconstructed row present (variant in {generated, v2, v3})
#    Kvasir cohort filter passes:
#      porting.execution.tests_executed > 0
#      AND diagnostics.write_scope.violation_count == 0
#
#  Derived outcomes:
#    pass_rate            = (executed - failed - errors - skipped) / executed
#    loc_ratio            = ncloc_reconstructed / ncloc_original
#    complexity_per_kloc  = complexity            / (ncloc / 1000)
#    cog_complexity_per_kloc = cognitive_complexity / (ncloc / 1000)
#    code_smells_per_kloc = code_smells          / (ncloc / 1000)
#    bugs_per_kloc        = bugs                 / (ncloc / 1000)
#
#  Derived figures (written to data/derived/cross_dimension/):
#    crossdim_mimir_vs_kvasir.pdf       (F1: branch-level joint scatter)
#    crossdim_pass_vs_loc.pdf           (F2: agent split on pass x LOC)
#    crossdim_parallel_coordinates.pdf  (F3: cell-mean co-movement)
# ============================================================

# ---- locate input and output ----------------------------------
script_path <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) {
    return(normalizePath(sub("^--file=", "", f[1]), mustWork = FALSE))
  }

  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    ofile <- frames[[i]]$ofile
    if (!is.null(ofile) && length(ofile) > 0 && nzchar(ofile[1])) {
      return(normalizePath(ofile[1], mustWork = FALSE))
    }
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                     error = function(e) "")
    if (!is.null(path) && length(path) > 0 && nzchar(path[1])) {
      return(normalizePath(path[1], mustWork = FALSE))
    }
  }

  NA_character_
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

script_dir <- if (!is.na(script_path)) dirname(script_path) else getwd()

path_parents <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  out <- path
  repeat {
    parent <- dirname(path)
    if (identical(parent, path)) break
    out <- c(out, parent)
    path <- parent
  }
  out
}

is_repo_root <- function(path) {
  file.exists(file.path(path, "data", "exported", "kvasir_stats.csv")) &&
    file.exists(file.path(path, "data", "exported", "sonar_projects.csv")) &&
    file.exists(file.path(path, "scripts", "Cross_Dimension.R"))
}

find_repo_root <- function(start_dirs) {
  candidates <- unique(unlist(lapply(start_dirs, path_parents),
                              use.names = FALSE))
  hit <- candidates[vapply(candidates, is_repo_root, logical(1))]
  if (length(hit) == 0) {
    stop("Could not find the replication-package repository root. Tried from:\n",
         paste(normalizePath(start_dirs, mustWork = FALSE), collapse = "\n"))
  }
  hit[1]
}

repo_root <- find_repo_root(c(
  script_dir,
  getwd(),
  file.path(getwd(), "replication-package"),
  "/Users/oleremidahl/Documents/Master/replication-package"
))

kvasir_csv <- find_path(
  file.path(repo_root, "data", "exported", "kvasir_stats.csv"),
  "kvasir_stats.csv"
)

sonar_csv <- find_path(
  file.path(repo_root, "data", "exported", "sonar_projects.csv"),
  "sonar_projects.csv"
)

mimir_csv <- find_path(c(
  file.path(repo_root, "data", "exported", "mimir.csv"),
  file.path(dirname(repo_root), "Heimdall", "exports",
            "heimdall-analysis-20260520T203812Z", "tables", "mimir.csv"),
  file.path(dirname(getwd()), "Heimdall", "exports",
            "heimdall-analysis-20260520T203812Z", "tables", "mimir.csv"),
  "/Users/oleremidahl/Documents/Master/Heimdall/exports/heimdall-analysis-20260520T203812Z/tables/mimir.csv"
), "mimir.csv")

derived_dir <- file.path(repo_root, "data", "derived", "cross_dimension")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(derived_dir)) {
  stop("Could not create output directory: ", derived_dir)
}

cat("Repo   :", repo_root,   "\n")
cat("Mimir  :", mimir_csv,   "\n")
cat("Kvasir :", kvasir_csv,  "\n")
cat("Sonar  :", sonar_csv,   "\n")
cat("Output :", derived_dir, "\n\n")

# ---- load ------------------------------------------------------
mimir  <- read.csv(mimir_csv,  stringsAsFactors = FALSE)
kvasir <- read.csv(kvasir_csv, stringsAsFactors = FALSE)
sonar  <- read.csv(sonar_csv,  stringsAsFactors = FALSE)

# ---- prepare Kvasir cohort flag and pass_rate ------------------
kvasir$tests_exec    <- kvasir$porting.execution.tests_executed
kvasir$tests_failed  <- kvasir$porting.execution.tests_failed
kvasir$tests_errors  <- kvasir$porting.execution.tests_errors
kvasir$tests_skipped <- kvasir$porting.execution.tests_skipped
kvasir$ws_viol       <- kvasir$diagnostics.write_scope.violation_count
kvasir$ws_viol[is.na(kvasir$ws_viol)] <- 0

kvasir$tests_passed  <- kvasir$tests_exec - kvasir$tests_failed -
                        kvasir$tests_errors - kvasir$tests_skipped
kvasir$pass_rate     <- ifelse(kvasir$tests_exec > 0,
                               kvasir$tests_passed / kvasir$tests_exec, NA)
kvasir$kvasir_in_cohort <- kvasir$tests_exec > 0 & kvasir$ws_viol == 0

kvasir_recon <- kvasir[, c("repo_slug", "agent", "variant",
                           "tests_exec", "pass_rate", "kvasir_in_cohort")]

# ---- prepare Sonar reconstructed rows + original ncloc lookup ----
sonar_original <- aggregate(ncloc ~ repo_slug,
                            data = sonar[sonar$variant == "original", ],
                            FUN = function(x) x[1])
names(sonar_original)[2] <- "ncloc_original"

sonar_recon <- sonar[sonar$variant %in% c("generated", "v2", "v3"), ]
sonar_recon <- merge(sonar_recon, sonar_original, by = "repo_slug", all.x = TRUE)
sonar_recon$loc_ratio <- sonar_recon$ncloc / sonar_recon$ncloc_original
kloc <- sonar_recon$ncloc / 1000
sonar_recon$complexity_per_kloc     <- sonar_recon$complexity            / kloc
sonar_recon$cog_complexity_per_kloc <- sonar_recon$cognitive_complexity  / kloc
sonar_recon$code_smells_per_kloc    <- sonar_recon$code_smells           / kloc
sonar_recon$bugs_per_kloc           <- sonar_recon$bugs                  / kloc

sonar_keep <- c("repo_slug", "agent", "variant", "ncloc", "ncloc_original",
                "loc_ratio", "complexity_per_kloc", "cog_complexity_per_kloc",
                "code_smells_per_kloc", "bugs_per_kloc",
                "reliability_rating", "security_rating")
sonar_recon <- sonar_recon[, sonar_keep]

# ---- prepare Mimir reconstructed rows -------------------------
mimir_keep <- c("repo_slug", "agent", "variant",
                "exact_similarity", "fuzzy_similarity")
mimir_recon <- mimir[, mimir_keep]

# ---- join on (repo_slug, agent, variant) ----------------------
joint_all <- merge(mimir_recon, kvasir_recon,
                   by = c("repo_slug", "agent", "variant"),
                   all = TRUE)
joint_all <- merge(joint_all, sonar_recon,
                   by = c("repo_slug", "agent", "variant"),
                   all = TRUE)
joint_all$variant <- factor(joint_all$variant,
                            levels = c("generated", "v2", "v3"))
joint_all$agent   <- factor(joint_all$agent,
                            levels = c("claude", "codex"))

cat("===== JOINT DATA =====\n")
cat("Mimir rows                                  :", nrow(mimir_recon), "\n")
cat("Kvasir rows                                 :", nrow(kvasir_recon),
    "  (cohort =", sum(kvasir_recon$kvasir_in_cohort, na.rm = TRUE), ")\n")
cat("Sonar reconstructed rows                    :", nrow(sonar_recon), "\n")
cat("Sonar originals (baseline ncloc lookup)     :", nrow(sonar_original), "\n")
cat("Joint outer-join rows                       :", nrow(joint_all), "\n")

# Joint analysis cohort: all three measurements present + Kvasir cohort filter
joint <- joint_all[
  !is.na(joint_all$exact_similarity) &
  !is.na(joint_all$pass_rate) &
  !is.na(joint_all$loc_ratio) &
  joint_all$kvasir_in_cohort %in% TRUE,
]
joint$variant <- droplevels(joint$variant)
joint$agent   <- droplevels(joint$agent)
cat("Joint analysis cohort (all three + Kvasir filter):", nrow(joint), "\n\n")

# ============================================================
#  COHORT ROBUSTNESS CHECK
#  -- per-dimension mean on (a) the joint cohort vs
#     (b) the full per-dimension data
# ============================================================
cat("\n========================================================\n")
cat("  Cohort robustness check\n")
cat("========================================================\n")

robust_rows <- list()
metrics_robust <- c("exact_similarity", "fuzzy_similarity",
                    "pass_rate", "loc_ratio",
                    "complexity_per_kloc",
                    "code_smells_per_kloc", "bugs_per_kloc")
for (m in metrics_robust) {
  full_vec  <- joint_all[[m]]
  full_vec  <- full_vec[!is.na(full_vec)]
  joint_vec <- joint[[m]]
  joint_vec <- joint_vec[!is.na(joint_vec)]
  robust_rows[[m]] <- data.frame(
    metric       = m,
    full_n       = length(full_vec),
    full_mean    = round(mean(full_vec),   3),
    full_median  = round(median(full_vec), 3),
    joint_n      = length(joint_vec),
    joint_mean   = round(mean(joint_vec),  3),
    joint_median = round(median(joint_vec),3),
    mean_delta   = round(mean(joint_vec) - mean(full_vec), 3)
  )
}
robust <- do.call(rbind, robust_rows)
rownames(robust) <- NULL
print(robust, row.names = FALSE)

# ============================================================
#  VARIANT CELL MEANS (joint cohort)
# ============================================================
cat("\n========================================================\n")
cat("  Variant cell means on the joint cohort\n")
cat("========================================================\n")

cell_metrics <- c("exact_similarity", "fuzzy_similarity",
                  "pass_rate", "loc_ratio",
                  "complexity_per_kloc",
                  "code_smells_per_kloc", "bugs_per_kloc")
cell_mean <- function(x) round(mean(x, na.rm = TRUE), 3)

cell_means_cell <- aggregate(
  as.formula(paste("cbind(", paste(cell_metrics, collapse = ", "),
                   ") ~ agent + variant")),
  data = joint, FUN = cell_mean)
cell_means_pooled <- aggregate(
  as.formula(paste("cbind(", paste(cell_metrics, collapse = ", "),
                   ") ~ variant")),
  data = joint, FUN = cell_mean)
cell_means_pooled$agent <- "pooled"
cell_means_pooled <- cell_means_pooled[, names(cell_means_cell)]
cell_means_all <- rbind(cell_means_cell, cell_means_pooled)
cell_means_all <- cell_means_all[
  order(factor(cell_means_all$agent,   levels = c("claude","codex","pooled")),
        factor(cell_means_all$variant, levels = c("generated","v2","v3"))), ]
print(cell_means_all, row.names = FALSE)

# ============================================================
#  POOLED BRANCH-LEVEL SPEARMAN CORRELATIONS (joint cohort)
# ============================================================
cat("\n========================================================\n")
cat("  Pooled branch-level Spearman correlations\n")
cat("========================================================\n")

spearman_row <- function(x, y, label_x, label_y, group = "pooled") {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(data.frame(group=group, x=label_x, y=label_y,
                                       rho=NA, p=NA, n=length(x)))
  tt <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  data.frame(group = group, x = label_x, y = label_y,
             rho = round(unname(tt$estimate), 3),
             p   = signif(tt$p.value, 3),
             n   = length(x))
}

cor_metrics <- c("exact_similarity", "fuzzy_similarity",
                 "pass_rate", "loc_ratio",
                 "complexity_per_kloc", "code_smells_per_kloc",
                 "bugs_per_kloc", "cog_complexity_per_kloc")
pooled_rows <- list()
for (i in seq_along(cor_metrics)) {
  for (j in seq_along(cor_metrics)) {
    if (j <= i) next
    pooled_rows[[length(pooled_rows) + 1]] <-
      spearman_row(joint[[cor_metrics[i]]], joint[[cor_metrics[j]]],
                   cor_metrics[i], cor_metrics[j], "pooled")
  }
}
pooled_cor <- do.call(rbind, pooled_rows)
print(pooled_cor, row.names = FALSE)

# ============================================================
#  HEADLINE CORRELATIONS PER VARIANT AND PER AGENT
# ============================================================
cat("\n========================================================\n")
cat("  Headline correlations per variant\n")
cat("========================================================\n")

headline_pairs <- list(
  c("exact_similarity",  "pass_rate"),
  c("exact_similarity",  "loc_ratio"),
  c("pass_rate",         "loc_ratio"),
  c("exact_similarity",  "complexity_per_kloc"),
  c("pass_rate",         "complexity_per_kloc"),
  c("loc_ratio",         "complexity_per_kloc"),
  c("exact_similarity",  "bugs_per_kloc"),
  c("pass_rate",         "bugs_per_kloc"),
  c("exact_similarity",  "code_smells_per_kloc"),
  c("pass_rate",         "code_smells_per_kloc")
)

per_variant_rows <- list()
for (v in c("generated", "v2", "v3")) {
  sub <- joint[joint$variant == v, ]
  for (pair in headline_pairs) {
    per_variant_rows[[length(per_variant_rows) + 1]] <-
      spearman_row(sub[[pair[1]]], sub[[pair[2]]],
                   pair[1], pair[2], v)
  }
}
per_variant_cor <- do.call(rbind, per_variant_rows)
print(per_variant_cor, row.names = FALSE)

cat("\n========================================================\n")
cat("  Headline correlations per agent\n")
cat("========================================================\n")

per_agent_rows <- list()
for (ag in c("claude", "codex", "pooled")) {
  sub <- if (ag == "pooled") joint else joint[joint$agent == ag, ]
  for (pair in headline_pairs) {
    per_agent_rows[[length(per_agent_rows) + 1]] <-
      spearman_row(sub[[pair[1]]], sub[[pair[2]]],
                   pair[1], pair[2], ag)
  }
}
per_agent_cor <- do.call(rbind, per_agent_rows)
print(per_agent_cor, row.names = FALSE)

# ============================================================
#  PER-REPOSITORY DELTA CORRELATIONS (clean -> v3)
# ============================================================
cat("\n========================================================\n")
cat("  Per-repository delta correlations (clean -> v3)\n")
cat("========================================================\n")

delta_metrics <- c("exact_similarity", "fuzzy_similarity",
                   "pass_rate", "loc_ratio",
                   "complexity_per_kloc",
                   "code_smells_per_kloc", "bugs_per_kloc")
delta_pairs <- list(
  c("exact_similarity", "pass_rate"),
  c("exact_similarity", "loc_ratio"),
  c("pass_rate",        "loc_ratio"),
  c("exact_similarity", "complexity_per_kloc"),
  c("pass_rate",        "complexity_per_kloc"),
  c("exact_similarity", "bugs_per_kloc"),
  c("pass_rate",        "bugs_per_kloc")
)

compute_deltas <- function(data, by_agent) {
  # build wide table per (repo_slug, agent) with one column per (metric, variant)
  if (by_agent == "pooled") {
    grp <- data[, c("repo_slug", "agent", "variant", delta_metrics)]
  } else {
    grp <- data[data$agent == by_agent,
                c("repo_slug", "agent", "variant", delta_metrics)]
  }
  if (nrow(grp) == 0) return(NULL)
  w <- reshape(grp, idvar = c("repo_slug", "agent"),
               timevar = "variant", direction = "wide")
  # build delta columns
  delta_df <- w[, c("repo_slug", "agent"), drop = FALSE]
  for (m in delta_metrics) {
    gen_col <- paste0(m, ".generated")
    v3_col  <- paste0(m, ".v3")
    if (gen_col %in% names(w) && v3_col %in% names(w)) {
      delta_df[[paste0("d_", m)]] <- w[[v3_col]] - w[[gen_col]]
    }
  }
  delta_df
}

delta_rows <- list()
for (group in c("claude", "codex", "pooled")) {
  delta_df <- compute_deltas(joint, group)
  if (is.null(delta_df)) next
  for (pair in delta_pairs) {
    col1 <- paste0("d_", pair[1]); col2 <- paste0("d_", pair[2])
    if (!(col1 %in% names(delta_df)) || !(col2 %in% names(delta_df))) next
    delta_rows[[length(delta_rows) + 1]] <-
      spearman_row(delta_df[[col1]], delta_df[[col2]],
                   pair[1], pair[2], group)
  }
}
delta_cor <- do.call(rbind, delta_rows)
print(delta_cor, row.names = FALSE)

# ============================================================
#  QUADRANT COUNTS (median splits) on clean and v3 cohorts
# ============================================================
cat("\n========================================================\n")
cat("  Quadrant counts (median splits)\n")
cat("========================================================\n")

quad_pairs <- list(
  c("exact_similarity", "pass_rate"),
  c("exact_similarity", "loc_ratio"),
  c("pass_rate",        "loc_ratio"),
  c("exact_similarity", "complexity_per_kloc")
)

quad_rows <- list()
for (v in c("generated", "v3")) {
  sub <- joint[joint$variant == v, ]
  for (pair in quad_pairs) {
    x <- sub[[pair[1]]]; y <- sub[[pair[2]]]
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 4) next
    mx <- median(x); my <- median(y)
    quad_rows[[length(quad_rows) + 1]] <- data.frame(
      variant = v, x = pair[1], y = pair[2], n = length(x),
      median_x = round(mx, 3), median_y = round(my, 3),
      hi_hi = sum(x >= mx & y >= my),
      hi_lo = sum(x >= mx & y <  my),
      lo_hi = sum(x <  mx & y >= my),
      lo_lo = sum(x <  mx & y <  my),
      pct_agreement = round(100 * (sum(x >= mx & y >= my) +
                                   sum(x <  mx & y <  my)) / length(x), 1)
    )
  }
}
quadrants <- do.call(rbind, quad_rows)
print(quadrants, row.names = FALSE)

# ============================================================
#  F1 - branch-level scatter, Mimir exact x Kvasir pass_rate
# ============================================================
cat("\n========================================================\n")
cat("  F1: Mimir exact x Kvasir pass_rate scatter\n")
cat("========================================================\n")

pdf(file.path(derived_dir, "crossdim_mimir_vs_kvasir.pdf"),
    width = 9, height = 4.5)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))
shapes <- c(generated = 1, v2 = 2, v3 = 3)
for (ag in c("claude", "codex")) {
  sub <- joint[joint$agent == ag, ]
  plot(sub$exact_similarity, sub$pass_rate,
       pch = shapes[as.character(sub$variant)],
       col = "black",
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Mimir exact similarity",
       ylab = "Kvasir per-branch pass rate",
       main = paste("Joint scatter -", ag))
  abline(h = median(joint$pass_rate, na.rm = TRUE),
         v = median(joint$exact_similarity, na.rm = TRUE),
         col = "gray70", lty = 3)
  legend("bottomright",
         legend = names(shapes), pch = shapes,
         bty = "n", cex = 0.8, title = "variant")
}
par(op); dev.off()
cat("Saved: crossdim_mimir_vs_kvasir.pdf\n")

# ============================================================
#  F2 - pass_rate vs LOC ratio, faceted by agent
# ============================================================
cat("\n========================================================\n")
cat("  F2: Kvasir pass_rate vs LOC ratio scatter, by agent\n")
cat("========================================================\n")

pdf(file.path(derived_dir, "crossdim_pass_vs_loc.pdf"),
    width = 9, height = 4.5)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))
for (ag in c("claude", "codex")) {
  sub <- joint[joint$agent == ag, ]
  plot(sub$loc_ratio, sub$pass_rate,
       pch = shapes[as.character(sub$variant)],
       col = "black",
       xlim = c(0, 1.5), ylim = c(0, 1),
       xlab = "LOC ratio (reconstructed / original)",
       ylab = "Kvasir per-branch pass rate",
       main = paste("pass_rate vs LOC ratio -", ag))
  abline(v = 1, col = "gray70", lty = 2)
  # least-squares trend line for visual orientation
  ok <- !is.na(sub$loc_ratio) & !is.na(sub$pass_rate)
  if (sum(ok) >= 4) {
    fit <- lm(pass_rate ~ loc_ratio, data = sub[ok, ])
    xs <- seq(min(sub$loc_ratio[ok]), max(sub$loc_ratio[ok]), length.out = 50)
    lines(xs, predict(fit, newdata = data.frame(loc_ratio = xs)),
          col = "firebrick", lwd = 1.5)
  }
  legend("bottomright",
         legend = names(shapes), pch = shapes,
         bty = "n", cex = 0.8, title = "variant")
}
par(op); dev.off()
cat("Saved: crossdim_pass_vs_loc.pdf\n")

# ============================================================
#  F3 - parallel-coordinates of normalized cell means by variant
#  one line per agent, three axes (Mimir, Kvasir, LOC ratio)
# ============================================================
cat("\n========================================================\n")
cat("  F3: parallel-coordinates of cell means\n")
cat("========================================================\n")

# Build matrix: rows = agent x variant, cols = three normalized outcomes
cm <- cell_means_cell  # only claude / codex cells
norm_outcomes <- c("exact_similarity", "pass_rate", "loc_ratio")
# Normalise each outcome across all six cells to [0, 1]
cm_norm <- cm
for (o in norm_outcomes) {
  v <- cm[[o]]
  rng <- range(v, na.rm = TRUE)
  if (rng[2] > rng[1]) {
    cm_norm[[o]] <- (v - rng[1]) / (rng[2] - rng[1])
  } else {
    cm_norm[[o]] <- 0.5
  }
}

pdf(file.path(derived_dir, "crossdim_parallel_coordinates.pdf"),
    width = 7, height = 4.5)
op <- par(mar = c(4, 4, 3, 1))
plot(NA, xlim = c(1, length(norm_outcomes)), ylim = c(-0.05, 1.05),
     xlab = "", ylab = "normalized cell mean (0 = min cell, 1 = max cell)",
     xaxt = "n", yaxt = "n",
     main = "Cell-mean co-movement across dimensions")
axis(1, at = seq_along(norm_outcomes),
     labels = c("Mimir exact", "Kvasir pass", "LOC ratio"))
axis(2, at = c(0, 0.5, 1.0))
abline(v = seq_along(norm_outcomes), col = "gray85", lty = 3)
agent_colors <- c(claude = "steelblue", codex = "tomato")
variant_lty  <- c(generated = 1, v2 = 2, v3 = 3)
for (i in seq_len(nrow(cm_norm))) {
  row <- cm_norm[i, ]
  ys <- as.numeric(row[1, norm_outcomes])
  lines(seq_along(norm_outcomes), ys,
        col = agent_colors[as.character(row$agent)],
        lty = variant_lty[as.character(row$variant)], lwd = 2)
  points(seq_along(norm_outcomes), ys,
         pch = 19, col = agent_colors[as.character(row$agent)])
}
legend("topright",
       legend = c("claude", "codex"),
       col = agent_colors, lty = 1, lwd = 2, bty = "n",
       title = "agent", cex = 0.8)
legend("bottomright",
       legend = names(variant_lty),
       lty = variant_lty, lwd = 2, bty = "n",
       title = "variant", cex = 0.8)
par(op); dev.off()
cat("Saved: crossdim_parallel_coordinates.pdf\n")

cat("\n===== DONE =====\n")
