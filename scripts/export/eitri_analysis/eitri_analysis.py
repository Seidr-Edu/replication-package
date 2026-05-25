#!/usr/bin/env python3
"""
Eitri modeling-output analysis
==============================

Computes the aggregate statistics reported in the "Modeling Output" results
subsection from the Eitri export (eitri.csv).

The export contains two runs per repository. Because determinism has been
verified separately (see ../eitri_determinism/), this script de-duplicates to
one row per repository (first occurrence) before aggregating.

It produces three blocks of numbers:
  1. Model-characteristics descriptive statistics (min/median/mean/max/total).
  2. Degradation realization for the slightly (v2, 10%) and moderately
     (v3, 20%) degraded variants.
  3. Applied-operator composition (counts + share of total applied) for v2/v3.

Standard library only.

Usage:
    python3 eitri_analysis.py [path/to/eitri.csv]
"""

import csv
import os
import statistics
import sys


def to_int(s):
    s = (s or "").strip()
    return int(s) if s else 0


def to_float(s):
    s = (s or "").strip()
    return float(s) if s else 0.0


def describe_int(values):
    return {
        "min": min(values),
        "median": statistics.median(values),
        "mean": statistics.mean(values),
        "max": max(values),
        "sum": sum(values),
    }


def load_unique(path):
    with open(path, newline="") as f:
        rows = list(csv.DictReader(f))
    seen, unique = set(), []
    for r in rows:
        if r["repo_slug"] not in seen:
            seen.add(r["repo_slug"])
            unique.append(r)
    return rows, unique


def main(path):
    all_rows, repos = load_unique(path)
    n = len(repos)

    print("=" * 72)
    print("EITRI MODELING-OUTPUT ANALYSIS")
    print("=" * 72)
    print(f"CSV file            : {path}")
    print(f"Rows in export      : {len(all_rows)}")
    print(f"Unique repositories : {n}")
    print(f"All status == passed: {all(r['status'] == 'passed' for r in all_rows)}")
    print()

    # ---- 1. Model characteristics ----------------------------------------
    model_metrics = [
        ("type_count", "Types"),
        ("relation_count", "Relations"),
        ("source_file_count", "Source files"),
        ("package_count", "Packages"),
        ("top_level_type_count", "Top-level types"),
        ("nested_type_count", "Nested types"),
        ("class_count", "Classes"),
        ("interface_count", "Interfaces"),
        ("enum_count", "Enums"),
        ("record_count", "Records"),
        ("abstract_class_count", "Abstract classes"),
        ("annotation_count", "Annotations"),
    ]

    print("-" * 72)
    print("1. MODEL CHARACTERISTICS (n = %d repositories)" % n)
    print("-" * 72)
    print(f"{'Metric':<18}{'Min':>7}{'Median':>9}{'Mean':>9}{'Max':>7}{'Total':>9}")
    model_stats = {}
    for col, label in model_metrics:
        vals = [to_int(r[col]) for r in repos]
        d = describe_int(vals)
        model_stats[col] = d
        print(f"{label:<18}{d['min']:>7}{d['median']:>9.1f}{d['mean']:>9.1f}"
              f"{d['max']:>7}{d['sum']:>9}")
    print()
    # data-quality flags
    print("Data-quality notes:")
    print(f"  abstract_class_count: all zero?  "
          f"{all(to_int(r['abstract_class_count']) == 0 for r in repos)}")
    nonzero_anno = sum(1 for r in repos if to_int(r['annotation_count']) > 0)
    print(f"  annotation_count: repos with >0   {nonzero_anno} / {n}")
    nonzero_rec = sum(1 for r in repos if to_int(r['record_count']) > 0)
    print(f"  record_count:    repos with >0    {nonzero_rec} / {n}")
    src_paths = {to_int(r['source_path_count']) for r in repos}
    print(f"  source_path_count distinct values {sorted(src_paths)}")
    print()

    # ---- 2. Degradation realization --------------------------------------
    def realization(prefix, target):
        eff = [to_float(r[f"{prefix}_effective_percentage"]) for r in repos]
        elig = [to_int(r[f"{prefix}_eligible_candidate_count"]) for r in repos]
        appl = [to_int(r[f"{prefix}_applied_count"]) for r in repos]
        return {
            "target": target,
            "eff_mean": statistics.mean(eff),
            "eff_min": min(eff),
            "eff_max": max(eff),
            "elig_mean": statistics.mean(elig),
            "elig_sum": sum(elig),
            "appl_mean": statistics.mean(appl),
            "appl_sum": sum(appl),
        }

    v2 = realization("v2", 10)
    v3 = realization("v3", 20)

    print("-" * 72)
    print("2. DEGRADATION REALIZATION")
    print("-" * 72)
    print(f"{'Variant':<22}{'Target':>7}{'Eff.mean':>10}{'Eff.min':>9}"
          f"{'Eff.max':>9}{'Elig.mean':>11}{'Appl.mean':>11}{'Appl.tot':>10}")
    for name, d in (("Slightly (v2, 10%)", v2), ("Moderately (v3, 20%)", v3)):
        print(f"{name:<22}{d['target']:>6}%{d['eff_mean']:>9.2f}%{d['eff_min']:>8.2f}%"
              f"{d['eff_max']:>8.2f}%{d['elig_mean']:>11.1f}{d['appl_mean']:>11.1f}"
              f"{d['appl_sum']:>10}")
    print()

    # ---- 3. Applied-operator composition ---------------------------------
    operators = [
        ("omit_method", "Omit method"),
        ("omit_field", "Omit field"),
        ("omit_relation", "Omit relation"),
        ("reverse_relation", "Reverse relation"),
        ("omit_type", "Omit type"),
    ]

    def composition(prefix):
        out = {}
        for col, _ in operators:
            out[col] = sum(to_int(r[f"{prefix}_applied_{col}"]) for r in repos)
        out["_total"] = sum(out[c] for c, _ in operators)
        return out

    c2 = composition("v2")
    c3 = composition("v3")

    print("-" * 72)
    print("3. APPLIED-OPERATOR COMPOSITION (totals across %d repos)" % n)
    print("-" * 72)
    print(f"{'Operator':<18}{'v2 count':>10}{'v2 %':>8}{'v3 count':>10}{'v3 %':>8}")
    for col, label in operators:
        p2 = 100 * c2[col] / c2["_total"] if c2["_total"] else 0
        p3 = 100 * c3[col] / c3["_total"] if c3["_total"] else 0
        print(f"{label:<18}{c2[col]:>10}{p2:>7.1f}%{c3[col]:>10}{p3:>7.1f}%")
    print(f"{'TOTAL':<18}{c2['_total']:>10}{'100.0%':>8}{c3['_total']:>10}{'100.0%':>8}")
    print()
    print("Note: v2 (slightly) applies neither omit_relation nor omit_type,")
    print("      matching the methodology's degradation strategy.")
    print("=" * 72)
    return 0


if __name__ == "__main__":
    default_csv = os.path.join(os.path.dirname(os.path.abspath(__file__)), "eitri.csv")
    csv_path = sys.argv[1] if len(sys.argv) > 1 else default_csv
    sys.exit(main(csv_path))
