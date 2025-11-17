"""Python entry point for the elective waitlist model (pure Python)."""

from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import List

from pywaitlist import (
    DEFAULT_CONFIG,
    compute_base_capacity,
    compute_drop_off_tables,
    compute_starting_average,
    create_capacity,
    create_data,
    load_budget_run,
    prepare_referrals,
)


def ensure_output_dir(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


def write_csv(path: Path, fieldnames: List[str], rows: List[dict]) -> None:
    ensure_output_dir(path)
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def main() -> None:
    config = DEFAULT_CONFIG
    df1, df2 = load_budget_run()
    base_capacity = compute_base_capacity(df1)
    starting_average = compute_starting_average(df1)
    df_a, df_c = compute_drop_off_tables(df2, config)
    referrals = prepare_referrals(df2, config, starting_average)

    capacity_plan = create_capacity(
        sim_time=config.sim_time,
        specialties=config.specs,
        growth=config.capacity_growth,
        base_capacity=base_capacity,
    )

    results = create_data(
        referrals=referrals,
        df_a=df_a,
        df_c=df_c,
        capacity=capacity_plan,
        breach_limit=config.breach_limit_months,
        sim_time=config.sim_time,
    )

    breaches = results["breaches"]
    waitlist = results["full_data"]

    write_csv(Path("output/python_breach_summary.csv"), ["t", "breach", "not_breach", "tot"], breaches)
    write_csv(Path("output/python_waitlist_projection.csv"), ["t", "i", "s", "z"], waitlist)

    final_month = max((row["t"] for row in waitlist), default=0)
    final_waiters = sum(float(row["z"]) for row in waitlist if row["t"] == final_month)
    final_breach_row = next((row for row in breaches if row["t"] == final_month), None)
    breach_ratio = 0.0
    if final_breach_row and final_breach_row["tot"]:
        breach_ratio = float(final_breach_row["breach"]) / float(final_breach_row["tot"])

    summary = {
        "config": config.as_dict(),
        "final_month": int(final_month),
        "final_waiters": final_waiters,
        "breach_ratio": breach_ratio,
    }
    summary_path = Path("output/python_summary.json")
    ensure_output_dir(summary_path)
    summary_path.write_text(json.dumps(summary, indent=2))

    print("Saved Python simulation outputs to the output/ directory.")


if __name__ == "__main__":
    main()
