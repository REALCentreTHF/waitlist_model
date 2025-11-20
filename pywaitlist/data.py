"""Data loading helpers for the Python waitlist model (pure Python version)."""

from __future__ import annotations

import csv
from collections import defaultdict
from pathlib import Path
from statistics import median
from typing import Dict, Iterable, List, Sequence, Tuple

from .config import ModelConfig

Row = Dict[str, float | int | str | None]


def _parse_float(value: str | None) -> float | None:
    if value in (None, "", "NA"):
        return None
    return float(value)


def _parse_int(value: str | None) -> int:
    if value in (None, "", "NA"):
        return 0
    return int(float(value))


def load_budget_run(directory: str = "const/budget_nov_run") -> Tuple[List[Row], List[Row]]:
    """Load the historical RTT data prepared for the November budget run."""

    base = Path(directory)
    df1_path = base / "df_1.csv"
    df2_path = base / "df_2.csv"

    def load(path: Path) -> List[Row]:
        with path.open("r", newline="") as handle:
            reader = csv.DictReader(handle)
            rows: List[Row] = []
            for row in reader:
                rows.append({k.lower(): v for k, v in row.items()})
            return rows

    return load(df1_path), load(df2_path)


def compute_base_capacity(df1: Sequence[Row]) -> Dict[str, float]:
    """Estimate baseline capacity using the most recent 12 months of activity."""

    totals: Dict[Tuple[int, str], float] = defaultdict(float)
    for row in df1:
        t = _parse_int(row.get("t"))
        s = str(row.get("s"))
        completed = _parse_float(row.get("completed"))
        if completed is None:
            continue
        totals[(t, s)] += completed

    if not totals:
        raise ValueError("No completed activity in df_1.csv")

    max_t = max(t for (t, _) in totals)
    per_spec: Dict[str, List[float]] = defaultdict(list)
    for (t, s), value in totals.items():
        if t >= max_t - 12:
            per_spec[s].append(value)

    return {s: median(values) for s, values in per_spec.items() if values}


def compute_starting_average(df1: Sequence[Row]) -> float:
    """Median referral inflow in the year preceding the latest observation."""

    records = [
        (_parse_int(row.get("t")), _parse_float(row.get("new")))
        for row in df1
        if _parse_float(row.get("new")) not in (None, 0.0)
    ]
    records = [(t, val) for t, val in records if val is not None]
    if not records:
        raise ValueError("Referral stream is empty; cannot compute starting average.")
    records.sort(key=lambda item: item[0])
    max_t = records[-1][0]
    window = [val for t, val in records if t <= max_t - 12]
    if not window:
        window = [val for _, val in records]
    return median(window)


def compute_drop_off_tables(
    df_data: Sequence[Row],
    config: ModelConfig,
) -> Tuple[Dict[Tuple[int, str], float], Dict[Tuple[int, str], float]]:
    """Return the drop-off (a) and treatment policy (c) lookup tables."""

    from math import ceil, floor

    def quantile(values: Sequence[float], q: float) -> float | None:
        if not values:
            return None
        sorted_vals = sorted(values)
        pos = (len(sorted_vals) - 1) * q
        lower = int(floor(pos))
        upper = int(min(len(sorted_vals) - 1, ceil(pos)))
        if lower == upper:
            return sorted_vals[lower]
        fraction = pos - lower
        return sorted_vals[lower] + (sorted_vals[upper] - sorted_vals[lower]) * fraction

    a_values: Dict[Tuple[int, str], List[float]] = defaultdict(list)
    c_values: Dict[Tuple[int, str], List[float]] = defaultdict(list)

    for row in df_data:
        i = _parse_int(row.get("i"))
        s = str(row.get("s"))
        a_val = _parse_float(row.get("a"))
        open_val = _parse_float(row.get("open"))
        completed_val = _parse_float(row.get("completed"))

        if a_val is not None:
            a_values[(i, s)].append(a_val)
        if open_val is not None and completed_val is not None and (open_val + completed_val) > 0:
            c_values[(i, s)].append(completed_val / (open_val + completed_val))

    a_lookup: Dict[Tuple[int, str], float] = {}
    for key, values in a_values.items():
        q_val = quantile(values, config.drop_off_quantile)
        if q_val is not None:
            a_lookup[key] = q_val
    # default drop-off rate where no history exists
    specs = {str(row.get("s")) for row in df_data}
    adjusted_lookup: Dict[Tuple[int, str], float] = {}
    for (i, s), value in a_lookup.items():
        adjusted_i = i - 1
        if adjusted_i >= 0:
            adjusted_lookup[(adjusted_i, s)] = value
    for s in specs:
        adjusted_lookup[(26, s)] = 0.97

    c_lookup: Dict[Tuple[int, str], float] = {}
    for key, values in c_values.items():
        q_val = quantile(values, config.policy_quantile)
        if q_val is not None:
            c_lookup[key] = q_val

    return adjusted_lookup, c_lookup
