"""Simulation utilities for the Python waitlist model (pure Python version)."""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from typing import Dict, Iterable, List, Sequence, Tuple

from .config import ModelConfig
from .data import _parse_float, _parse_int

Row = Dict[str, float | int | str | None]


@dataclass
class CapacityPlan:
    schedule: List[Dict[str, float | int | str]]
    lookup: Dict[Tuple[int, str], float]


def create_referrals(
    min_x: int,
    max_x: int,
    specialties: Iterable[str],
    growth: float,
    starting_value: float,
    jitter_factor: float,
) -> Dict[Tuple[int, str], float]:
    """Replicate the R `CreateReferrals` helper using pure Python."""

    referrals: Dict[Tuple[int, str], float] = {}
    for s in specialties:
        for offset in range(min_x, max_x + 1):
            value = starting_value * (growth ** offset)
            referrals[(-offset, s)] = referrals.get((-offset, s), 0.0) + value
    return referrals


def prepare_referrals(
    df_data: Sequence[Row],
    config: ModelConfig,
    starting_average: float,
) -> Dict[Tuple[int, str], float]:
    """Build the initial waitlist/referral frame used by the simulator."""

    initial: Dict[Tuple[int, str], float] = defaultdict(float)
    for row in df_data:
        if _parse_int(row.get("t")) == 35:
            key = (_parse_int(row.get("i")), str(row.get("s")))
            open_val = _parse_float(row.get("open"))
            if open_val is not None:
                initial[key] += open_val

    future = create_referrals(
        min_x=1,
        max_x=config.sim_time,
        specialties=config.specs,
        growth=config.referral_growth,
        starting_value=starting_average,
        jitter_factor=config.jitter_factor,
    )
    for key, value in future.items():
        initial[key] += value

    return dict(initial)


def create_capacity(
    sim_time: int,
    specialties: Iterable[str],
    growth: float,
    base_capacity: Dict[str, float],
) -> CapacityPlan:
    """Generate the capacity schedule for each specialty."""

    schedule: List[Dict[str, float | int | str]] = []
    lookup: Dict[Tuple[int, str], float] = {}
    for t in range(1, sim_time + 1):
        for s in specialties:
            base = base_capacity.get(s)
            if base is None:
                raise KeyError(f"No base capacity estimate available for specialty {s}.")
            cap = base * (growth ** t)
            schedule.append({"t": t, "s": s, "cap": cap})
            lookup[(t, s)] = cap
    return CapacityPlan(schedule=schedule, lookup=lookup)


def wait_list(
    sim_time: int,
    referrals: Dict[Tuple[int, str], float],
    capacity: CapacityPlan,
    df_a: Dict[Tuple[int, str], float],
    df_c: Dict[Tuple[int, str], float],
) -> List[Dict[str, float | int | str]]:
    """Simulate the waitlist evolution month-by-month."""

    state = dict(referrals)
    outputs: List[Dict[str, float | int | str]] = []

    for month in range(1, sim_time + 1):
        capped: Dict[Tuple[int, str], float] = defaultdict(float)
        for (i, s), z in state.items():
            capped[(min(i, 24), s)] += z
        state = dict(capped)

        z_sum: Dict[str, float] = defaultdict(float)
        z_c: Dict[Tuple[int, str], float] = {}
        for (i, s), z in state.items():
            if i >= 0:
                c_val = df_c.get((i, s), 0.0)
                value = z * c_val
                z_c[(i, s)] = value
                z_sum[s] += value

        next_state: Dict[Tuple[int, str], float] = defaultdict(float)
        for (i, s), z in state.items():
            a_val = df_a.get((i, s), 1.0)
            c_val = df_c.get((i, s), 0.0)
            cap = capacity.lookup.get((month, s), 0.0)
            new_z = z
            if i >= 0:
                total = z_sum.get(s, 0.0)
                deduction = (z * c_val * cap / total) if total > 0 else 0.0
                new_z = a_val * (z - deduction)
            new_i = i + 1
            next_state[(new_i, s)] += new_z
            if new_i >= 0:
                outputs.append({"t": month, "i": new_i, "s": s, "z": new_z})
        state = dict(next_state)

    return outputs


def create_data(
    referrals: Dict[Tuple[int, str], float],
    df_a: Dict[Tuple[int, str], float],
    df_c: Dict[Tuple[int, str], float],
    capacity: CapacityPlan,
    breach_limit: int,
    sim_time: int,
) -> Dict[str, List[Dict[str, float | int | str]]]:
    """Run the simulator and return breaches, capacity, and the full time series."""

    full_data = wait_list(
        sim_time=sim_time,
        referrals=referrals,
        capacity=capacity,
        df_a=df_a,
        df_c=df_c,
    )

    breaches_map: Dict[int, Dict[str, float]] = defaultdict(lambda: {"breach": 0.0, "not_breach": 0.0})
    for row in full_data:
        month = int(row["t"])
        bucket = "breach" if int(row["i"]) > breach_limit else "not_breach"
        breaches_map[month][bucket] += float(row["z"])

    breaches: List[Dict[str, float | int]] = []
    for month in sorted(breaches_map):
        breach = breaches_map[month]["breach"]
        not_breach = breaches_map[month]["not_breach"]
        breaches.append({
            "t": month,
            "breach": breach,
            "not_breach": not_breach,
            "tot": breach + not_breach,
        })

    return {
        "breaches": breaches,
        "capacity": capacity.schedule,
        "full_data": full_data,
    }
