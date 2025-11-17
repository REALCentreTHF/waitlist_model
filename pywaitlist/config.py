"""Model configuration and defaults."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Tuple


@dataclass(frozen=True)
class ModelConfig:
    """Holds tunable parameters for the waitlist simulation."""

    sim_time: int = 44
    jitter_factor: float = 0.0
    drop_off_quantile: float = 0.75
    policy_quantile: float = 0.5
    breach_month: int = 4
    referral_growth: float = (1.015) ** (1 / 12)
    capacity_growth: float = (1.0685) ** (1 / 12)
    specs: Tuple[str, ...] = ("C_999",)
    breach_limit_months: int = 4
    random_seed: int | None = None
    output_start_year: int = 2024
    output_start_month: int = 9
    output_start_day: int = 1

    def as_dict(self) -> dict:
        """Expose the configuration as a serialisable dictionary."""

        return {
            "sim_time": self.sim_time,
            "jitter_factor": self.jitter_factor,
            "drop_off_quantile": self.drop_off_quantile,
            "policy_quantile": self.policy_quantile,
            "breach_month": self.breach_month,
            "referral_growth": self.referral_growth,
            "capacity_growth": self.capacity_growth,
            "specs": self.specs,
            "breach_limit_months": self.breach_limit_months,
            "random_seed": self.random_seed,
            "output_start": (
                self.output_start_year,
                self.output_start_month,
                self.output_start_day,
            ),
        }


DEFAULT_CONFIG = ModelConfig()
