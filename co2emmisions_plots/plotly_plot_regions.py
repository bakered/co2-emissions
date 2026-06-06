import sys
import os
import time

sys.path.insert(0, os.path.dirname(__file__))
from plot_engine import createCountryBubbleGraph


class Progress:
    """Dummy progress reporter so createCountryBubbleGraph runs outside Shiny."""
    def set(self, value, message=""):
        print(f"  [{value}%] {message}")


OUTPUT_DIR = os.path.join(os.path.dirname(__file__), 'outputs')
os.makedirs(OUTPUT_DIR, exist_ok=True)

createCountryBubbleGraph(
    datasource    = "GCP and Maddison",
    geographyLevel= "regions",
    geography_list= [
        "Africa",
        "Developed",
        "Developing Asia and Oceania",
        "Latin America and the Caribbean",
    ],
    x_var         = "gdp_per_capita",
    y_var         = "co2_per_capita",
    size_var      = "co2",
    start_year    = 1950,
    smoothness    = 2,
    leave_trace   = True,
    fixed_axes    = True,
    show_flags    = True,
    x_log         = True,
    y_log         = False,
    size_log      = True,
    use_loess     = True,
    length        = 10,
    fps           = 2,
    download      = "html",
    include_plotlyjs = "cdn",
    filename      = os.path.join(OUTPUT_DIR, "regions.html"),
    progress      = Progress(),
    start_time    = time.time(),
)

print(f"\nSaved: {os.path.join(OUTPUT_DIR, 'regions.html')}")
