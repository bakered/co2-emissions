import sys
import os
import time
import pandas as pd

sys.path.insert(0, os.path.dirname(__file__))
from plot_engine import createCountryBubbleGraph


class Progress:
    def set(self, value, message=""):
        print(f"  [{value}%] {message}")


OUTPUT_DIR = os.path.join(os.path.dirname(__file__), 'outputs')
os.makedirs(OUTPUT_DIR, exist_ok=True)

START_YEAR = 1950
SMOOTHNESS = 3    # sub-frames between years — more frames = smoother image movement
LENGTH     = 20   # seconds total

# Get end year from data so frame_duration is exact
_df = pd.read_csv(os.path.join(os.path.dirname(__file__), 'dataRegions.csv'))
END_YEAR = int(_df['year'].max())
del _df

total_frames  = (END_YEAR - START_YEAR) * SMOOTHNESS
frame_duration = round(1000 * LENGTH / total_frames)   # ms per frame

print(f"Frames: {total_frames}  |  Duration per frame: {frame_duration} ms  |  End year: {END_YEAR}")

# ── Generate the figure (no file write yet) ──────────────────────────────────
fig = createCountryBubbleGraph(
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
    start_year    = START_YEAR,
    smoothness    = SMOOTHNESS,
    leave_trace   = True,
    fixed_axes    = True,
    show_flags    = True,
    x_log         = True,
    y_log         = False,
    size_log      = True,
    use_loess     = True,
    length        = LENGTH,
    fps           = 2,
    download      = "nothing",  # return fig; we write HTML ourselves below
    progress      = Progress(),
    start_time    = time.time(),
)

# ── Aesthetic improvements ────────────────────────────────────────────────────

# Title
fig.update_layout(
    title=dict(
        text=(
            "<span style='font-size:22px;'>"
            "<b>The glaring inequality of income and CO₂ emissions</b>"
            "</span>"
            "<br>"
            "<span style='font-size:14px; color:#666;'>"
            "CO₂ per capita vs GDP per capita &nbsp;|  "
            "Bubble size = total annual CO₂ emissions"
            "</span>"
        ),
        x=0.04,
        y=0.97,
        xanchor='left',
        yanchor='top',
    ),
)

# Background & grid
fig.update_layout(
    plot_bgcolor  = '#F7F8FA',
    paper_bgcolor = '#FFFFFF',
)

fig.update_xaxes(
    gridcolor     = '#E2E4E8',
    gridwidth     = 1,
    showgrid      = True,
    zeroline      = False,
    tickfont      = dict(size=11, color='#555'),
    tickformat    = '~s',
)

fig.update_yaxes(
    gridcolor     = '#E2E4E8',
    gridwidth     = 1,
    showgrid      = True,
    zeroline      = False,
    tickfont      = dict(size=11, color='#555'),
)

# Legend: cleaner, slight transparency so it doesn't clash with bubbles
fig.update_layout(
    legend=dict(
        bgcolor      = 'rgba(255,255,255,0.85)',
        bordercolor  = '#D0D3DA',
        borderwidth  = 1,
        font         = dict(size=13),
        itemsizing   = 'constant',
    ),
)

# Margins: more breathing room at top for title
fig.update_layout(
    margin=dict(l=55, r=30, t=90, b=55),
)

# Data source footnote — fixed to paper coords so it doesn't animate
fig.add_annotation(
    text=(
        "Sources: Global Carbon Project (2024), Maddison Project Database (2023)"
    ),
    xref='paper', yref='paper',
    x=0.01, y=-0.04,
    showarrow=False,
    font=dict(size=10, color='#999', family='Inter, Helvetica Neue, Arial, sans-serif'),
    align='left',
    xanchor='left',
)

# ── Build slider steps at integer years only.
#    Auto-play runs through ALL sub-frames (smooth); clicking the slider
#    jumps to the nearest whole year.
slider_steps = []
for frame in fig.frames:
    year_float = float(frame.name)
    if year_float != int(year_float):   # skip sub-frames
        continue
    year_int   = int(year_float)
    display_label = str(year_int) if year_int % 10 == 0 else ''
    slider_steps.append({
        'args': [[frame.name], {
            'frame'     : {'duration': frame_duration, 'redraw': True},
            'mode'      : 'immediate',
            'fromcurrent': True,
            'transition': {'duration': 0},
        }],
        'label' : display_label,
        'method': 'animate',
    })

import plotly.graph_objects as go

go_steps = tuple(
    go.layout.slider.Step(
        args=s['args'],
        label=s['label'],
        method=s['method'],
    )
    for s in slider_steps
)
# Direct assignment replaces the step list; update_layout merges it
fig.layout.sliders[0].steps = go_steps
fig.layout.sliders[0].currentvalue.visible = False
fig.layout.sliders[0].ticklen   = 4
fig.layout.sliders[0].tickcolor = '#aaa'
fig.layout.sliders[0].font      = {'size': 11, 'color': '#666'}

# ── Write HTML ────────────────────────────────────────────────────────────────
output_file    = os.path.join(OUTPUT_DIR, 'regions_v2.html')
animation_opts = {
    'frame'     : {'duration': frame_duration, 'redraw': True},
    'transition': {'duration': 0},
}

fig.update_traces(hoverinfo='skip', hovertemplate=None)
fig.update_layout(hovermode=False)

fig.write_html(
    output_file,
    include_plotlyjs = 'cdn',
    full_html        = True,
    auto_play        = True,
    default_width    = '95vw',
    default_height   = '95vh',
    div_id           = 'id_plot-container',
    animation_opts   = animation_opts,
    config           = {'displayModeBar': False},
)

# ── Post-process: inject Google Font so Inter renders reliably ────────────────
with open(output_file, 'r', encoding='utf-8') as f:
    html = f.read()

google_font = (
    '<link rel="preconnect" href="https://fonts.googleapis.com">\n'
    '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>\n'
    '<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap" rel="stylesheet">\n'
    '<style>body, .plotly { font-family: Inter, "Helvetica Neue", Helvetica, Arial, sans-serif !important; }</style>\n'
)
html = html.replace('<head>', '<head>\n' + google_font, 1)

with open(output_file, 'w', encoding='utf-8') as f:
    f.write(html)

size_kb = os.path.getsize(output_file) // 1024
print(f"\nSaved: {output_file}  ({size_kb} KB)")
