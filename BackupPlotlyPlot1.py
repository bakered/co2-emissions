#. %reset -f

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import numpy as np

# Import the data
data = pd.read_csv('dataPlot1.csv')
data.loc[data['ISO3'] == "NAM", 'ISO2'] = "NA"
data['year'] = data['year'].astype(int)
# create image link from ISO2
data['image_link'] = data['ISO2'].apply(lambda iso: f"https://hatscripts.github.io/circle-flags/flags/{iso.lower()}.svg")
data['geography'] = data['ISO3']


### funtionalise
leave_trace = True
fixed_axes = True
x_var = 'gdp'
y_var = 'co2_per_capita'
size_var = 'co2'
max_bubble_size_wanted = 3000000

geography_list = [
    'ARG', 'AUS', 'BRA', 'CAN', 'CHN', 'FRA', 'DEU', 'IND', 'IDN', 
    'ITA', 'JPN', 'MEX', 'RUS', 'SAU', 'ZAF', 'KOR', 'TUR', 'GBR', 'USA'
]


# Filter the DataFrame based on the list of ISO3 codes
plot_df = data[data['geography'].isin(geography_list)]

max_x = plot_df[x_var].max() * 1.1
max_y = plot_df[y_var].max() * 1.1
max_bubble_size_wanted = max_x/7.5
max_x = plot_df[x_var].max() + max_bubble_size_wanted

# Add normalized_size column such that the value is the diameter making 
plot_df['normalized_size'] = 2*np.sqrt(plot_df[size_var]/3.141592653589793) 
co2_max = plot_df['normalized_size'].max()
# scale column such that the co2_max will be the size of the bubble you want measured in x-axis units. 
# (warning: i think only works if x-axis is larger than y-axis)
plot_df['normalized_size'] =  plot_df['normalized_size'] * (max_bubble_size_wanted / co2_max) # 3000000

geographies = plot_df['geography'].unique()

# Create the base plot with Plotly Express
figScatter = px.scatter(
    plot_df, 
    x=x_var, 
    y=y_var, 
    size=size_var, 
    hover_name='geography', 
    animation_frame='year', 
    size_max=60,
    template="plotly_white"
)
figScatter.update_traces(marker=dict(opacity=0))  # Set opacity to 0 for invisibility

if leave_trace:
  #  Loop over each year in the DataFrame
  expanded_rows = pd.DataFrame()
  for year in plot_df['year'].unique(): # year=plot_df['year'].unique()[12]
    # Filter the rows for the current year and all previous years
    filtered_rows = plot_df[plot_df['year'].astype(int) <= int(year)].copy()
    filtered_rows.loc[:, 'year'] = year
    # Append these rows to the list
    expanded_rows = pd.concat([expanded_rows, filtered_rows], axis=0, ignore_index=True)

  # Create an animated line plot
  figLine = px.line(
      expanded_rows, 
      x=x_var, 
      y=y_var, 
      color='geography',
      #line_group='geography',  # Group by geography to draw a separate line for each
      hover_name='geography', 
      animation_frame='year', 
      template="plotly_white"
      )
  figLine.update_layout(
    legend=dict(
      visible=False
      )
      )
  #fig.update_layout(
  #  xaxis=dict(range=[0, plot_df[x_var].max() * 1.1]),  # Adjust range as needed
  #  yaxis=dict(range=[0, plot_df[y_var].max() * 1.1]),  # Adjust range as needed
  #)


  # now integrate, traces, frames and layout
  fig = go.Figure(
    data=figLine.data + figScatter.data,
    frames=[
        go.Frame(data=fr1.data + fr2.data, name=fr2.name)
        for fr1, fr2 in zip(figLine.frames, figScatter.frames)
    ],
    layout=figScatter.layout,
  )
  fig.update_layout(
    legend=dict(
      visible=False
      )
      )
else:
  fig = figScatter


# Add flags to each frame
for frame in fig.frames: #frame = fig.frames[19]
  #print(frame.layout.images)
  year = frame.name
  #print(year)
  # Filter data for the specific year
  #if leave_trace:
  #  year_data = plot_df[plot_df['year'] <= int(year)]
  #  ## add in NA rows for missing data
  #  full_years = list(range(1970, int(year)+1))
  #  full_index = pd.MultiIndex.from_product([countries, full_years], names=['geography', 'year'])
  #  year_data = year_data.set_index(['geography', 'year']).reindex(full_index).reset_index()
  #else:
  year_data = plot_df[plot_df['year'] == int(year)]
  ## add in NA rows for missing data
  full_index = pd.Index(geographies, name='geography')
  year_data = year_data.set_index('geography').reindex(full_index).reset_index()
    
  
  
  # Create list of image annotations for this year
  image_annotations = []
  for i, row in year_data.iterrows():
    if pd.isna(row['image_link']):
      image_annotations.append(
            {
              'source': "https://hatscripts.github.io/circle-flags/flags/gb.svg",
              'xref': "x",
              'yref': "y",
              'x': 0,
              'y': 0,
              'sizex': 0,
              'sizey': 0,
              'xanchor': "center",
              'yanchor': "middle",
              'sizing': "contain",
              'opacity': 0.8,
              'layer': "above"
              }
            )
    else:
      image_annotations.append(
        {
                'source': row['image_link'],
                'xref': "x",
                'yref': "y",
                'x': row[x_var],
                'y': row[y_var],
                'sizex': row['normalized_size'],
                'sizey': row['normalized_size'],
                'xanchor': "center",
                'yanchor': "middle",
                'sizing': "contain",
                'opacity': 0.8,
                'layer': "above"
                }
            )
  # Update the layout of the frame to include the images
  frame.layout.images = image_annotations
  #print(frame.layout.images)
  x_max = year_data[x_var].max() + max_bubble_size_wanted
  y_max = year_data[y_var].max()  * 1.1
  custom_xaxis = {'range': [0, x_max]}
  custom_yaxis = {'range': [0, y_max]}
  # Assign the custom axes to the frame's layout
  # hashed out because makes plot jittery
  # frame.layout.xaxis = custom_xaxis
  # frame.layout.yaxis = custom_yaxis
  

# Adjust layout for better appearance
fig.update_layout(
    xaxis_title=x_var,
    yaxis_title=y_var,
    title=""
)

if fixed_axes:
  fig.update_layout(
    xaxis=dict(range=[0, max_x]),
    yaxis=dict(range=[0, max_y])
    )

# Update layout to not autoplay
fig.update_layout(
    updatemenus=[{
        'buttons': [
            {
                'args': [None, {'frame': {'duration': 500, 'redraw': True}, 'fromcurrent': True, 'mode': 'immediate'}],
                'label': 'Play',
                'method': 'animate'
            },
            {
                'args': [[None], {'frame': {'duration': 0, 'redraw': True}, 'mode': 'immediate', 'transition': {'duration': 0}}],
                'label': 'Pause',
                'method': 'animate'
            }
        ],
        'direction': 'left',
        'pad': {'r': 10, 't': 87},
        'showactive': True,
        'type': 'buttons',
        'x': 0.1,
        'xanchor': 'right',
        'y': 0,
        'yanchor': 'top'
    }],
    sliders=[{
        'active': 0,
        'steps': [
            {
                'label': str(year),
                'method': 'animate',
                'args': [[str(year)], {'mode': 'immediate', 'frame': {'duration': 500, 'redraw': True}, 'transition': {'duration': 0}}],
            } for year in sorted(plot_df['year'].unique())
        ]
    }]
)

# Save the figure as HTML
fig.write_html('plotly_animation.html')
