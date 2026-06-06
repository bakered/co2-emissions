# Create the main figure with subplots
newfig = make_subplots(rows=2, cols=1, shared_xaxes=False)

# Append traces from the frames of 'fig' to the first subplot
for frame in fig.frames:
    for trace in frame.data:
        # Append the trace to the first subplot
        new_trace = go.Scatter(
            x=copy.deepcopy(trace['x']),
            y=copy.deepcopy(trace['y']),
            mode=copy.deepcopy(trace['mode']) if 'mode' in trace else 'lines',
            marker=copy.deepcopy(trace['marker']) if 'marker' in trace else dict(),
            text=copy.deepcopy(trace['text']) if 'text' in trace else None,
            hovertemplate=copy.deepcopy(trace['hovertemplate']) if 'hovertemplate' in trace else None,
            hovertext=copy.deepcopy(trace['hovertext']) if 'hovertext' in trace else None,
            name=copy.deepcopy(trace['name']),
            showlegend=copy.deepcopy(trace['showlegend']) if 'showlegend' in trace else True,
            visible=False
        )
        newfig.append_trace(new_trace, row=1, col=1)



for trace in figRace.frames[5].data:
  print(trace)

# Append traces from the frames of 'figRace' (bar plots) to the second subplot
for frame in figRace.frames:
    for trace in frame.data:
        new_trace = go.Bar(
            x=copy.deepcopy(trace['x']),
            y=copy.deepcopy(trace['y']),
            marker=copy.deepcopy(trace['marker']) if 'marker' in trace else dict(),
            text=copy.deepcopy(trace['text']) if 'text' in trace else None,
            orientation=copy.deepcopy(trace['orientation']) if 'orientation' in trace else 'h',
            hovertemplate=copy.deepcopy(trace['hovertemplate']) if 'hovertemplate' in trace else None,
            showlegend=copy.deepcopy(trace['showlegend']) if 'showlegend' in trace else True,
            visible=False,
            name=copy.deepcopy(trace['name'])
        )
        newfig.append_trace(new_trace, row=2, col=1)


# Initially, show the first trace of each subplot
newfig.data[0].visible = True
newfig.data[len(fig.frames[0].data)].visible = True  # The first trace of the second subplot

# Create and add sliders
steps = []
for i, frame in enumerate(fig.frames):
    step = dict(
        method="restyle",
        args=["visible", [False] * len(newfig.data)],
        label=frame.name  # Use the frame's name (year) as the slider label
    )
    # Show the i-th trace of the first subplot and the i-th trace of the second subplot
    step["args"][1][i] = True
    step["args"][1][i + len(fig.frames[0].data)] = True
    steps.append(step)

sliders = [dict(
    active=0,
    currentvalue={"prefix": "Year: "},
    pad={"t": 50},
    steps=steps
)]

newfig.update_layout(sliders=sliders)

# Update layout to ensure the plot looks good
newfig.update_layout(
    height=600,
    width=800,
    title_text="Subplots with Independent Axes and Slider Control"
)

newfig.show()


figRace.show()


### below is code for race
if show_race:
        # Function to keep only the top x geographies by CO2 emissions for each year
        n_bars = 6
        plot_dfRace = plot_df.groupby('year').apply(lambda x: x.nlargest(n_bars, race_var)).reset_index(drop=True)
        
        # Sort the data by year and CO2 emissions in descending order
        plot_dfRace = plot_dfRace.sort_values(by=['year', race_var], ascending=[True, False])
    
        # Create a horizontal bar chart race
        figRace = px.bar(
            plot_dfRace,
            x=race_var,
            y='geography',
            text='geography',
            color='geography',
            color_discrete_map=color_map,
            orientation='h',
            animation_frame='year',
            #range_x=[0, plot_dfRace[x_var].max() * 1.1],  # Set x-axis range with some padding
            title="Accumalitve CO2 Emissions since 1970",
        )
        
        # Update layout to move text to the right of the bars
        figRace.update_traces(
            textposition='outside',  # Move text outside to the right of the bars
            #insidetextanchor='start'  # Align the text to the start of the bar (left side)
        )
        
        # Adjust layout for better appearance
        figRace.update_layout(
            template='plotly_white',
            xaxis_title=race_var,
            yaxis_title="Geography",
            showlegend=False,  # Hide legend since text labels are used
            yaxis=dict(
                categoryorder='total ascending'  # Ensure bars are ordered from top to bottom
                ),
            )
        
        # Update each frame to manage the y-axis dynamically
        frames = figRace.frames
        for frame in frames:
            year = frame.name
            year_data = plot_dfRace[plot_dfRace['year'] == int(year)]
            top_geographies = year_data['geography'].tolist()
            frame.layout.yaxis.update(
                categoryarray=top_geographies  # Update y-axis to only show top geographies
            )
        figRace.frames = frames
        
        newfig = make_subplots(
            rows=2, cols=1,  # Two rows, one column
            shared_xaxes=False,  # Do not share x-axis
            vertical_spacing=0.2,  # Spacing between the plots
            subplot_titles=("", "")  # Titles for subplots
            )
        # Add scatter plot traces to the first subplot
        for trace in fig['data']:
          print(type(trace))
          newfig.add_trace(go.Scatter(trace), row=1, col=1)
        
        # Add line plot traces to the second subplot
        for trace in figRace['data']:
          print(type(trace))
          newfig.add_trace(go.Bar(trace), row=2, col=1)
 
        # grab frames from both figures and Change 'xaxis' and 'yaxis' references
        figframeslist = []
        for frame in fig.frames:
            new_frame_data = [copy.deepcopy(trace) for trace in frame.data]
            for trace in new_frame_data:
                trace.xaxis = 'x1'
                trace.yaxis = 'y1'
            figframeslist.append(go.Frame(data=new_frame_data, name=frame.name))
        
        figRaceframeslist = []
        for frame in figRace.frames:
            new_frame_data = [copy.deepcopy(trace) for trace in frame.data]
            for trace in new_frame_data:
                trace['xaxis'] = 'x2'
                trace['yaxis'] = 'y2'
            frame_layout = go.Layout(xaxis2={'range': [0, None]})
            #put together
            figRaceframeslist.append(go.Frame(data=new_frame_data, name=frame.name, layout=frame_layout))
            
            # too jittery to set laout in frames - dont do it!
            #layout
            #year = frame.name
            #year_data = plot_df[plot_df['year'] == int(year)]
            #x_max_frame = year_data[race_var].max() *1.1
            #custom_xaxis = {'range': [0, x_max_frame]}
            #custom_yaxis = {'categoryorder': 'total ascending'} #do i need to add category array thing?
            # Create a layout for this specific frame
            #frame_layout = go.Layout(xaxis2=custom_xaxis) 
            
        
        newfig.frames = [
          go.Frame(data=fr1.data + fr2.data, layout=fr2.layout, name=fr2.name)
          for fr1, fr2 in zip(figframeslist, figRaceframeslist)
          ]

   
        # Update layout to preserve the original fig layout and integrate the new subplot
        newfig.update_layout(
          autosize=True,
          title=fig.layout.title,  # Preserve original title
          showlegend=fig.layout.showlegend,  # Preserve original legend settings
          template = "plotly_white",
          xaxis=dict(range=[min_x, max_x], title=fig.layout.xaxis.title),
          yaxis=dict(range=[min_y, max_y],anchor='x1', title=fig.layout.yaxis.title),
          xaxis2=dict(range=[None, None], anchor='y2', autorange=True, title=figRace.layout.xaxis.title),
          yaxis2=dict(anchor='x2', categoryorder='total ascending', autorange=True, title=figRace.layout.yaxis.title)
          )
        #newfig.update_xaxes(range=[None, None], row=1, col=1)

        if x_log:
          fig.update_xaxes(type="log", row=1, col=1)
        if y_log:
          fig.update_yaxes(type='log', row=1, col=1)
          
        # Explicitly set images for the top subplot (row=1)
        newfig.update_layout(
            images=[{
                'source': img['source'],
                'xref': "x1",  # Reference to the x-axis of the top subplot
                'yref': "y1",  # Reference to the y-axis of the top subplot
                'x': img['x'],
                'y': img['y'],
                'sizex': img['sizex'],
                'sizey': img['sizey'],
                'xanchor': img['xanchor'],
                'yanchor': img['yanchor'],
                'sizing': img['sizing'],
                'opacity': img['opacity'],
                'layer': img['layer']
            } for img in fig.layout.images]  # Use images from figScatter
        )
        
        #fig=newfig
