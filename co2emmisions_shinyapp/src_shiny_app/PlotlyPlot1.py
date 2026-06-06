#. %reset -f

import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import numpy as np
from pathlib import Path
#from plotly.subplots import make_subplots
#import copy
import time
import math
import statsmodels.api as sm
import moviepy.editor as mpy
import io
from PIL import Image
import cv2

from guppy import hpy

# Create a heap object to track memory usage
hp = hpy()

def weighted_percentile(values, weights, percentile): #values = plot_df[y_var][index_max_y]; weights=plot_df['pop'][index_max_y]; percentile=95
    """Compute the weighted percentile of a given list of values."""
    # Convert values and weights to numpy arrays
    values = np.array(values)
    weights = np.array(weights)
    # Remove NaNs from both values and weights
    mask = ~np.isnan(values) & ~np.isnan(weights)
    values = values[mask]
    weights = weights[mask]
    sorted_indices = np.argsort(values)
    sorted_values = np.array(values)[sorted_indices]
    sorted_weights = np.array(weights)[sorted_indices]
    cumulative_weights = np.cumsum(sorted_weights)
    percentile_value = np.percentile(cumulative_weights, percentile)
    index = np.searchsorted(cumulative_weights, percentile_value)
    return sorted_values[index]

def createCountryBubbleGraph(datasource="GCP and Maddison",
                             geographyLevel='countries', 
                             x_var = 'gdp_per_capita', 
                             y_var = 'co2_per_capita', 
                             size_var = 'co2',
                             race_var = 'accumulated_co2',
                             smoothness = 2,
                             leave_trace = True, 
                             fixed_axes = True,
                             bubble_similarity=0,
                             flag_size= 1,
                             bubble_size = 1,
                             rolling_mean_years=10,
                             start_year=1950,
                             geography_list = [
                                 'ARG', 'AUS', 'BRA', 'CAN', 'CHN', 'FRA', 'DEU', 'IND', 'IDN', 
                                 'ITA', 'JPN', 'MEX', 'RUS', 'SAU', 'ZAF', 'KOR', 'TUR', 'GBR', 'USA'
                             ],
                             x_log=True,
                             y_log=False,
                             size_log=True,
                             show_flags=True,
                             use_loess=True,
                             start_time=time.time(),
                             progress=None,
                             download="nothing",
                             filename="plotly_animation_test5.mp4",
                             width="default",
                             height="default",
                             fps=2,
                             length=10,
                             ):
    
    #geographyLevel='countries'; x_var='gdp_per_capita'; y_var='co2_per_capita'; size_var='co2'; race_var='accumulated_co2'; leave_trace=True; fixed_axes=True; flag_size=1; x_log=False; y_log=False; show_flags=False; start_year=1950
    
    
    
    ########## SET PARAMETERS
    region1s = ["Developed", 
                "Developing Asia and Oceania", 
                "Latin America and the Caribbean", 
                "Africa"]
    
    if False: #geographyLevel=='countries':
        color_map = {'Developed': '#009EDB', 'Developing': '#ED1847', 'LDCs': '#FFC800'}
        colour_var = 'region2'
    else:
        color_map = {'Developed': '#009EDB', 'Developing Asia and Oceania': '#ED1847', 'Latin America and the Caribbean': '#72BF44', 'Africa': '#FBAF17', 
                     'Developing Asia <br>and Oceania': '#ED1847', 'Latin America <br>and the Caribbean': '#72BF44',
                     'Developedtext': '#009EDB', 'Developing Asia and Oceaniatext': '#ED1847', 'Latin America and the Caribbeantext': '#72BF44', 'Africatext': '#FBAF17',
                     'Developing Asia <br>and Oceaniatext': '#ED1847', 'Latin America <br>and the Caribbeantext': '#72BF44',}
        
        
        # #B8505E  
        # asia dull: #a71f36 
        # Developed dull': '#005392'
        # 'Latin America and the Caribbean dull': '#006747'
        # 'Africa dull': '#b16d03', 
        
        
        colour_var = 'region1'
    
    if datasource == "GCP and Maddison":
        labels = {"co2_per_capita": "CO<sub>2</sub> per capita",
                 "gdp_per_capita": "GDP per capita",
                 "co2": "Yearly CO<sub>2</sub>",
                 "accumulated_co2": "Accumulated CO<sub>2</sub>", 
                 "pop": "Population", 
                 "gdp": "GDP",
                 }
        labels_parenthesis = {"co2_per_capita": "<br>(Tons)",
                 "gdp_per_capita": "<br>(2011 Dollars, PPP)",
                 "co2": "(Kilotons)",
                 "accumulated_co2": "<br>(Kilotons) (could be inconsistent!)", 
                 "pop": "", 
                 "gdp": "<br>(Millions)",
                 }
    else:
         labels = {"co2_per_capita": "CO<sub>2</sub> per capita",
                  "gdp_per_capita": "GDP per capita",
                  "co2": "Yearly CO<sub>2</sub>",
                  "accumulated_co2": "Accumulated CO<sub>2</sub>", 
                  "pop": "Population", 
                  "gdp": "GDP",
                  }
         labels_parenthesis = {"co2_per_capita": " (Tons)",
                  "gdp_per_capita": " (2021 Dollars, PPP)",
                  "co2": " (Kilotons)",
                  "accumulated_co2": " (Kilotons) (could be inconsistent!)", 
                  "pop": "", 
                  "gdp": " (Millions)",
                  }
                  
    
    if geographyLevel == "countries":
        text_positions = {
            'USA': 'top right',
            "ZAF": 'top left', 
            "EGY": 'top right', 
            "DZA": 'top right', 
            "USA": 'top right', 
            "RUS": 'top left', 
            "JPN": 'top right', 
            "CHN": 'top right', 
            "IND": 'top left', 
            "IRN": 'top right', 
            "BRA": 'bottom right', 
            "MEX": 'middle left', 
            "ARG": 'top left',
            "CAN": 'middle left',
            "AUS": 'top left',
            "DEU": 'middle right',
            "FRA": 'bottom right',
            "GBR": 'bottom left',
            "TUR": 'top center',
        }
    else:
        text_positions = {
            'Africa': 'middle right',
            'Developing Asia and Oceania': 'top center',
            'Developing Asia<br>and Oceania': 'top center',
            'Latin America and the Caribbean': 'middle right',
            'Latin America and<br>the Caribbean': 'middle right',
            'Developed': 'bottom center' #'top right' #
        } 
        
    if datasource == "GCP and Maddison":
        if start_year<1820:
            start_year = 1820
    else:
        if start_year<1990:
            start_year = 1990
    
    
    x_var_label = labels.get(x_var, x_var) 
    y_var_label = labels.get(y_var, y_var)
    
    
    ########## LOAD DATA
    now_time = time.time()
    print(f"start function time: {now_time - start_time} seconds")
    progress.set(2, "Loading data...")
    
    # Import the data
    #data = pd.read_csv("dataCountries.csv")
    #data = pd.read_csv(open_url('https://github.com/bakered/co2emmisions/blob/main/src_shiny_app/dataPlot1.csv'))
    if datasource == "GCP and Maddison":
        if geographyLevel == "countries":
            infile = Path(__file__).parent / "data" / "dataCountries.csv"
        else:
            infile = Path(__file__).parent / "data" / "dataRegions.csv"
            print(infile)
    else:
        if geographyLevel == "countries":
            infile = Path(__file__).parent / "data" / "dataWDICountries.csv"
        else:
            infile = Path(__file__).parent / "data" / "dataWDIRegions.csv"
    data = pd.read_csv(infile)
    data['region1'] = pd.Categorical(data['region1'], categories=region1s, ordered=True)
    data['year'] = data['year'].astype(int)
    
    end_year = max(data['year'])
    
    total_frames = (end_year - start_year)*smoothness
    frame_duration = 1000*length /  total_frames 
    
    now_time = time.time()
    print(f"loaded data time: {now_time - start_time} seconds")
    progress.set(3, "creating bubbles...")
    
      
    if geographyLevel == 'countries': 
        # create image link from ISO2
        data['ISO2']= data['ISO2'].astype(str)
        data['image_link'] = data['ISO2'].apply(lambda iso: f"https://hatscripts.github.io/circle-flags/flags/{iso.lower()}.svg")
        geography = 'ISO3'
        #geography = 'country'
        unique_data = data[['ISO3', 'country']].drop_duplicates()
        country_map = dict(zip(unique_data['ISO3'], unique_data['country']))
    else:
       # import urllib.parse
        geography = 'region1'
        region_to_image_link = {
            "Africa":  "https://raw.githubusercontent.com/bakered/co2emmisions/main/src_shiny_app/static/africa_map.png", #"https://storage.unctad.org/2024-co2emissions/assets/img/africa_map.png", # safe=''), #
            "Developed":   "https://raw.githubusercontent.com/bakered/co2emmisions/main/src_shiny_app/static/developed_map.png", # "https://storage.unctad.org/2024-co2emissions/assets/img/developed_map.png", # safe=''), #
            "Developing Asia and Oceania":  "https://raw.githubusercontent.com/bakered/co2emmisions/main/src_shiny_app/static/asia_and_oceania_map.png", # "https://storage.unctad.org/2024-co2emissions/assets/img/asia_and_oceania_map.png", # safe=''), #
            "Latin America and the Caribbean":  "https://raw.githubusercontent.com/bakered/co2emmisions/main/src_shiny_app/static/latin_america_and_the_caribbean_map.png", # "https://storage.unctad.org/2024-co2emissions/assets/img/latin_america_and_the_caribbean_map.png", # safe=''), #
        }
     #   region_to_image_link = {
     #       "Africa": "/static/africa_map.png",
     #       "Developed": "/static/developed_map.png",
     #       "Developing Asia and Oceania": "/static/asia_and_oceania_map.png",
     #       "Latin America and the Caribbean": "/static/latin_america_and_the_caribbean_map.png"
     #   }
        region_map = {
            'Africa': 'Africa',
            'Developing Asia and Oceania': 'Developing Asia<br>and Oceania',
            'Latin America and the Caribbean': 'Latin America and<br>the Caribbean',
            'Developed': 'Developed' #'bottom center'
        }

        # Use map() to create the new column 'image_link'
        data['image_link'] = data['region1'].map(region_to_image_link)
        

    ########## FILTER AND ORDER DATA
    latin_america_countries = ["ABW", "AIA", "ARG", "ATG", "BES", "BHS", "BLZ", "BOL", "BRA", 
                               "BRB", "CHL", "COL", "CRI", "CUB", "CUW", "DMA", "DOM", "ECU", 
                               "GRD", "GTM", "GUY", "HND", "HTI", "JAM", "LCA", "MEX", "MSR", 
                               "NIC", "PAN", "PER", "PRY", "SLV", "SUR", "SXM", "TCA", "TTO", 
                               "URY", "VCT", "VEN", "VGB"]
    african_countries = ["AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
                         "COG", "COM", "CPV", "DJI", "DZA", "EGY", "ERI", "ETH", "GAB", 
                         "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", 
                         "MAR", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", 
                         "NGA", "RWA", "SDN", "SEN", "SHN", "SLE", "SOM", "SSD", "STP", 
                         "SWZ", "SYC", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", 
                         "ZWE"]
    asia_countries = ["AFG", "ARE", "ARM", "AZE", "BGD", "BHR", "BRN", "BTN", "CHN", 
                      "COK", "FJI", "FSM", "GEO", "HKG", "IDN", "IND", "IRN", "IRQ", 
                      "JOR", "KAZ", "KGZ", "KHM", "KIR", "KWT", "LAO", "LBN", "LKA", 
                      "MAC", "MDV", "MHL", "MMR", "MNG", "MYS", "NCL", "NIU", "NPL", 
                      "NRU", "OMN", "PAK", "PHL", "PLW", "PNG", "PRK", "PSE", "PYF", 
                      "QAT", "SAU", "SGP", "SLB", "SYR", "THA", "TJK", "TKM", "TLS", 
                      "TON", "TUR", "TUV", "TWN", "UZB", "VNM", "VUT", "WLF", "WSM", 
                      "YEM"]
    developed_countries = ["ALB", "AND", "AUS", "AUT", "BEL", "BGR", "BIH", "BLR", "BMU", 
                           "CAN", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", 
                           "FRA", "FRO", "GBR", "GRC", "GRL", "HRV", "HUN", "IRL", "ISL", 
                           "ISR", "ITA", "JPN", "KOR", "LIE", "LTU", "LUX", "LVA", "MDA", 
                           "MKD", "MLT", "MNE", "NLD", "NOR", "NZL", "POL", "PRT", "ROU", 
                           "RUS", "SPM", "SRB", "SVK", "SVN", "SWE", "UKR", "USA"]
    

    geography_list = list(geography_list)
    
    if geographyLevel == "countries":
        if "Latin America and the Caribbean" in geography_list:
            geography_list.remove("Latin America and the Caribbean")  # Modify in place
            geography_list += latin_america_countries
            
        if "Africa" in geography_list:
            geography_list.remove("Africa")
            geography_list += african_countries
            
        if "Developed" in geography_list:
            geography_list.remove("Developed")
            geography_list += developed_countries
            
        if "Developing Asia and Oceania" in geography_list:
            geography_list.remove("Developing Asia and Oceania")
            geography_list += asia_countries
        
        geography_list = list(set(geography_list))

    
    # Flatten the list in case it contains sublists
    geography_list = [item for sublist in geography_list for item in (sublist if isinstance(sublist, list) else [sublist])]

    # Filter the DataFrame based on the list of ISO3 codes
    plot_df = data[(data[geography].isin(geography_list)) & (data['year'] > start_year)].copy()
    
    ##### reorder to prevent frantic swapping
    ## make changes to either scatter plot            
    desired_order = region1s
    desired_order = [category for category in desired_order if category in plot_df[colour_var].unique()]
     
    # create ordering so NaNs come last
    mask = ~plot_df[[x_var, y_var, size_var]].isna().any(axis=1)
    iso3_counts = plot_df[mask][geography].value_counts().reset_index()
    iso3_counts.columns = [geography, 'count']  # Rename the columns
    plot_df = plot_df.merge(iso3_counts, on=geography)
    if geographyLevel == "countries":
        # create ordering such that the ISO3s for whom text should be written come first
        position_rank = {key: rank + 1 for rank, key in enumerate(text_positions.keys())}
        plot_df['custom_order'] = plot_df[geography].map(position_rank).fillna(len(position_rank) + 1).astype(int)
        plot_df = plot_df.sort_values(by=['region1', 'custom_order', 'count', geography, 'year'], ascending=[True, True, False, True, True])
        plot_df = plot_df.drop(columns=['count', 'custom_order'])
    else: 
        plot_df = plot_df.sort_values(by=['region1', 'count', 'year'], ascending=[True, False, True])
        plot_df = plot_df.drop(columns=['count'])

    #geography_list=['ARG', 'AUS', 'BRA', 'CAN', 'CHN', 'FRA', 'DEU', 'IND', 'IDN', 'ITA', 'JPN', 'MEX', 'RUS', 'SAU', 'ZAF', 'KOR', 'TUR', 'GBR', 'USA', 'SGP', 'PNG', 'MYS', 'BRN', 'IRN', 'OMN']
    
    
    ########## CALCULATE VALUES 
    # Calculate weighted 95% percentile
    index_max_x = plot_df.groupby(geography)[x_var].idxmax().dropna()
    index_max_y = plot_df.groupby(geography)[y_var].idxmax().dropna()
    max_x = weighted_percentile(plot_df[x_var][index_max_x], plot_df['pop'][index_max_x], 95) # plot_df[x_var].max() * 1.2
    max_y = weighted_percentile(plot_df[y_var][index_max_y], plot_df['pop'][index_max_y], 95) # plot_df[y_var].max() * 1.2
    
    
    def find_dtick(max_y): #max_y=50
        lower_bound = max_y/10
        upper_bound = max_y/4
        power = np.floor(math.log10(abs(lower_bound)))
        dtick_floor = lower_bound / 10 ** power
        dtick_floor = np.ceil(dtick_floor)
        dtick_ceil = upper_bound / 10 ** power
        dtick_ceil = np.floor(dtick_ceil)
        candidates = list(range(int(dtick_floor), int(dtick_ceil) + 1))
        priority = [5, 2, 1, 10, 4, 6, 8, 3, 7, 9]
        # Iterate through the priority list
        for number in priority:
            if number in candidates:
                dtick = number  # Return the first number found in candidates
                break
        dtick = dtick * 10 ** power
        return dtick
    
    dtick_x = find_dtick(max_x)
    dtick_y = find_dtick(max_y)
    
    # Calculate the range for logarithmic scale
    if x_log:

        dtick_x *= 4
        max_x = ((max_x + dtick_x - 1) // dtick_x) * dtick_x 
        max_x = np.log10(max_x) + np.log10(1.02)


        min_x = plot_df[x_var].min() * 0.9 
        min_x = min_x if min_x > 0 else 0.1
        min_x = np.log10(min_x) - np.log10(1.2)
        min_x = np.log10(985)
    else:
        max_x = ((max_x + dtick_x - 1) // dtick_x) * dtick_x * 1.005
        min_x = 0
    if y_log:
        dtick_y *= 4
        max_y = ((max_y + dtick_y - 1) // dtick_y) * dtick_y 
        max_y = np.log10(max_y) + np.log10(1.02)
        n = np.ceil(np.log10(max_y))
        # Compute the closest power of 10
       # max_y = 10 ** n
        
        min_y = plot_df[y_var].min() * 0.9
        min_y = min_y if min_y > 0 else 0.1
        min_y = np.log10(min_y) - np.log10(1.2)
    else:
        max_y = ((max_y + dtick_y - 1) // dtick_y) * dtick_y * 1.005
        min_y = 0
        
    
    
    #deal with size of bubbles 
    plot_df['bubble_size'] = np.where(plot_df[size_var] > 0, plot_df[size_var] + bubble_similarity, plot_df[size_var])

    plot_df['bubble_size'] = plot_df['bubble_size'].fillna(0)
    if geographyLevel == "countries":
        scatter_size_max_parameter = 60 * bubble_size
    else:
        scatter_size_max_parameter = 60 * bubble_size * 1.25

    # set parameter for flags
    if max_x>max_y:
        if geographyLevel == "countries":
            max_bubble_size_wanted = (max_x/24)*flag_size
        else:
            max_bubble_size_wanted = (max_x/40)*flag_size*4.5
    else:
        if geographyLevel == "countries":
            max_bubble_size_wanted = (max_y/12)*flag_size
        else:
            max_bubble_size_wanted = (max_y/20)*flag_size*4.5
    
    
    # Add normalised_size column such that the value is the diameter making 
    plot_df.loc[:, 'normalised_size'] = 2 * np.sqrt(plot_df['bubble_size'] /np.pi) 
    co2_max = plot_df['normalised_size'].max()
    # scale column such that the co2_max will be the size of the bubble you want measured in x-axis units. 
    # (warning: i think only works if x-axis is larger than y-axis)
    plot_df.loc[:, 'normalised_size'] *= (max_bubble_size_wanted / co2_max)
    
    
    #this is used later in flag addition
    geographies = plot_df[geography].unique()
    
    # num_bubbles used in text - plot_df_scatter
    num_bubbles = len(plot_df[geography].unique())
    
    ########## ADD SMOOTHING
    progress.set(5, "Adding smoothness...")
    def expand_dataframe(df, n, geography, year, x_var, y_var, bubble_size, normalised_size, additional_cols):
        # Create an empty list to store the results
        expanded_rows = []
    
        # Iterate over unique geography values
        for geo in df[geography].unique():
            # Filter the dataframe for the current geography
            geo_df = df[df[geography] == geo].sort_values(by=year).reset_index(drop=True)
    
            # Iterate through consecutive rows
            for i in range(len(geo_df) - 1):
                # Get the starting and ending rows
                row_start = geo_df.iloc[i]
                row_end = geo_df.iloc[i + 1]
    
                # Create interpolated rows
                for j in range(n+1):
                    # Interpolation factor (j=0 gives the start row, j=n-1 gives the end row)
                    alpha = j / n
    
                    # Interpolate the numeric values using dynamic column names
                    interpolated_row = {
                        geography: row_start[geography],  # geography stays the same
                        year: row_start[year] * (1 - alpha) + row_end[year] * alpha,
                        x_var: row_start[x_var] * (1 - alpha) + row_end[x_var] * alpha,
                        y_var: row_start[y_var] * (1 - alpha) + row_end[y_var] * alpha,
                        bubble_size: row_start[bubble_size] * (1 - alpha) + row_end[bubble_size] * alpha,
                        normalised_size: row_start[normalised_size] * (1 - alpha) + row_end[normalised_size] * alpha,
                    }
                   
                    # Keep additional columns (like image_link and region1) unchanged
                    for col in additional_cols:
                        interpolated_row[col] = row_start[col]
                        
                        
                    # Append the interpolated row to list of rows
                    expanded_rows.append(interpolated_row)

        
        expanded_df = pd.DataFrame(expanded_rows)
        return expanded_df


    if geographyLevel == "countries":
        additional_cols = ['image_link', 'region1']
    else:
        additional_cols = ['image_link', 'region1']
       
   # print(hp.heap())    
    plot_df = expand_dataframe(plot_df, 
                               n=smoothness, 
                               geography=geography, 
                               year='year', 
                               x_var=x_var, 
                               y_var=y_var, 
                               bubble_size='bubble_size',
                               normalised_size = 'normalised_size',
                               additional_cols = additional_cols
                               )
   
    
    plot_df.to_csv('/Users/edbaker/UN_projects/c02emmisions/plot_df.csv')
    
    # print(hp.heap())
    print(hp.heap())
    
    ########## CREATE SCATTER PLOT
    if geographyLevel == "countries":
        # Create the base plot with Plotly Express
        
        print("try to make plot_df_scatter")
        # Create a copy of the original plot_df
        plot_df_scatter = plot_df.copy()
        
        # Step 1: Create a mask to find rows where geography is in text_positions
        if num_bubbles <= 20:
            mask = pd.Series([True] * len(plot_df_scatter))
        else:
            mask = plot_df_scatter[geography].isin(text_positions)
        
        # Step 2: Create a DataFrame of the filtered rows where geography is in text_positions
        replicated_df = plot_df.loc[mask].copy()
        
        # Step 3: Modify the replicated rows
        replicated_df[colour_var] = replicated_df[colour_var] + 'text'  # Change colour_var to 'text'
        replicated_df['bubble_size'] *= 0.2  # Halve the bubble size
        
        # Step 4: Concatenate the original DataFrame with the modified replicated DataFrame
        plot_df_scatter = pd.concat([plot_df_scatter, replicated_df], ignore_index=True)
        replicated_df = None

        desired_order = region1s + [item + 'text' for item in region1s]
        desired_order = [category for category in desired_order if category in plot_df_scatter[colour_var].unique()]
      
        print("made plot_df_scatter")
        
        
        figScatter = px.scatter(
            plot_df_scatter, 
            x=x_var, 
            y=y_var, 
            color=colour_var,
            color_discrete_map=color_map,
            size='bubble_size',
            text=geography,
            hover_name=geography,
            animation_frame='year', 
            size_max=scatter_size_max_parameter,
            template="plotly_white",
            #trendline="lowess"
        )
        plot_df_scatter = None
        

        figScatter.update_traces(marker=dict(opacity=1))  # Set opacity to 0 for invisibility

        

    else:
        # Create the base plot with Plotly Express
        figScatter = px.scatter(
            plot_df, 
            x=x_var, 
            y=y_var, 
            color=colour_var,
            color_discrete_map=color_map,
            size='bubble_size', 
            text=geography,
            hover_name=geography,
            animation_frame='year', 
            size_max=scatter_size_max_parameter,
            template="plotly_white"
        )
        
        figScatter.update_traces(textposition='top right')
        figScatter.update_traces(marker=dict(opacity=0))
        
        
        

    # Extract traces and reorder them -  is this for legend only?
    ordered_traces = []
    #print(figScatter.data)
    #print(desired_order)
    for category in desired_order:
        # Filter traces for each category
        trace = next(tr for tr in figScatter.data if tr.name == category)
        ordered_traces.append(trace)        
    # Update the figure with reordered traces
    figScatter.data = ordered_traces
    ordered_traces = None
    #print(figScatter.data)
    
    figScatter.update_layout(
        legend_title_font=dict(family = "Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size = 21, color = "black", weight="bold"),
        legend=dict(
            title="",
            orientation="h",
            entrywidth=150,
           # entrywidthmode='fraction',
            yanchor="bottom",
            y=1,
            xanchor="center",
            x=0.5,
            #x=0.10,            # x-coordinate of the legend (0 = left, 1 = right)
            #y=0.85,            # y-coordinate of the legend (0 = bottom, 1 = top)
            #xanchor='left', # Positioning relative to the x-coordinate (left)
            #yanchor='top',  # Positioning relative to the y-coordinate (top)
            #traceorder='normal',
            font = dict(family = "Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size = 21, color = "black", weight="bold"),
            itemsizing='constant'
        ),
        plot_bgcolor='#F4F9FD',  # Background color inside the axes
        paper_bgcolor='#F4F9FD',
        xaxis=dict(
            linecolor='black',  # Color of the x-axis line
            linewidth=2,        # Thickness of the x-axis line
            gridwidth=2,
            gridcolor='darkgrey', 
            griddash='dot',
            tickfont=dict(size=20, color='#6e6259', family = "Inter, Helvetica Neue, Helvetica, Arial, sans-serif"),
        ),
        yaxis=dict(
            showline=False,
            linecolor='black',  # Color of the y-axis line
            linewidth=2,        # Thickness of the y-axis line
            gridwidth=2, 
            gridcolor='darkgrey', 
            griddash='dot',
            tickfont=dict(size=20, color='#6e6259', family = "Inter, Helvetica Neue, Helvetica, Arial, sans-serif"),
        )
    )
 
    # add log axes if necessary
    if x_log:
        figScatter.update_xaxes(type="log")
    else:
        figScatter.update_xaxes(dtick=dtick_x)
    
    if y_log:
        figScatter.update_yaxes(type='log')
    else:
        figScatter.update_yaxes(dtick=dtick_y)
        
    # following code does not work: it changes the hovertext on the first frame only, and in that case wrong... need to deal with frames!.
    #.. better to go trace by trace?
    
  #  figScatter.update_traces(
  #      customdata=plot_df[[geography, 'gdp']].values,
  #      hovertemplate="<br>".join([
  #          "<strong>%{customdata[0]}</strong><br>",
  #          "GDP (millions): %{customdata[1]:,.0f}",
  #          "<extra></extra>"  # Removes the trace name from hover
  #  ])
  #  )
    
    
    ########## CREATE LINE PLOT    
    now_time = time.time()
    print(f"Created scatter time: {now_time - start_time} seconds")
    progress.set(10, "calculating lines data...")
    
    if leave_trace:
        # xxxxx add a line of best fit : smooth curve..or pre-calculate and save in data?

        if use_loess:
            def loess_smoothing(x, y, frac=0.3):
                # Apply LOESS (Locally Weighted Scatterplot Smoothing)
                loess_model = sm.nonparametric.lowess(y, x, frac=frac)
                return loess_model[:, 1]
            
            # Group by 'geography' and apply LOESS smoothing to 'y_var' over 'year'
            plot_df_line = pd.DataFrame()
            for geog, group in plot_df.groupby(geography):
                # Apply LOESS smoothing for y_var (you can adjust frac for smoother/less smooth fit)
                group[x_var] = loess_smoothing(group['year'], group[x_var], frac=0.3)
                group[y_var] = loess_smoothing(group['year'], group[y_var], frac=0.3)
                # Append smoothed data to a new DataFrame
                plot_df_line = pd.concat([plot_df_line, group], axis=0)
        else:
            
            plot_df_line = pd.DataFrame()
            for geog, group in plot_df.groupby(geography):
                group[x_var] = group[x_var].rolling(rolling_mean_years).mean()
                group[y_var] = group[y_var].rolling(rolling_mean_years).mean()

                
                plot_df_line = pd.concat([plot_df_line, group], axis=0)
            
        
        expanded_rows = pd.DataFrame()
        for year in plot_df_line['year'].unique(): # year=plot_df['year'].unique()[12]
            # Filter the rows for the current year and all previous years
            filtered_rows = plot_df_line[plot_df_line['year'].astype(float) <= float(year)].copy()
            filtered_rows.loc[:, 'year'] = year
            # Append these rows to the list
            expanded_rows = pd.concat([expanded_rows, filtered_rows], axis=0, ignore_index=True)
        
        
        plot_df_line.to_csv('/Users/edbaker/UN_projects/c02emmisions/loess_smoothing.csv')
        
        plot_df_line = None
        
        
        #print(expanded_rows[expanded_rows['ISO3'] =="JPN"])
        
        now_time = time.time()
        print(f"calculated lines time: {now_time - start_time} seconds")
        progress.set(40, "creating lines plot...")
        
        # Create an animated line plot
        figLine = px.line(
            expanded_rows, 
            x=x_var, 
            y=y_var, 
            color=colour_var,
            line_group=geography,
            color_discrete_map=color_map,
            #line_group=geography,  # Group by geography to draw a separate line for each
            #hover_name=geography, 
            animation_frame='year',
            template="plotly_white"
            )
        expanded_rows=None
        
        figLine.update_traces(line={'width': 5})
        figLine.update_traces(hoverinfo='skip', hovertemplate=None)
        figLine.update_traces(opacity=0.8)

    

        # add log axes
        if x_log:
            figLine.update_xaxes(type="log")
        if y_log:
            figLine.update_yaxes(type='log')

        # Hide legend for figLine traces
        figLine.update_layout(showlegend=False)
        for trace in figLine.data:
            trace.showlegend = False
            
        ########### COMBINE PLOTS
        now_time = time.time()
        print(f"created line graph time: {now_time - start_time} seconds")
        progress.set(70, "Combining plots...")    
            
        # now integrate, traces, frames and layout
        fig = go.Figure(
          data=figLine.data + figScatter.data,
          frames=[
              go.Frame(data=fr1.data + fr2.data, name=fr2.name)
              for fr1, fr2 in zip(figLine.frames, figScatter.frames)
          ],
          layout=figScatter.layout,
        )
        #fig.update_layout(
        #    legend=dict(
        #        x=0,            # x-coordinate of the legend (0 = left, 1 = right)
        #        y=1,            # y-coordinate of the legend (0 = bottom, 1 = top)
        #        xanchor='left', # Positioning relative to the x-coordinate (left)
        #        yanchor='top',  # Positioning relative to the y-coordinate (top)
        #    )
        #)
        fig.update_traces(hoverinfo='skip', hovertemplate=None)
        
        now_time = time.time()
        print(f"combined graphs time: {now_time - start_time} seconds")
        progress.set(90, "finishing...")
        
    else:
        fig = figScatter
    figScatter = None
    figLine = None
    
    print(hp.heap())
    
    ########## CHANGE CUSTOM SETTINGS
    progress.set(50, "custom settings...")
    #first set fig traces then go frame by frame and 
    # add flags or,
    # add labels
    # hovertext
    # set settings xxx combine all frame by frames
    
    
    

    # Update inital data 
    for trace in fig.data:
        #set frame data for all traces
        if trace.legendgroup == "Developed":
            trace.legendgroup = "Developed"
            trace.name = "Developed"
        elif trace.legendgroup == "Latin America and the Caribbean":
            trace.legendgroup = "Latin America <br>and the Caribbean"
            trace.name = "Latin America <br>and the Caribbean"
        elif trace.legendgroup == "Developing Asia and Oceania":
            trace.legendgroup = "Developing Asia <br>and Oceania"
            trace.name = "Developing Asia <br>and Oceania"
        elif trace.legendgroup == "Africa":
            trace.legendgroup = "Africa"
            trace.name = "Africa"
        
        # set initial data for line traces
        if trace.mode == 'lines':
            trace.showlegend = False
        
        # set initial data for bubble data
        else:
            if geographyLevel == "countries":
                if 'text' not in trace.legendgroup:
                    trace.showlegend = True
                else:
                    trace.showlegend = False
                    
            else:
                trace.showlegend = False
            
            #regading text
            if trace.text is not None and len(trace.text) > 0:
                

                tracetext = []
                tracetextpositions = []
                for listed_geog in trace.text:
                    #for all listed geogs
                    tracetextfonts = dict(family = "Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size = 21, weight="bold", color=color_map[trace.legendgroup])
                    if ('text' in trace.name or geographyLevel == "regions"):
                        if geographyLevel == "regions":
                            tracetext.append(region_map[listed_geog])
                            
                        else:
                            tracetext.append(country_map[listed_geog])
                        if listed_geog in text_positions:
                            tracetextpositions.append(text_positions[listed_geog])
                        else:
                            tracetextpositions.append('top right')
                        if 'text' in trace.name:
                            trace.marker.opacity = 0
                    else:
                        tracetext.append('')
                        tracetextpositions.append('top right')
                trace.text = tracetext
                trace.textposition = tracetextpositions
                trace.textfont = tracetextfonts
                    
    
    # Update frame by frame
    for frame in fig.frames:
        #set traces in frame.data
        for trace in frame.data:
            #set frame data for all traces
            if trace.legendgroup == "Developed":
                trace.legendgroup = "Developed"
                trace.name = "Developed"
            elif trace.legendgroup == "Latin America and the Caribbean":
                trace.legendgroup = "Latin America <br>and the Caribbean"
                trace.name = "Latin America <br>and the Caribbean"
            elif trace.legendgroup == "Developing Asia and Oceania":
                trace.legendgroup = "Developing Asia <br>and Oceania"
                trace.name = "Developing Asia <br>and Oceania"
            #print(frame.name)
            
            # set frame data for line traces
            if trace.mode == 'lines':
                trace.showlegend = False
            
            # set frame data for bubble traces
            else:
                if geographyLevel == "countries":
                    if 'text' not in trace.legendgroup:
                        trace.showlegend = True
                    else:
                        trace.showlegend = False
                else:
                    trace.showlegend = False
                    
                #regarding text
                if trace.text is not None and len(trace.text) > 0:
                    
                    tracetext = []
                    tracetextpositions = []
                    for listed_geog in trace.text:
                        #for all listed geogs
                        tracetextfonts = dict(family = "Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size = 21, weight="bold", color=color_map[trace.legendgroup])
                        if ('text' in trace.name or geographyLevel == "regions"):
                            if geographyLevel == "regions":
                                tracetext.append(region_map[listed_geog])
                            else:
                                tracetext.append(country_map[listed_geog])
                            if listed_geog in text_positions:
                                tracetextpositions.append(text_positions[listed_geog])
                            else:
                                tracetextpositions.append('top right')
                            if 'text' in trace.name:
                                trace.marker.opacity = 0
                        else:
                            tracetext.append('')
                            tracetextpositions.append('middle center')
                    trace.text = tracetext
                    trace.textposition = tracetextpositions
                    trace.textfont = tracetextfonts
                    
                    

        
        #set annotations in frame, year and axes labels
        year = math.floor(float(frame.name))
        new_annotation = go.layout.Annotation(
            text=f"{year}",  # Annotation text showing the year
            showarrow=False,  # No arrow pointing to a data point
            xref="x",  # X position relative 'paper' or 'x' for 'data'
            yref="y",  # Y position relative 'paper' or 'y' for 'data'
            x= min_x + 0.1*(max_x-min_x), #   pow(10, max_x) * 0.5,  # X position (bottom right)
            y= min_y + 0.75*(max_y-min_y),  # Y position (bottom right)
            font=dict(size=45, color="black", family="Inter, Helvetica Neue, Helvetica, Arial, sans-serif", weight="bold"),  # Font size and color
            xanchor="left",
            align="left"  # Align text to the right
        )
        if 'annotations' in frame.layout:
            frame.layout.annotations += (new_annotation,)
        else:
            frame.layout.annotations = (new_annotation,)
        
    
        new_annotation = go.layout.Annotation(
            text=x_var_label,  # Annotation text showing the year
            showarrow=False,  # No arrow pointing to a data point
            xref="x",  # X position relative 'paper' or 'x' for 'data'
            yref="y",  # Y position relative 'paper' or 'y' for 'data'
            x= min_x + 0.99*(max_x-min_x), #   pow(10, max_x) * 0.5,  # X position (bottom right)
            y= min_y + 0.02*(max_y-min_y),  # Y position (bottom right)
            font=dict(size=17, color="black", family="Inter, Helvetica Neue, Helvetica, Arial, sans-serif", weight="bold"),  # Font size and color
            xanchor="right",
            align="right",
        )
        if 'annotations' in frame.layout:
            frame.layout.annotations += (new_annotation,)
        else:
            frame.layout.annotations = (new_annotation,)
            
        new_annotation = go.layout.Annotation(
            text=y_var_label,  # Annotation text showing the year
            showarrow=False,  # No arrow pointing to a data point
            xref="x",  # X position relative 'paper' or 'x' for 'data'
            yref="y",  # Y position relative 'paper' or 'y' for 'data'
            x= min_x + 0.01 *(max_x-min_x), #   pow(10, max_x) * 0.5,  # X position (bottom right)
            y= min_y + 0.97 *(max_y-min_y),  # Y position (bottom right)
            font=dict(size=17, color="black", weight="bold", family="Inter, Helvetica Neue, Helvetica, Arial, sans-serif"),  # Font size and color
            xanchor="left",
            align="left",
        )
        if 'annotations' in frame.layout:
            frame.layout.annotations += (new_annotation,)
        else:
            frame.layout.annotations = (new_annotation,)
        
        new_annotation=None



    #change label in slider steps

    new_steps = []
    for step in fig.layout.sliders[0].steps:
        # Check if the step label can be converted to a float and is divisible by 5
            if float(step.label) % 5 == 0:  # Check if divisible by 5
                step.label = str(round(float(step.label)))
                new_steps.append(step)  # Add step to new_steps

    # Update the slider with the new filtered steps
    fig.layout.sliders[0].steps = new_steps


            
    fig.layout.sliders[0].currentvalue = dict(visible=False)
    fig.layout.sliders[0].minorticklen = 0
    fig.layout.sliders[0].ticklen = 0

    
    full_index = pd.Index(geographies, name=geography)
    if show_flags:
        progress.set(90, "adding flags...")
        
        for frame in fig.frames: #frame = fig.frames[19]
            year = frame.name
            year_data = plot_df[plot_df['year'] == float(year)]
            
            ## add in NA rows for missing data
            year_data = year_data.drop_duplicates(subset=[geography])
            year_data = year_data.set_index(geography).reindex(full_index).reset_index()

            # Create list of image annotations for this year
            image_annotations = []
            for i, row in year_data.iterrows():
                if x_log:
                    x_position = np.log10(row[x_var])
                else:
                    x_position = row[x_var]
                if y_log:
                    y_position = np.log10(row[y_var])
                else:
                    y_position = row[y_var]
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
                            'opacity': 1,
                            'layer': "above"
                            }
                          )
                else:
                    image_annotations.append(
                      {
                              'source': row['image_link'],
                              'xref': "x",
                              'yref': "y",
                              'x': x_position,
                              'y': y_position,
                              'sizex': row['normalised_size'],
                              'sizey': row['normalised_size'],
                              'xanchor': "center",
                              'yanchor': "middle",
                              'sizing': "contain",
                              'opacity': 1,
                              'layer': "above"
                              }
                          )
            
            frame.layout.images = image_annotations
            image_annotations = None
            #print(frame.layout.images)
            
        now_time = time.time()
        print(f"added flags time: {now_time - start_time} seconds")
        progress.set(95, "printing plot...")
     
        
    plot_df = None

    ######## LABELS, AXES, AND SLIDER
    
    
    buttons = [{
        'buttons': [
            {
                'args': [None, {'frame': {'duration': frame_duration, 'redraw': True}, 'fromcurrent': True, 'mode': 'immediate', 'transition': {'duration': 0}}],
               # 'label': 'Play',
                'method': 'animate'
            },
            {
                'args': [[None], {'frame': {'duration': frame_duration, 'redraw': True}, 'mode': 'immediate', 'transition': {'duration': 0}}],
               # 'label': 'Pause',
                'method': 'animate'
            }
        ],
        'direction': 'left',
        'pad': {'r': 10, 't': 87},
        'showactive': False,
        'type': 'buttons',
        'x': 0.1,
        'xanchor': 'right',
        'y': 0.05,
        'yanchor': 'top'
    }]
    sliders=[{
        'active': 0,
        #'currentvalue': {'prefix': 'Frame: '},
        'x': 0.1,
        'len': 0.9,
        'pad': {'b': 10},
        'ticklen': 0,
    }]
    
    
    # Adjust layout
    fig.update_layout(
        xaxis_title=None, #{'text': x_var_label},
        yaxis_title=None, #{'text': y_var_label},
      #  title={'text': f"<span style='font-size:28px; font-family:Inter;'><b>The glaring inequality of income and CO<sub>2</sub> emissions</b></span><br><span style='font-size:18px;'><i>{y_var_label} vs {x_var_label}</i></span>",
      #         'x': 0.08,
      #         'xanchor': 'left',},
        autosize=True,
        #width=1100,
        #height=700,
        updatemenus=buttons,
        #sliders=sliders,
        title_font=dict(family="Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size=24, color="black"), #now there is no title, moved to annotations
        xaxis_title_font=dict(family="Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size=18, color="black"), #now there is none, moved to annotations
        yaxis_title_font=dict(family="Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size=18, color="black"), #now there is none, moved to annotations
        font=dict(family="Inter, Helvetica Neue, Helvetica, Arial, sans-serif", size=14, color="black"),
    )
    
    if fixed_axes:
        fig.update_yaxes(fixedrange = True)
        fig.update_xaxes(fixedrange = True)
        fig.update_layout(
          xaxis=dict(range=[min_x, max_x]),
          yaxis=dict(range=[min_y, max_y])
          )


   
    ######### SAVE OR RETURN PLOT
  
    if download == "html":
        num_frames = len(fig.frames)
        print("Number of frames:", num_frames)
        
        # Generate the HTML string
        animation_opts = {'frame': {'duration': frame_duration, 'redraw': True},'transition': {'duration': 0}}
        fig.update_traces(hoverinfo="skip", hovertemplate=None)
        fig.update_layout(hovermode=False)  # Turns off hover globally
        fig.update_xaxes(tickformat= "~s") #".1r")
        
        plot_path = filename #'/Users/edbaker/UN_projects/c02emmisions/plotly_animation.html'
        fig.write_html(plot_path,
                       include_plotlyjs=False, #'cdn',
                       full_html=True, 
                       auto_play=True, 
                       default_width='95vw', 
                       default_height='95vh', 
                       div_id='id_plot-container', 
                       animation_opts=animation_opts,
                       config={'displayModeBar': False})
    
  
    if width != "default":
        fig.update_layout(width=int(width), height=int(height))
    
        
    def plotly_fig2array(fig):
       fig_bytes = fig.to_image(format="png")
       buf = io.BytesIO(fig_bytes)
       img = Image.open(buf)
       return np.asarray(img)
   
    animation_duration = length  # seconds for the entire animation
   
    from copy import deepcopy
    
   # Function to update the figure for each frame and return an image array
    def make_frame(t):
       print("t= " + str(t))
       # Get the current frame based on time 't' (frames are sequentially spaced)
      # print(hp.heap())
       current_frame = int(t * len(fig.frames)/animation_duration)  # Scales time 't' to the number of frames
       if current_frame > len(fig.frames)-1:
           current_frame = len(fig.frames)-1
       
       print("frame data xxxxxxxxxxxxxxxxxxxxxxxxxx") 
       print(fig.frames[current_frame].data)
       print("fig data xxxxxxxxxxxxxxxxxxxxxxxxxxx")
       print(fig.data) 
       
       # Update the figure with the data of the current frame
       figFrame = go.Figure()
       # Loop through each trace in the current frame's data
       for trace_index, trace_template in enumerate(fig.data):
            # Deep copy the trace from fig.data to avoid modifying the original trace
            new_trace = deepcopy(trace_template)
            # Step 2: Update the x, y, and marker size according to the current frame's data
            if trace_index < len(fig.frames[current_frame].data):  # Check if the frame has this trace
                frame_trace_data = fig.frames[current_frame].data[trace_index]
                # Update positions and marker sizes
                new_trace.update({
                    'x': frame_trace_data['x'],             # Update x values
                    'y': frame_trace_data['y'],             # Update y values
                    'marker.size': frame_trace_data['marker.size']  # Update marker size
                })
            
                # Add the updated trace to figFrame
                figFrame.add_trace(new_trace)
            
            
       figFrame.update_layout(fig.layout)
       figFrame.update_layout(fig.frames[current_frame].layout)
      # fig.update_layout(updatemenus=[], sliders=[])  # Remove buttons and sliders
       figFrame.layout.sliders[0].visible=False
       figFrame.layout.updatemenus[0].buttons[0].visible=False
       figFrame.layout.updatemenus[0].buttons[1].visible=False
       
       # Convert the updated figure to an array (image) and return it
       return plotly_fig2array(figFrame)
   
    
    if download == "png":
        final_frame = make_frame(animation_duration)
    
        # Save the final frame as a PNG file
        png_path = filename #'/Users/edbaker/UN_projects/c02emmisions/final_frame.png'
        Image.fromarray(final_frame).save(png_path)
   
   
   
    if download != "nothing":
        print(hp.heap())
        # Function to convert a Plotly figure to an image array

        print("attempt videoclip")
        # Create a MoviePy video clip using the `make_frame` function
        animation = mpy.VideoClip(make_frame, duration=animation_duration)
        
        print(hp.heap())
        print("delete fig")
       # fig = None
        print(hp.heap())
        print(type(animation))


    if download=="mp4":
        print("attempt mp4")
        
        

        # Get the dimensions of the frame
        frame_shape = animation.get_frame(0).shape
        frame_height, frame_width, _ = frame_shape
        print(frame_shape)
        
        print("writing " + filename)
        # Define the video writer
        out = cv2.VideoWriter(filename, cv2.VideoWriter_fourcc(*'mp4v'), fps, (frame_width, frame_height))
        
        print(hp.heap())
        
        print("for loop")
        # Write each frame to the video file
        for i in range(int(animation.duration * fps)):  
            print(i)
            frame = animation.get_frame(i / fps)
            out.write(cv2.cvtColor(frame, cv2.COLOR_RGB2BGR))  # Convert from RGB to BGR (for OpenCV)
        
        # Release the video writer
        out.release()
       # print(hp.heap())
        
    if download=="gif":
        print("writing " + filename)
        # Write the animation to a GIF
        animation.write_gif(filename, fps=fps)
       # print(hp.heap())
        
    if False:
        # write to video
        animation.write_videofile("/Users/edbaker/UN_projects/c02emmisions/plotly_animation2.mp4", fps=fps, codec="mpeg4", temp_audiofile="temp_audiofilexxxx")
        animation.write_videofile("/Users/edbaker/UN_projects/c02emmisions/plotly_animation3.webm",audio=False, fps=fps)

        # Directory to save individual frames
        import os 
        
        output_dir = "frames"
        os.makedirs(output_dir, exist_ok=True)
        
        # Save each frame as an image
        for i in range(int(animation.duration * fps)):  # Assuming 24 FPS
            frame = animation.get_frame(i / fps)  # Get frame at the given time
            frame_img = Image.fromarray(frame)
            frame_img.save(f"{output_dir}/frame_{i:04d}.png")
            
            
            
        
    
    return(fig)
    
#fig = createCountryBubbleGraph()    
# Save the figure as HTML
#fig.write_html('/Users/edbaker/UN_projects/c02emmisions/plotly_animation.html')
