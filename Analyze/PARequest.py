# -*- coding: utf-8 -*-
"""
This code gets hisotrical PurpleAir data of one site at a time and 
for two days ONLY from new PurpleAir API.

Data from the site are in bytes/text and NOT in JSON format.

@author: Zuber Farooqui, Ph.D.
Modifed by Hongjian 
Lastest modification: 06/30/2023
"""

import requests
import pandas as pd
from datetime import datetime
import time
import json
import numpy as np
from io import StringIO
# from sqlalchemy import create_engine

API_KEY = # Your API read Key
# Starting engine for postgresql
#engine = create_engine('postgresql://postgres:password@location:port/database')

# API Keys provided by PurpleAir(c)
key_read  = API_KEY

# Sleep Seconds
sleep_seconds = 1 # wait sleep_seconds after each query

def get_sensorslist(nwlng,nwlat,selng,selat,location,key_read):
    # PurpleAir API URL
    root_url = 'https://api.purpleair.com/v1/sensors/'

    # Box domain: lat_lon = [nwlng,, nwlat, selng, selat]
    lat_lon = [nwlng, nwlat, selng, selat]
    for i,l in enumerate(lat_lon):
        if (i == 0):
            ll_api_url = f'&nwlng={l}'
        elif (i == 1):
            ll_api_url += f'&nwlat={l}'
        elif (i == 2):
            ll_api_url += f'&selng={l}'
        elif (i == 3):
            ll_api_url += f'&selat={l}'
        
    # Fields to get
    fields_list = ['sensor_index','name','latitude','longitude'] 
    for i,f in enumerate(fields_list):
        if (i == 0):
            fields_api_url = f'&fields={f}'
        else:
            fields_api_url += f'%2C{f}'

    # Indoor, outdoor or all
    if (location == 'indoor'):
        loc_api = f'&location_type=1'
    elif (location == 'outdoor'):
        loc_api = f'&location_type=0'
    else:
        loc_api = ''
            
    # Final API URL
    api_url = root_url + f'?api_key={key_read}' + fields_api_url + ll_api_url + loc_api

    # Getting data
    response = requests.get(api_url)

    if response.status_code == 200:
        #print(response.text)
        json_data = json.loads(response.content)['data']
        df = pd.DataFrame.from_records(json_data)
        df.columns = fields_list
    else:
        raise requests.exceptions.RequestException

    # Creating a PurpleAir monitors table in PostgreSQL (Optional)
    # df.to_sql('tablename', con=engine, if_exists='append', index=False)
    
    # writing to csv file
    #f#olderpath = 'Folder path'
    #filename = folderpath + '\sensors_list.csv'
    #df.to_csv(filename, index=False, header=True)
            
    # Creating a Sensors 
    sensorslist = list(df.sensor_index)
    
    return df

def get_historicaldata(sensors_list,bdate,edate,average_time,key_read):
    out = pd.DataFrame()
    # Historical API URL
    root_api_url = 'https://api.purpleair.com/v1/sensors/'
    
    # Average time: The desired average in minutes, one of the following:0 (real-time),10 (default if not specified),30,60
    average_api = f'&average={average_time}'

    # Creating fields api url from fields list to download the data: Note: Sensor ID/Index will not be downloaded as default
    fields_list = ['pm2.5_atm_a', 'pm2.5_atm_b', 'pm2.5_cf_1_a', 'pm2.5_cf_1_b', 'humidity_a', 'humidity_b', 
               'temperature_a', 'temperature_b', 'pressure_a', 'pressure_b']
    for i,f in enumerate(fields_list):
        if (i == 0):
            fields_api_url = f'&fields={f}'
        else:
            fields_api_url += f'%2C{f}'

    # Dates of Historical Data period
    begindate = datetime.strptime(bdate, '%m-%d-%Y')
    enddate   = datetime.strptime(edate, '%m-%d-%Y')
    
    # Downlaod days based on average
    if (average_time == 60):
        date_list = pd.date_range(begindate,enddate,freq='14d') # for 14 days of data
    else:
        date_list = pd.date_range(begindate,enddate,freq='2d') # for 2 days of data
        
    # Converting to UNIX timestamp
    date_list_unix=[]
    for dt in date_list:
        date_list_unix.append(int(time.mktime(dt.timetuple())))

    # Reversing to get data from end date to start date
    date_list_unix.reverse()
    len_datelist = len(date_list_unix) - 1
        
    # Getting 2-data for one sensor at a time
    for s in sensors_list:
        # Adding sensor_index & API Key
        hist_api_url = root_api_url + f'{s}/history/csv?api_key={key_read}'

        # Creating start and end date api url
        for i,d in enumerate(date_list_unix):
            # Wait time 
            time.sleep(sleep_seconds)
            
            if (i < len_datelist):
                print('Downloading for PA: %s for Dates: %s and %s.' 
                      %(s,datetime.fromtimestamp(date_list_unix[i+1]),datetime.fromtimestamp(d)))
                
                dates_api_url = f'&start_timestamp={date_list_unix[i+1]}&end_timestamp={d}'
            
                # Final API URL
                api_url = hist_api_url + dates_api_url + average_api + fields_api_url
                            
                #
                try:
                    response = requests.get(api_url)
                except:
                    print(api_url)
                #
                try:
                    assert response.status_code == requests.codes.ok
                
                    # Creating a Pandas DataFrame
                    df = pd.read_csv(StringIO(response.text), sep=",", header=0)
                
                except AssertionError:
                    df = pd.DataFrame()
                    print('Bad URL!')
            
                if df.empty:
                    print('------------- No Data Available -------------')
                else:
                    # Adding Sensor Index/ID
                    df['id'] = s
                
                    #
                    date_time_utc=[]
                    for index, row in df.iterrows():
                        date_time_utc.append(datetime.fromtimestamp(row['time_stamp']))
                    df['date_time_utc'] = date_time_utc
                
                    # Dropping duplicate rows
                    df = df.drop_duplicates(subset=None, keep='first', inplace=False)
                    
                    out = pd.concat([out, df])
                    # Writing to Postgres Table (Optional)
                    # df.to_sql('tablename', con=engine, if_exists='append', index=False)
                    
                    # writing to csv file
                    # folderpath = 'Folder path'
                    # filename = folderpath + '\sensorsID_%s_%s_%s.csv' % (s,datetime.fromtimestamp(date_list_unix[i+1]).strftime('%m-%d-%Y'),datetime.fromtimestamp(d).strftime('%m-%d-%Y'))
                    #df.to_csv(filename, index=False, header=True)
    return out


# Getting sensors list in Box domain [nwlng,, nwlat, selng, selat]
location='outdoor' # or 'indoor' or 'both'
# -81.67, 36.588,34.62, -75.84
# NC coordinate: (36.588, -81.69)
# NC coordinate: (34.62, -75.84)
# CA coordinate: (42.00, -124.34) # NW
# CA coordinate: (32.77, -114.27) # SE
# Coordiante here is in reverse order
df = get_sensorslist(-124.34, 42.00, -114.27, 32.77, location, key_read)

#%% Exclude sensor list in some states
# For California 2021, we exclude data in Nevada
# (Latitude, Longitude)
# Coordinate: (39.02, -119.97), (41.99, -119.97)
# (39.02, -119.97), (34.30, -114.10)
exclude1 = df[(df.latitude >= 39.02) & (df.latitude <= 41.99) & (df.longitude >= -119.97)]
exclude2 = df[(df.latitude >= -0.804 * df.longitude - 57.43588) & (df.longitude >= -119.97)]
invalid_index = exclude1.sensor_index.append(exclude2.sensor_index)

#%% Get history reading based on sensors list
# Average_time. The desired average in minutes, one of the following: 0 (real-time), 
#                  10 (default if not specified), 30, 60, 360 (6 hour), 1440 (1 day)
average_time=60 # or 10  or 0 (Current script is set only for real-time, 10, or 60 minutes data)
sensors = df.sensor_index
sensors = list(set(sensors_list) - set(invalid_index))
# Getting PA data
# Data download period
bdate = '6-1-2021' 
edate = '11-31-2021'
df = get_historicaldata(sensors,bdate,edate,average_time,key_read)
#%%
# Export sensors index
s = pd.DataFrame(sensors)
s.to_csv("sensors.csv")
df.to_csv("PAData.csv")






