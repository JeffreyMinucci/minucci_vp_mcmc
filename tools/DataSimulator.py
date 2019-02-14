from VarroaPy.VarroaPy.RunVarroaPop import VarroaPop
from itertools import product
import numpy as np
import os
import pandas as pd
from math import floor

#DEBUG options
LOGS = False

#Start date for sims
DATA_DIR = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..','data'))
INITIAL_DF = os.path.join(DATA_DIR, 'raw/','field_initial_conditions.csv')
START_DATES = ['04/29/2015','04/27/2015', '04/27/2015', '04/28/2015', '04/28/2015', '05/01/2015', '04/30/2015',
               '04/29/2015', '04/28/2015', '04/30/2015']
END_DATE = '08/25/2015'

DATES = ['5', '6', '8']
DATES_STR = pd.read_csv(os.path.join(DATA_DIR+"/raw/field_bee_areas.csv")).iloc[:, 15:19]
SITES = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10']
RESPONSE_VARS = [('Adults', ['Adult Drones', 'Adult Workers', 'Foragers']),('Pupae',['Capped Drone Brood', 'Capped Worker Brood']),
                 ('Larvae', ['Drone Larvae', 'Worker Larvae']),  ('Eggs', ['Drone Eggs', 'Worker Eggs'])]
RESPONSE_FILTER = ['Adults'] #For now use only these responses!


def simulate(pars, save = False, logs = False):
    """
    Simulate data from the colony study using a set of VarroaPop parameters

    :param pars: Dictionary of parameters to vary.
                ICQueenStrength_mean
                ICQueenStrength_sd
                ICForagerLifespan_mean
                ICForagerLifespan_sd
                AIAdultLD50  # in log10!
                AIAdultSlope
                AILarvaLD50   # in log10!
                AILarvaSlope
                AIHalfLife
    :return a dictionary of summary stats
    """
    #print(DATA_DIR)
    #print(INITIAL_DF)
    #print(parameters)
    parameters = pars.copy() #copy our inputs so that we don't ever modify them (pyabc needs these)
    static_pars = {'SimEnd': END_DATE, 'IPollenTrips': 8, 'INectarTrips': 17, 'RQEnableReQueen': 'false'}
    for name, value in parameters.items():
        if not name.endswith(('_mean','_sd')):
            static_pars[name] = value
    static_pars['AIAdultLD50'] = 10**static_pars['AIAdultLD50'] #un log transform
    static_pars['AILarvaLD50'] = 10**static_pars['AILarvaLD50'] #un log transform
    static_pars['NecPolFileEnable'] = 'true'
    weather_path = os.path.join(DATA_DIR,'external/weather/weather_2015','18815_grid_39.875_lat.wea')
    #all_responses = pd.DataFrame(index = rows, columns = cols)
    all_responses = pd.DataFrame()
    for index, site in enumerate(SITES):
        #site_response = np.empty((len(DATES), len(RESPONSE_VARS)))
        exposure_filename = 'neonic_profile_' + site + '.csv'
        exposure_path = os.path.join(DATA_DIR,"processed/neonic_profiles/", exposure_filename)#os.path.abspath(os.path.join('data', exposure_filename))
        site_pars = generate_start(static_pars.copy(), site)
        site_pars['NecPolFileName'] = exposure_path
        site_pars['ICQueenStrength'] = 0
        site_pars['ICForagerLifespan'] = 0
        while not (1 <=site_pars['ICQueenStrength'] <= 5):
            site_pars['ICQueenStrength'] = np.random.normal(loc = float(parameters['ICQueenStrength_mean']), scale = float(parameters['ICQueenStrength_sd']))
        while not (4 <= site_pars['ICForagerLifespan'] <= 16):
            site_pars['ICForagerLifespan'] = np.random.normal(loc = float(parameters['ICForagerLifespan_mean']), scale = float(parameters['ICForagerLifespan_sd']))
        vp = VarroaPop(parameters = site_pars, weather_file = weather_path, verbose=False, unique=True, keep_files=save, logs=logs)
        vp.run_model()
        dates = DATES_STR.iloc[index,:]
        site_response = filter_site_responses(vp.get_output(), dates_str= dates)
        #print("Site {} responses: {}".format(site, site_response))
        start_row = len(DATES)*index
        end_row = start_row + len(DATES)
        #all_responses.loc[start_row:end_row,:] = site_responses
        #print("Site {}: {}".format(index,site_response))
        all_responses = all_responses.append(site_response, ignore_index=True)

    #Generate labels for rows and columns
    rows = ['_'.join(x) for x in product(SITES, DATES)]
    #print('Row labels: {}'.format(rows))
    response_cols = [x[0] for x in RESPONSE_VARS]
    cols = ['_'.join([x,y]) for y in ['mean', 'sd'] for x in response_cols]
    #print('Col labels: {}'.format(cols))

    all_responses['Index'] = pd.Series(rows) #Add our row labels
    all_responses.set_index("Index", inplace=True) #Set row labels to be the index
    #print('Final result: {}'.format(all_responses))
    filtered_resp = all_responses.loc[:,RESPONSE_FILTER] #Keep only the summary stats that we are using
    #print('Filtered result: {}'.format(filtered_resp))
    output_dict = {}
    for row in filtered_resp.index:
        for col in filtered_resp.columns:
            label = "_".join([row,col])
            output_dict[label] = filtered_resp.loc[row,col]
    #print('Output dictionary: {}'.format(output_dict))
    return output_dict


def filter_site_responses(output, dates_str):
    """
    From raw VarroaPop output, filter out the dates and response variables that we need for our summary stats.
    :param output: dataframe of VarroaPop raw outputs
    :param dates_str: Dates of the desired observations
    :return: a dataframe with raw response vars in the columns and dates in the rows
    """

    output.set_index('Date',inplace=True)
    #print('REP output: {}'.format(output))
    #print([output.loc[dates_str, cols[1]].sum(axis=1) for cols in RESPONSE_VARS])
    responses = [output.loc[dates_str, cols[1]].sum(axis=1) for cols in RESPONSE_VARS]
    #print(responses)
    col_names = [x[0] for x in RESPONSE_VARS]
    response_df = pd.DataFrame.from_items(zip(col_names,responses))
    #print(response_df)
    #print("REP filtered responses: {}".format(response_df))
    return response_df


def generate_start(pars, site):
    paras = pars.copy()
    df = pd.read_csv(INITIAL_DF)
    df['site'] = np.arange(1,11,1).astype("str")
    df.set_index('site', inplace=True)
    #print("Initial conditions: {}".format(df))
    paras["SimStart"] = START_DATES[int(site)-1] #convert site number to 0-9 list index
    vars = ['bees_cm2_4', 'capped_cm2_4', 'open_cm2_4', 'pollen_cm2_4', 'nectar_cm2_4'] #cols of initial conditions
    vals = df.loc[site,vars].copy()
    cells_per_cm2 = 3.96 #based on hex w flat to flat distance of 0.54 cm = cell vol of 0.2525 cm2
    paras['ICWorkerAdults'] = floor(vals[0] * 1.45) #number from Chia
    paras['ICDroneAdults'] = 0
    paras['ICWorkerBrood'] = floor(vals[1] * cells_per_cm2 * .66) #based on time in stage
    paras['ICDroneBrood'] = 0
    paras['ICWorkerLarvae'] = floor(vals[1] * cells_per_cm2 * .33) #based on time in stage
    paras['ICDroneLarvae'] = 0
    paras['ICWorkerEggs'] = floor(vals[2] * cells_per_cm2)
    paras['ICDroneEggs'] = 0
    paras['InitColPollen'] = floor(vals[3] * cells_per_cm2* 0.316 *1.45) #based off 0.316 cm3 cell volume and 1.45 g/cm3 pollen density
    paras['InitColNectar'] = floor(vals[4] * cells_per_cm2* 0.316 * 1.13) #based off 0.316 cm3 cell volume and 1.13 g/cm3 nectar density
    return paras


