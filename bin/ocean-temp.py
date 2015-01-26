#!/bin/env python

# Grab the current ocean temperature

import os
import sys
import urllib2
import json
import socket

# http://tidesandcurrents.noaa.gov/stations.html?type=Physical+Oceanography
# http://tidesandcurrents.noaa.gov/map/index.shtml?type=MeteorologicalObservations
# Interesting station locations:
# 9410230 - La Jolla, CA
# 9410170 - San Diego, CA (Bay)
# 9410660 - Los Angeles, CA
# 9412110 - Port San Luis, CA
# 9414290 - San Francisco, CA
# 9437540 - Garibaldi, OR (Due West from Portland)
# 9447130 - Seattle, WA
# 9063063 - Cleveland, OH
# 8661070 - Springmaid Pier, SC (Myrtle Beach)
# 8518750 - The Battery, NY (NYC)
# 8443970 - Boston, MA
# 8722670 - Lake Worth Pier, FL (East coast, north of Miami)
# 8725110 - Naples, FL (West coast)
# 8761305 - Shell Beach, LA (Just outside of NO)
# 8771341 - Galveston Bay, TX (Coast outside of Houston)
# 8775870 - Bob Hall Pier, Corpus Christi, TX


def gen_url(date, station="9410170", units="english"):
    """ Generate the correct URL to retrieve. You just supply the date in a
    YYYYMMDD format and optionally override the station location and
    units"""
    URL = 'http://tidesandcurrents.noaa.gov/api/datagetter?product=water_temperature' + \
          '&application=NOS.COOPS.TAC.PHYSOCEAN' + \
          '&begin_date=%s&end_date=%s' + \
          '&station=%s' + \
          '&time_zone=GMT' + \
          '&units=%s' + \
          '&interval=6' + \
          '&format=json'
    return URL % (date, date, station, units)

def get_ocean_temps(date, station="9410170", units="english"):
    URL = gen_url(date, station, units)
    cache_file = '/tmp/ocean-temp_%s.cache' % station
    try:
        data = urllib2.urlopen(URL, timeout=3).read()
        with open(cache_file, 'w') as cache:
            cache.write(data)
    except socket.timeout:
        if os.path.exists(cache_file):
            data = open(cache_file).read()
        else:
            return "location", [{'v': '??'}]
    fulldata = json.loads(data)
    # Filter out non-values
    temps = [ dp for dp in fulldata['data'] if dp[u'v'] ]
    # Ensure the returned data set is sorted by datetime(t)
    return fulldata['metadata']['name'], sorted(temps, key=lambda x: x[u't'])

if __name__ == "__main__":

    import time
    default_station = "9410230"
    if len(sys.argv) > 1:
        default_station = sys.argv[1]

    location, ocean_temps = get_ocean_temps(time.strftime('%Y%m%d'), default_station)
    print 'Ocean Temp @ %s is %s' % (location, ocean_temps[-1]['v'])
