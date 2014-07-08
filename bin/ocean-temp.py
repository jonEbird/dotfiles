#!/bin/env python

# Grab the current ocean temperature

import os
import sys
import urllib2
import json

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
    fulldata = json.loads(urllib2.urlopen(URL).read())
    # Filter out non-values
    temps = [ dp for dp in fulldata['data'] if dp[u'v'] ]
    # Ensure the returned data set is sorted by datetime(t)
    return fulldata['metadata']['name'], sorted(temps, key=lambda x: x[u't'])

if __name__ == "__main__":

    import time

    location, ocean_temps = get_ocean_temps(time.strftime('%Y%m%d'))
    print 'Ocean Temp @ %s is %s' % (location, ocean_temps[-1]['v'])
