#!/bin/env python

# Grab the current ocean temperature

import os
import sys
import time
import urllib2
import json
import socket


# http://tidesandcurrents.noaa.gov/stations.html?type=Physical+Oceanography
# http://tidesandcurrents.noaa.gov/map/index.shtml?type=MeteorologicalObservations
# Interesting station locations:
INTERESTING_STATIONS = {
    'La Jolla, CA': '9410230',
    'San Diego, CA': '9410170',        # Bay
    'Los Angeles, CA': '9410660',
    'Port San Luis, CA': '9412110',
    'San Francisco, CA': '9414290',
    'Garibaldi, OR': '9437540',        # Due West from Portland
    'Seattle, WA': '9447130',
    'Cleveland, OH': '9063063',
    'Springmaid Pier, SC': '8661070',  # Myrtle Beach
    'The Battery, NY': '8518750',      # NYC
    'Boston, MA': '8443970',
    'Lake Worth Pier, FL': '8722670',  # East coast, north of Miami
    'Naples, FL': '8725110',           # West coast
    'Shell Beach, LA': '8761305',      # Just outside of NO
    'Galveston Bay, TX': '8771341',    # Coast outside of Houston
    'Bob Hall Pier, TX': '8775870',    # Corpus Christi
}


class OceanTemps(object):
    """Retrieve Ocean Temperatures from NOAA.gov"""

    def __init__(self):
        pass

    def __get_station(self, name):
        """Find station ID from by loosely matching against keys in INTERESTING_STATIONS"""
        locations = INTERESTING_STATIONS.keys()
        for loc in locations:
            if name.lower() in loc.lower():
                return INTERESTING_STATIONS[loc]

    def __gen_url(self, date, station, units):
        """Generate the correct URL to retrieve for station

        Args:
            date (str): String date in the YYYYMMDD format
            station (str): ID location of station or name from INTERESTING_STATIONS
            units (str): Retrieve data in either "english" or "metric" values

        Returns:
            str: URL that you can retrieve for data on station
        """
        if not station.isdigit():
            station_id = self.__get_station(station)
            if station_id:
                station = station_id

        URL = 'http://tidesandcurrents.noaa.gov/api/datagetter?product=water_temperature' + \
              '&application=NOS.COOPS.TAC.PHYSOCEAN' + \
              '&begin_date=%s&end_date=%s' + \
              '&station=%s' + \
              '&time_zone=GMT' + \
              '&units=%s' + \
              '&interval=6' + \
              '&format=json'
        return URL % (date, date, station, units)

    def get_ocean_temps(self, station, date=None, units="english"):
        """Retrieve Ocean Tempuratures for station

        Args:
            station (str): Station name or ID
            date (str|None): Date in YYYYMMDD form or None to default to today
            units (str): Units specified in either "english" or "metric"

        Returns:
            tuple(location, list(dict)): Location is the station choosen,
                dictionary items are temperature readings with keys: 'f'
                for ??, 't' for datetime, 'v' for temperature value. All
                values are strings.
            E.g. (u'San Diego',
                   [{u'f': u'0,0,0', u't': u'2015-03-27 00:00', u'v': u'69.8'},
                    {u'f': u'0,0,0', u't': u'2015-03-27 00:06', u'v': u'69.8'},
                    {u'f': u'0,0,0', u't': u'2015-03-27 00:12', u'v': u'69.8'},
                    {u'f': u'0,0,0', u't': u'2015-03-27 00:18', u'v': u'69.8'},
                    ...
                    {u'f': u'0,0,0', u't': u'2015-03-27 19:18', u'v': u'68.9'},
                    {u'f': u'0,0,0', u't': u'2015-03-27 19:24', u'v': u'69.1'}])
        """
        if date is None:
            date = time.strftime('%Y%m%d')

        cache_file = '/tmp/ocean-temp_%s.cache' % station
        try:
            URL = self.__gen_url(date, station, units)
            data = urllib2.urlopen(URL, timeout=3).read()

        except (socket.timeout, urllib2.HTTPError):
            if os.path.exists(cache_file):
                data = open(cache_file).read()
            else:
                # Simulate what we'd get from NOAA.gov
                data = '{"error": {"message": "We are out of luck here"}}'

        jsondata = json.loads(data)
        if 'error' in jsondata:
            return "location", [{'v': '??'}]

        # Cache our results in case next time we can not retrieve it again
        with open(cache_file, 'w') as cache:
            cache.write(data)

        # Filter out non-values
        temps = [dp for dp in jsondata['data'] if dp[u'v']]

        # Ensure the returned data set is sorted by datetime(t)
        return jsondata['metadata']['name'], sorted(temps, key=lambda x: x[u't'])


if __name__ == "__main__":

    default_location = "La Jolla"
    if len(sys.argv) > 1:
        default_location = sys.argv[1]

    location, ocean_temps = OceanTemps().get_ocean_temps(default_location)
    print 'Ocean Temp @ %s is %s' % (location, ocean_temps[-1]['v'])
