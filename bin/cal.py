#!/home/jsmiller/venv/cal/bin/python

import re
import sys
from datetime import datetime, timedelta
from contextlib import contextmanager

import pytz
from pyexchange import Exchange2010Service, ExchangeNTLMAuthConnection


def get_password():
    """ Retrieve password from KeyringManager """
    try:
        from getmail import KeyringManager
        km = KeyringManager()
        return km.get_pass('jsmiller', 'qcmail1.qualcomm.com')
    except ImportError:
        import getpass
        password = getpass.getpass('Password: ')
        return password


@contextmanager
def stderr_off():
    import sys
    stderr_orig = sys.stderr
    sys.stderr = open('/dev/null', 'w')
    yield
    sys.stderr = stderr_orig


def strike(text):
    #return u'\u0336'.join(text.encode('utf-8'))
    #return u'\u0336'.join(text)
    return '-%s-' % text


def pformat_list(l, join=' ', indent=1, width=80, maxlines=0, formatter=None):
    """Pretty format a list of strings to a more suitable version for printing

    Args:
      l (list): List of elements to be formatted
      join (str): String to be used to join list elements
      indent (int): How many spaces to indent after newlines
      width (int): Maximum length each formatted line should take
      maxlines (int): Maximum number of lines to produce
      formatter (fun): Function to be used to format list elements
    Returns:
      str: Formatted string
    """
    if formatter is not None:
        l = map(formatter, l)

    result = ''
    cur_len = 0
    lines = 1
    for item in l:
        # Using str(item) can periodically throw UnicodeEncodeError whereas
        # this '%s' hack doesn't. Don't know why. FYI, "bad" char was u'\u2013'
        newtxt = '%s%s' % (join, item)

        # Will newtxt push us over the limit?
        if cur_len + len(newtxt) > width:
            # Will we be exceeding the maxlines limit?
            if maxlines and lines >= maxlines:
                result += '...'
                break
            result += '%s\n%s' % (join, ' ' * indent)
            cur_len = indent
            lines += 1
            newtxt = newtxt.replace(join, '', 1)
        result += newtxt
        cur_len += len(newtxt)

    return result.lstrip(join)


class MeetingFormatter(object):
    """Help format Exchange meeting objects for printing"""
    many_people = 15

    def __init__(self, style="normal", tz="US/Pacific"):
        self.style = style
        self._local_tz = pytz.timezone(tz)

    def _normalize_name(self, name):
        """Return a normalized "First Last" version of name"""
        if not isinstance(name, str):
            # Must be passing a pyexchange.base.calendar.ExchangeEventResponse
            name = name.name

        # Handle a special case with Active Directory / LDAP styled names
        if 'cn=' in name.lower():
            # Example: '/O=QUALCOMM/OU=SAN DIEGO ADMIN GROUP/CN=RECIPIENTS/CN=DGOLD'
            name = name.lower().split('cn=')[-1]
            import pwd
            try:
                p = pwd.getpwnam(name)
                name = p.pw_gecos
            except KeyError:
                pass

        # Remove any parenthesis of extra information
        name = re.sub(r' *\(.*?\)', '', name)

        # Finally return name in "given surname" order instead of "surname, given"
        return ' '.join(reversed(name.split(', ', 1)))

    def _pformat_event_normal(self, event):
        txt = "{start}-{stop}: {subject} @ {location}\n{attendees}\n".format(
            start=event.start.astimezone(self._local_tz).strftime('%H:%M'),
            stop=event.end.astimezone(self._local_tz).strftime('%H:%M'),
            subject=event.subject,
            location=event.location,
            attendees=pformat_list(event.attendees, join='; ', indent=2,
                                   formatter=self._normalize_name),
        )
        return txt

    def _pformat_event_conky(self, event):
        heading = ''
        body = ''

        # Format the time
        start = event.start.astimezone(self._local_tz)
        stop = event.end.astimezone(self._local_tz)
        if stop - start < timedelta(minutes=10) and not event.attendees:
            if 'reminder' not in event.subject.lower():
                heading += 'Reminder:'
        else:
            heading += '%s-%s:' % (start.strftime('%H:%M'), stop.strftime('%H:%M'))

        # Add subject
        if 'Canceled:' in event.subject:
            return '%s\n\n' % \
                pformat_list([heading] + strike(event.subject).split(), width=60)
        else:
            # heading = pformat_list([time_s] + event.subject.split(), width=60)
            heading += ' %s' % event.subject

        # Add location (if any)
        if event.location:
            heading += ' @ %s' % event.location

        # Done with the heading, now format it
        heading = pformat_list(heading.split(), width=60)

        # Finally add attendees
        if event.attendees:
            if len(event.attendees) >= self.many_people:
                body += '  Many Attendees (%d)' % len(event.attendees)
            else:
                body += '  %s' % pformat_list(event.attendees, join='; ',
                                              indent=2, width=60, maxlines=4,
                                              formatter=self._normalize_name)
        if body:
            return '%s\n%s\n\n' % (heading, body)
        else:
            return '%s\n\n' % (heading)

    def pformat_events(self, events):
        result = ''
        if self.style == "normal":
            enotes = ''.join(map(self._pformat_event_normal, events))
            result = enotes if enotes else 'No meetings'
        elif self.style == "conky":
            enotes = ''.join(map(self._pformat_event_conky, events))
            result = enotes if enotes else 'None'
        return result


if __name__ == '__main__':

    try:
        mode = sys.argv[1]
    except IndexError:
        mode = 'normal'

    URL = 'https://mymail.qualcomm.com/EWS/Exchange.asmx'
    USERNAME = r'NA\jsmiller'
    PASSWORD = get_password()

    # Set up the connection to Exchange
    connection = ExchangeNTLMAuthConnection(url=URL,
                                            username=USERNAME,
                                            password=PASSWORD)

    service = Exchange2010Service(connection)

    my_calendar = service.calendar()

    local_tz = pytz.timezone("US/Pacific")

    now = datetime.now()
    this_morning = datetime(now.year, now.month, now.day, 0, 1, 0)
    this_evening = datetime(now.year, now.month, now.day, 23, 59, 0)
    with stderr_off():
        events = my_calendar.list_events(
            start=local_tz.localize(this_morning).astimezone(pytz.utc),
            end=local_tz.localize(this_evening).astimezone(pytz.utc),
            details=True
        )

    events.load_all_details()

    mf = MeetingFormatter(style=mode)
    print mf.pformat_events(events.events).encode('utf-8')
