#!/usr/bin/env python

import urllib2, re

def download(url):
    '''Download a URL to the current directory.'''

    if len(url) <= 0:
        raise Exception('Please specify a url to download.')
    print('Downloading "%s"' % url)
    remote_file = urllib2.urlopen(url)
    match = re.match(r'.*/([^?]*).*', url)
    if match is None:
        raise Exception("Don't know how to name the output file.")
    filename = match.group(1)
    local_file = open(filename, 'wb')
    local_file.write(remote_file.read())
    local_file.close()
    remote_file.close()
    print('Finished download.')
