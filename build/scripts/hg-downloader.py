#!/usr/bin/env python

from mercurial import hg
import urllib2, re

# every command must take a ui and and repo as arguments.
# opts is a dict where you can find other command line flags
#
# Other parameters are taken in order from items on the command line that
# don't start with a dash.  If no default value is given in the parameter list,
# they are required.
#
# For experimenting with Mercurial in the python interpreter:
# Getting the repository of the current dir:
#    >>> from mercurial import hg, ui
#    >>> repo = hg.repository(ui.ui(), path = ".")

def download(ui, repo, url, **opts):
    """Download a URL to the current directory."""

    if len(url) <= 0:
        ui.warn("Please specify a url to download.\n")
        return
    ui.status('Downloading "%s"\n' % url)
    remote_file = urllib2.urlopen(url)
    match = re.match(r'.*/([^?]*).*', url)
    if match is None:
        ui.warn("Don't know how to name the output file.\n")
        return
    filename = match.group(1)    
    local_file = open(filename, 'wb')
    local_file.write(remote_file.read())
    local_file.close()
    remote_file.close()
    ui.status('Finished download.\n')

cmdtable = { "download": (download, [], "<url>") }
