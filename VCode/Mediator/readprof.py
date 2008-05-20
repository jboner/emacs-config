import re
import pstats
import os
import os.path
vc = os.environ['VCODE_HOME']
med = os.path.join(vc, 'Mediator')
pj = os.path.join

def read_all(prefix, dir = None):
    if dir is None:
        dir = med
    l = os.listdir(dir)
    pfiles = {}
    for file in l:
        s = "%s\.(.*)\.dat" % prefix
        m = re.match(s, file)
        if m:
            pfiles[m.group(0)] = file
            print file
    profiles = {}
    cumulative = None
    for test, file in pfiles.items():
        full_name = os.path.join(dir, file)
        profiles[test] = pstats.Stats(full_name)
        if not cumulative:
            cumulative = pstats.Stats(full_name)
        else:
            cumulative.add(full_name)
        profiles[test].strip_dirs()
        profiles[test].sort_stats('cumulative')
    cumulative.strip_dirs()
    cumulative.sort_stats('cumulative')
    return profiles, cumulative


        
