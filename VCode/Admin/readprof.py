##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2000, National Research Council of Canada
#
##############################################################################

"""utility to simplify reading in multiple profile files generated by
using the -p option to new_test, new_server, or wxMediator
"""

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
    l = os.listdir(med)
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


        
