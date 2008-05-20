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

"""Various global functions and constants relevant to VoiceCode

The following variables are defined in this module

*STR* home -- path of the home directory for VoiceCode (*VCODE_HOME* environment variable(

*STR* data -- path of the data directory

*STR* test_data -- path of the test data directory

*STR* config -- path of the configuartion directory

"""

import os, sys, copy
#
# special error for deprecated things
#
class DeprecationError(Exception):
    pass


#
# Various directories
#
home = os.environ['VCODE_HOME']
admin = os.path.join(home, 'Admin')
unit_tests_dir = os.path.join(admin, 'UnitTests')
config = os.path.join(home, 'Config')
data = os.path.join(home, 'Data')
mediator_dir = os.path.join(home, 'Mediator')
state = os.path.join(data, 'State')
tmp = os.path.join(data, 'Tmp')
test_data = os.path.join(data, 'TestData')
benchmark_dir = os.path.join(data, 'Benchmark')
sample_config = os.path.join(config, 'Samples')

#What Can I Say:
wcisay_html_folder = os.path.join(data, 'whatCanISay')
wcisay_test_folder = os.path.join(benchmark_dir, "WhatCanISayTestResults")

doc = os.path.join(home, 'Doc')
doc_modules = os.path.join(doc, 'Modules')

default_config_file = os.path.join(config, 'vc_config.py')
default_user_config_file = os.path.join(config, 'user_config.py')
regression_user_config_file = os.path.join(config, 'regression_config.py')
#sym_state_file = os.path.join(state, 'symdict.dat')
sym_state_file = os.path.join(state, 'symdict.dict')

# Add some paths to $PYTHONPATH
for p in [admin, config, mediator_dir]:
    if p not in sys.path:
        sys.path.insert(0, p)


# max_all_languages is all the languages that are supported,
# all_languages can be defined in user_globals, restricting the
# size of your voicecoder session...
max_all_languages = ['C', 'python', 'perl', 'javascript', 'php', 'java', 'scala']
max_all_languages.sort()
max_all_languages = tuple(max_all_languages)
try:
    from user_globals import all_languages
except ImportError:
    all_languages = list(max_all_languages)
all_languages.sort()
all_languages = tuple(all_languages)


# likewise c_style_languages can be restricted in the user_globals:
all_c_style_languages = ['C', 'perl', 'javascript', 'php', 'java', 'scala']
all_c_style_languages.sort()
c_style_languages = tuple([c for c in all_c_style_languages if c in all_languages])
del all_c_style_languages
    
