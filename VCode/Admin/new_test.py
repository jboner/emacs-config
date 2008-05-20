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

"""Regression testing script"""

import os, natlink, posixpath, sys
import debug, sim_commands, sr_interface, vc_globals
import regression
import NewMediatorObject
import EdSim

sys.path = sys.path + [vc_globals.config, vc_globals.admin]

# Uncomment this and add some entries to trace_what if you want to 
# activate some traces.
#debug.config_traces(status="on", trace_what={})

debug.config_traces(status="on", 
                    active_traces={
#                      'NewMediatorObject': 1,
#                      'SymDict': 1,
#                      'SymDict._add_corresponding_expansion': 1,
#                      'CmdInterp': 1,
#                       'StateStackBasic': 1,
#                       'BufferStateBasic': 1,
#                      'CmdInterp': 1
#                      'CmdInterp': 1
#                      'DictWinGramNL': 1,
#                      'SourceBuff.print_buff': 1, 
#                      'ResMgr': 1,
#                      'mediator.say': 1
#                      'StateStack': 1,
#                      'SourceBuffEdSim.restore_state': 1,
#                      'BufferStates': 1
#                       'CmdInterp.interpret_NL_cmd': 1
#                       'OwnerObject': 1
#                      'RecogStartMgr': 1,
#                      'init_simulator_regression': 1
#                      'WinGramMgr': 1,
#                      'synchronize': 1,
#                      'insert_indent': 1,
#                      'get_selection': 1,
#                      'set_selection_cbk': 1,
#                      'goto_cbk': 1,
#                      'listen_one_transaction': 1
#                                    'SourceBuff.on_change': 1
#                                   'get_mess':1, 
#                                   'send_mess': 1,
#                                   'AppState.synchronize_with_app': 1,
#                                   'SourceBuff': 1,
#                                   'SourceBuffMessaging.line_num_of': 1,
#                                    'delete_instance_cbk': 1,
#                                    'listen_one_transaction': 1,
#                                    'close_app_cbk': 1,
#                                    'AppState': 1
      'now_you_can_safely_put_a_comma_after_the_last_entry_above': 0
                                   },
                                   allow_trace_id_substrings = 1)


import auto_test, util


def usage():
    print """

Usage: python test.py -h suite-name -s
                      -d output1 output2
                      

OPTIONS
-------

-h       : print this help message

-d       : instead of doing tests, compare the outputs of two tests runs.
           Typically used to compare output of a test run done on a new
           (and possibly buggy) version of the system to output of a test run
           done on a bug-free (yeah, right ;-) version of the system.

-p pfile : profile the code, writing the output of the python profiler
           to pfile (see Python Profiler in the Python library manual)

--bypass : bypass natlink for dictation utterances (used for profiling)

ARGUMENTS
---------

suite-name : name of a test or test suite

output1, output2 :
             two test run ouput files to be compared
    """


if (__name__ == '__main__'):
    opts, args = util.gopt(('d', None, 
        'bypass', None,
        'h', None, 
        'p=', None,
        'train=', 0))

    if (opts['h']) or len(args) == 0:
        usage()
    elif (opts['d']):
        print "-d option not implemented yet.\n"
    else:
        the_mediator = \
            NewMediatorObject.NewMediatorObject(
                test_or_suite = args[0],
                global_grammars = 1, exclusive = 1, 
                profile_prefix = opts['p'],
                bypass_sr_recog = opts['bypass'],
                num_words_training = int(opts['train']))
        sys.stderr.write('Configuring the mediator...\n')
        if the_mediator.configure():
            sys.stderr.write('Finished configuring...\n')
            ed = EdSim.EdSim()
            the_mediator.new_editor(ed, server = 0, check_window = 0, 
                test_editor = 1)
        the_mediator.quit(save_speech_files=0, 
            disconnect=1)
        the_mediator.cleanup()



