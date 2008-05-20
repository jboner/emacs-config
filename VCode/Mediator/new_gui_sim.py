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
# (C)2002, National Research Council of Canada
#
##############################################################################

"""A NewMediatorObject-based VoiceCode server that uses TCP/IP based 
messaging protocol to communicate with external editors.
"""

import vc_globals

import NewMediatorObject

import WaxEdSim
import WaxEdSimSpeech
import AppStateGenEdit


import sim_commands

from MediatorConsoleWX import MediatorConsoleWX

import os, posixpath, re
import string, sys, time

import debug
import Object
import sr_interface, util

import auto_test

from wxPython.wx import *

import SaveSpeech
from wxMediator import *




# Uncomment this and add some entries to active_traces if you want to 
# activate some traces.
debug.config_traces(status="on", 
                    active_traces={
#                        'interp_massaged':1,
#                        'match_untranslated_text': 1,
#                        'SB_ServiceIndent':1,
#                        'ActionInsertNewClause':1,
#                        'sr_interface.connect':1,
#      'SimpleSelection':1,
#                        'wxMediator': 1,
#                        'send_mess': 1,
#                        'get_mess': 1,
#                        'RSMInfrastructure': 1,
#                        'RecogStartMgr': 1,
#                        'ResMgr': 1,
#                      'SelectWinGram': 1,
#                      'DictWinGram': 1,
#                        'GramMgr': 1,
#                        'BasicCorrectionWinGram': 1,
#                      'CmdInterp.is_spoken_LSA': 1
#                       'NewMediatorObject': 1,
#                       'OwnerObject': 1
#                      'init_simulator_regression': 1,
#                      'WinGramMgr': 1,
#                      'CmdInterp.interpret_NL_cmd': 1
#                      'synchronize': 1,
#                      'insert_indent': 1,
#                      'get_selection': 1,
#                      'set_selection_cbk': 1,
#                      'goto_cbk': 1,
#                      'listen_one_transaction': 1
#                                    'SourceBuff.on_change': 1
#                                   'get_mess':1, 
#                                   'send_mess': 1,
#                                   'RecogStartMgr': 1
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

#debug.config_traces(status="on", active_traces={'CmdInterp':1, 'sr_interface': 1, 'get_mess':1, 'send_mess': 1, 'sim_commands': 1}, allow_trace_id_substrings = 1)
#debug.config_traces(status="on", active_traces = 'all')
#debug.config_traces(status="on", active_traces = {'sr_interface':1},
#allow_trace_id_substrings = 1)

class wxMediatorSim(WaxEdSim.WaxEdSimBase, wxMediator):
    """wxMediator subclass for use with an internal WaxEdSim editor
    simulator

    **INSTANCE ATTRIBUTES**

    *WaxEdSimConsoleSpeech wax_console* -- the WaxEdit console
    """
    def __init__(self, **args):
        self.decl_attrs({'wax_console': None})
        self.deep_construct(wxMediatorSim, 
                            {
                            }, 
                            args)
        self.add_owned('wax_console')
        sr_interface.set_change_callback(mic_change_callback = self.mic_change)
        app = AppStateGenEdit.AppStateGenEdit(self.wax_console)
        if not self.the_mediator.new_editor(app, server = 0, check_window = 1):
            sys.stderr.write('NewMediatorObject.new_editor failed\n')
            app.cleanup()
            self.wax_console.on_exit()
#        wxInitAllImageHandlers()

# get actual namespace used by panel (which is a copy of the names we
# passed to WaxEdSim)
        actual_names = self.wax_console.access_command_space()

        commands = sim_commands.SimCmdsObj(app, 
            self.the_mediator.interpreter(), actual_names)
# add bound methods of the commands SimCmdsObj instance, corresponding
# to the functions in the sim_commands module, to this 
# namespace 
        commands.bind_methods(actual_names)



    def main_frame(self):
        """returns a reference to the main frame of the wxMediator
        application

        **INPUTS**

        *none*

        **OUTPUTS**

        *wxFrame* -- the main wxFrame
        """
        return self.wax_console.initial_frame()

    def create_main(self):
        """create the main frame window for the mediator, but do not
        show it
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**

        *none*
        """
        self.wax_console = WaxEdSimSpeech.WaxEdSimConsoleSpeech(app = self, 
            app_name = 'WaxEdSim',
            command_space = self.command_space, curr_dir =
            vc_globals.test_data, show = 0)

    def create_server(self, test_server):
        """create the TCP server for the mediator, if running in server
        mode, and call hook_events, but do not start the server yet.
        
        **INPUTS**
        
        *BOOL test_server* -- true if the mediator has been started with
        a test suite and the server should listen for connections from a
        test client
        
        **OUTPUTS**

        *ServerNewMediator* -- the server, or None if we are running
        with an internal test editor instead
        """
        return None
    

##############################################################################
def run():
    """Start a ServerNewMediator/ServerMainThread with external message 
    loop using wxWindows events and the new NewMediatorObject
    """

    sys.stderr.write('creating wxMediator\n')
    app = wxMediatorSim()
    sys.stderr.write('starting...\n')
    app.run()
#    sys.stderr.write("run_ext_server finishing\n")
    

def help():
    print """
Usage: python new_gui_sim.py [OPTIONS]

Runs the VoiceCode GUI mediator with TCP server.

When this server is running, external editors can connect to VoiceCode through
TCP connections on the VC_LISTEN (45770) and VC_TALK (45771) ports.

OPTIONS
-------

-h :

   print this help message.

    """


if __name__ == '__main__':
    opts, args = util.gopt(['h', None])
    
#    sr_interface.connect()

    #
    # Create a global grammar manager
    #
#    the_recog_start_mgr = RecogStartMgr.RecogStartMgr()


    if opts['h']:
        help()
    else:
        run()



# defaults for vim - otherwise ignore
# vim:sw=4

