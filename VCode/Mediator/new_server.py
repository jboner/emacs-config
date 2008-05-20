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
import tcp_server

import exceptions
import natlink, os, posixpath, pythoncom, re, select, socket
import SocketServer, string, sys, threading, time, whrandom, win32event

import AppStateEmacs, AppStateMessaging, auto_test, debug
import messaging, Object
import AppMgr, RecogStartMgr, SourceBuffMessaging, sb_services
import sr_interface, util

import regression


# Uncomment this and add some entries to active_traces if you want to 
# activate some traces.
debug.config_traces(status="on", 
                    active_traces={
#                       'NewMediatorObject': 1,
#                       'PersistentConfigNewMediator': 1,
#                       'TempConfig': 1,
#                       'CmdInterp': 1,
#                       'SymDict': 1,
#                       'RecogStartGram': 1,
#                       'RecogStartMgr': 1,
#                       'RecogStartMgrNL': 1,
#                       'TempConfigNewMediatorFactory': 1,
#                        'sr_interface.addWord': 1,
#                        'CmdInterp.add_lsa': 1,
#                        'LSAlias.__init__': 1,
#                       'recent_dictation': 1,
#                       'safe_depth': 1,
#                       'ResMgrBasic.interpret_dictation': 1,
#                       'send_mess': 1,
#                       'get_mess': 1,
#                       'AppState': 1,
#                       'SourceBuff': 1,
#                       'MessPackager_FixedLenSeq.pack_mess': 1,
#                       'RecogStartMgr': 1,
#                       'DictWinGramNL': 1,
#                       'sr_grammarsNL': 1,
#                       'sr_grammars': 1,
#                       'line_num_of': 1,
#                       'lines_around_cursor': 1,
#                       'get_pos_selection': 1,
#                       'cur_pos': 1,
#                       'get_selection': 1,
#                       'sr_interface': 1,  
#                        'set_text': 1,
#                       'SelectWinGramNL': 1,
#                       'SimpleSelection': 1,  
#                       'SelectWinGram': 1,
#                       'SelectWinGramDummy': 1,
#                      'CmdInterp.is_spoken_LSA': 1
#                       'NewMediatorObject': 1,
#                       'OwnerObject': 1
#                      'init_simulator_regression': 1,
#                      'ResMgrBasic': 1,
#                       'BasicCorrectionWinGram': 1,
#                      'RSMInfrastructure': 1,
#                      'WinGramMgr': 1,
#                      'CmdInterp.interpret_NL_cmd': 1                      
#                      'synchronize': 1,
#                      'insert_indent': 1,
#                      'get_selection': 1,
#                      'set_selection_cbk': 1,
#                      'goto_cbk': 1,
#                      'listen_one_transaction': 1,
#                                    'SourceBuff.on_change': 1
#                                   'AppState.synchronize_with_app': 1,
#                          'AppState.close_all_buffers': 1,
#                         'SourceBuff': 1,
#                                   'SourceBuffMessaging.line_num_of': 1,
#                                    'delete_instance_cbk': 1,
#                                    'listen_one_transaction': 1,
#                                    'close_app_cbk': 1,
#                                    'AppState': 1,
#                       'SimCmdsObj.say': 1,
#                       'got': 1,
#                       'RecogStartGram': 1,
#                       'DictWinGramNL.gotResultsObject': 1,
#                       'SelectWinGramNL.gotResultsObject': 1,
#                       'sr_interface.CommandDictGrammar': 1,
#                     'sr_interface.CodeSelectGrammar': 1,
#                       'SelectWinGram': 1,
                                   },
                                   allow_trace_id_substrings = 1)

#debug.config_traces(status="on", active_traces={'CmdInterp':1, 'sr_interface': 1, 'get_mess':1, 'send_mess': 1, 'sim_commands': 1}, allow_trace_id_substrings = 1)
#debug.config_traces(status="on", active_traces = 'all')
#debug.config_traces(status="on", active_traces = {'sr_interface':1},
#allow_trace_id_substrings = 1)

#
# Port numbers for the communication link
#
VC_LISTEN_PORT = 45770
VC_TALK_PORT = 45771

class ExtLoopWin32NewMediator(tcp_server.ExtLoopWin32):
    """implementation of ExtLoopWin32 for ServerNewMediator 

    **INSTANCE ATTRIBUTES**

    *ServerNewMediator the_server* -- the underlying server

    *NewMediatorObject the_mediator* -- the mediator object

    STR *test_suite=None* -- name of regression test suite to run

    *BOOL okay* -- true if the configuration was successful
    """
    def remove_other_references(self):
# for now, quit first, then cleanup (including server owned by the
# NewMediatorObject)
#        print 'about to quit the mediator'
        self.the_server = None
        self.the_mediator.quit(save_speech_files=0, 
            disconnect=1)
        Object.OwnerObject.remove_other_references(self)

    def __init__(self, test_suite = None, profile_prefix = None, 
        bypass_sr_recog = 0, num_words_training = 0, **args_super):
        """
        **INPUTS**

        STR *test_suite=None* -- name of regression test suite to run

        *STR profile_prefix* -- prefix for filename for output of profiler,
        or None if not profiling (ignored if test_suite is None) 

        *BOOL bypass_sr_recog* -- when testing, bypass natlink for 
        dictation utterances (ignored if test_suite is None) 
        """

        self.deep_construct(ExtLoopWin32NewMediator, 
                            {
                             'the_server': None,
                             'test_suite': test_suite,
                             'the_mediator': None
                            }, 
                            args_super)
        self.add_owned('the_mediator')
        factory = tcp_server.AppStateFactorySimple()
        data_events = \
            tcp_server.DataEvtSourceWin32(self.evt_sockets_ready)
        test_server = not (test_suite == None)
        self.the_server = \
            tcp_server.ServerNewMediator(data_events = data_events,
                                         test_server = test_server,
                                         editor_factory = factory) 

#        print self.the_server
        self.the_mediator = \
            NewMediatorObject.NewMediatorObject(server = self.the_server,
                test_or_suite = test_suite,
                global_grammars = 1, exclusive = 1,
                profile_prefix = profile_prefix, 
                bypass_sr_recog = bypass_sr_recog,
                num_words_training = num_words_training)
                
                
#        print self.the_mediator.server
        sys.stderr.write('Configuring the mediator...\n')
        sys.stderr.flush()
        self.okay = self.the_mediator.configure() and self.the_mediator.ready()
        if not okay:
            sys.stderr.write('Mediator configuration failed...exiting\n')
#        print self.the_mediator.server
        sys.stderr.write('Finished ExtLoop init...\n')
        sys.stderr.flush()

    def run(self):
        """Start the server as well as the ExtLoopWin32 message loop.
        """
        if self.okay:
            ExtLoopWin32.run()

    def server(self):
        """returns a reference to the server

        **INPUTS**

        *none*

        **OUTPUTS**

        *ServerOldMediator* -- the underlying server
        """
        return self.the_server


##############################################################################
# start test standalone server
##############################################################################
def run_new_server(test_suite=None, profile_prefix = None, 
    bypass_sr_recog = 0, num_words_training=0, extra_opts = None):
    """Start a ServerNewMediator/ServerMainThread with external message 
    loop using win32event and the new NewMediatorObject
    """

    sys.stderr.write('running ExtLoopWin32NewMediator with ServerNewMediator\n')
    print 'running ExtLoopWin32NewMediator with ServerNewMediator'
    try:
        a_loop = ExtLoopWin32NewMediator(test_suite = test_suite, 
            profile_prefix = profile_prefix, 
            bypass_sr_recog = bypass_sr_recog,
            num_words_training = num_words_training) 
    except:
        return

    sys.stderr.write('Running ExtLoopWin32...\n')
    a_loop.run()
#    sys.stderr.write("run_ext_server finishing\n")
    

def help():
    print """
Usage: python new_server.py [OPTIONS]

Runs the VoiceCode TCP server.

When this server is running, external editors can connect to VoiceCode through
TCP connections on the VC_LISTEN (45770) and VC_TALK (45771) ports.

OPTIONS
-------

-h :

   print this help message.

    
-t testSuite:

   Upon connection by a new external editor, run regression test suite
   *testSuite* on that external editor.

   (Default: None)

-0:
   Close the server when the last external editor disconnects.
   Currently ignored except for ServerOldMediator


-p pfile : profile the code, writing the output of the python profiler
           to pfile (see Python Profiler in the Python library manual)

--bypass : bypass natlink for dictation utterances (used for profiling)
    """


if __name__ == '__main__':
    opts, args = util.gopt(['h', None, 't=', None, '0', None,
    'bypass', 0, 'p=', None, 'train=', 0])
    non_exclusive_opts = ['0']
    
#    sr_interface.connect()

    #
    # Create a global grammar manager
    #
#    the_recog_start_mgr = RecogStartMgr.RecogStartMgr()


    sys.stderr = sys.stdout
    #
    # Start servers on the VC_LISTEN and VC_TALK ports
    #
    extra_opts = {}
    for opt in non_exclusive_opts:
        try:
            extra_opts[opt] = opts[opt]
        except KeyError:
            pass

    run_new_server(test_suite=opts['t'], profile_prefix = opts['p'],
        bypass_sr_recog = opts['bypass'], 
        num_words_training = int(opts['train']), 
        extra_opts = extra_opts)

#    sys.stderr.write("run finished\n")

# DCF - NewMediatorObject.quit does this, at the direction of
# ExtLoopWin32NewMediator
#    sr_interface.disconnect()
#    sys.stderr.write("disconnected from natlink\n")


