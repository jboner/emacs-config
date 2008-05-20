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

import exceptions, os, sys
import time
import traceback
import messaging
import string
import debug
import util
import actions_gen, AppState, CmdInterp, cont_gen, CSCmd, Object, re, sr_interface, SymDict, vc_globals
import tcp_server
import AppMgr, RecogStartMgr, GramMgr
import MediatorConsole
import sr_grammars
import RecogStartMgrNL
import ResMgr
import sr_grammarsNL
import auto_test
import regression
import cPickle

"""Defines main class for the mediator.

**MODULE VARIABLES**


..[MediatorObject] file:///./MediatorObject.MediatorObject.html"""


def disconnect_from_sr(disconnect, save_speech_files):
    """Save SR user and disconnect from SR.
    **INPUTS**
        
    *BOOL* disconnect -- true iif should disconnect from SR 
        
    *BOOL* save_speech_files -- true iif should save user. If *None*, prompt user.

        
    **OUTPUTS**
        
    *none* -- 
    """

#    print '-- MediatorObject.disconnect_from_sr: disconnect=%s, save_speech_files=%s' % (disconnect, save_speech_files)
    #
    # Ask the user if he wants to save speech files
    #
    while save_speech_files == None:
        sys.stdout.write('Would you like to save your speech files (y/n)?\n> ')
        answer = sys.stdin.readline()
        answer = answer[:len(answer)-1]
       
        if answer == 'y':
            save_speech_files = 1
        elif answer == 'n':
            save_speech_files = 0
           
        if save_speech_files == None:
            print "\nPlease answer 'y' or 'n'."

    if save_speech_files and sr_interface.sr_user_needs_saving:
        print 'Saving speech files. This may take a moment ...'
        sr_interface.saveUser()
        print 'Speech files saved.'

    if disconnect: sr_interface.disconnect()
            
        

def do_nothing(*positional, **keywords):
    pass

class BadRelativeUtteranceIndex(exceptions.RuntimeError):
    """Raised when the relative index of an utterance is wrong.
    """
    def __init__(self, rel_index, reason):
       msg = "Relative utterance index %s was %s" % (rel_index, reason)
       RuntimeError.__init__(self, msg)
       self.msg = msg
       
class BadUtteranceId(exceptions.RuntimeError):
    """Raised when the ID of an utterance is wrong.
    """
    def __init__(self, utter_id, reason):
       msg = "Utterance ID %s was %s" % (utter_id, reason)
       RuntimeError.__init__(self, msg)
       self.msg = msg

class NewMediatorObject(Object.OwnerObject):
    """Main object for the mediator.

    Note: there will be one such object, even if multiple editor
    instances are being controlled through a VoiceCode mediator.
    
    **INSTANCE ATTRIBUTES**

    [AppMgr] *editors* -- class which manages editor instances and whe
    their corresponding AppState interfaces.

    [CmdInterp] *interp=CmdInterp.CmdInterp()* -- Command interpreter used to
    translate pseudo-code to native code.

    *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
    dialog/prompt.  Normally disabled except during regression
    testing, and even then, the persistent mediator object should
    be created symbol_match_dlg = 0, because if
    symbol_match_dlg_regression is set, the mediator object will 
    automatically enable the dialog when regression testing starts 
    and disable it afterwards.  

    *BOOL symbol_match_dlg_regression* -- use a CmdInterp with symbol match 
    dialog/prompt during regression testing, with both persistent
    and temporary mediator objects.

    [ServerNewMediator] *server* -- Server to listen for connections
    from new editors, or None if the mediator should operate only with
    an internal test editor

    *{STR: BOOL} external_editors* -- set of instance names
    of all external editors connected to the mediator via the server

    [MediatorConsole] *the_console* -- GUI console for viewing mediator
    status, allowing the user to close the mediator when it is running
    as a server, and for invoking the correction dialog boxes.  May be
    None if the mediator is not running in GUI mode

    *CLASS* wave_playback -- class constructor for a concrete
    subclass of WavePlayback, or None if no playback is available

    *CorrectUtteranceEvent correct_evt* -- doorbell used to send an
    event to bring up the correction box asynchronously.

    *CorrectRecentEvent correct_recent_evt* -- doorbell used to send an
    event to bring up the correct recent box asynchronously.
    
    *ReformatSymbolEvent reformat_recent_evt* -- doorbell used to send an
    event to bring up the reformat recent box asynchronously.

    *{STR:ANY} test_space* -- if the mediator is started in regression 
    testing mode, test_space is the namespace in which regression tests 
    have been defined and will run.  Otherwise, it should be None.

    *STR pickled_interp* -- CmdInterp as originally configured,
    pickled to speed up regression testing

    *BOOL global_grammars* -- should this instance use global grammars for 
    regression testing (ignored if test_space is None)

    *BOOL exclusive* -- should this instance use exclusive grammars for 
    regression testing (ignored if test_space is None or global_grammars
    is false)

    *STR profile_prefix* -- prefix for filename for output of profiler,
    or None if not profiling (ignored if test_space is None) 

    *BOOL bypass_sr_recog* -- when testing, bypass natlink for 
    dictation utterances (ignored if test_space is None) 
    
    *INT num_words_training* -- if > 0, generate some training files
    with that number of words.

    *TestSuite test_suite* -- test suite to run, or None if we are not going 
    to run regression tests using the next test editor to connect

    *BOOL testing* -- flag indicating that the mediator is currently
    running regression tests.

    *BOOL foreground_testing* -- flag indicating that the mediator is 
    currently running foreground regression tests.

    *STR config_file* -- file which was used to configure the mediator.
    This file will also be the default to use if reconfigure is called
    (usually only by init_simulator_regression during regression tests)

    *STR user_config_file* -- user-specific file which was used to 
    configure the mediator.
    This file will also be the default to use if reconfigure is called
    (usually only by init_simulator_regression during regression tests)

    STR *alt_sym_file=None* -- Name of an alternative file
    containing the persistent version of the symbols dictionary, to
    be used in place of the default file.  Note: the default is to
    use vc_globals.sym_state_file for interactive use, but no file
    for regression testing.

    BOOL *construction_failed* -- indicates whether there has been a
    fatal error during construction of the mediator.

    **CLASS ATTRIBUTES**
    
    *none* --

    ..[AppMgr] file:///./AppMgr.AppMgr.html
    ..[CmdInterp] file:///./CmdInterp.CmdInterp.html
    ..[ServerNewMediator] file:///./ServerNewMediator.tcp_server.html
    """
    
    def __init__(self, interp = None,
                 server = None,
                 console = None,
                 wave_playback = None,
                 correct_evt = None,
                 correct_recent_evt = None,
                 reformat_recent_evt = None,
                 test_or_suite = None,
                 global_grammars = 0, exclusive = 0, 
                 profile_prefix = None,
                 bypass_sr_recog = 0,
                 num_words_training = 0,	                 
                 alt_sym_file = None,
                 symbol_match_dlg_regression = 0,
                 symbol_match_dlg = 0,
                 temporary = 0,
                 **attrs):
        """creates the NewMediatorObject

        **NOTE:** the caller must also call configure before calling
        other methods, or starting the server.
    
        **INPUTS**

        *CmdInterp interp* -- the command interpreter, or None to have
        NewMediatorObject create one

        *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
        dialog/prompt.  Normally disabled except during regression
        testing, and even then, the persistent mediator object should
        be created symbol_match_dlg = 0, because if
        symbol_match_dlg_regression is set, the mediator object will 
        automatically enable the dialog when regression testing starts 
        and disable it afterwards.  

        *BOOL symbol_match_dlg_regression* -- use a CmdInterp with symbol match 
        dialog/prompt during regression testing, with both persistent
        and temporary mediator objects.

        *ServerNewMediator server* -- the TCP server which will listen 
        for new connections from external editors, or None if we are running
        only with internal editors.  The caller must create the server,
        but NewMediatorObject will always own it.  The application which
        creates NewMediatorObject must start the server but only after
        NewMediatorObject has been configured, and should then delete
        its reference to the server.

        *MediatorConsole console* -- GUI console for viewing mediator
        status, allowing the user to close the mediator when it is running
        as a server, and for invoking the correction dialog boxes.  Since
        console is an interface to an underlying GUI, NewMediatorObject
        does not own it, and will not clean up either it or the
        underlying GUI.  May be None if the mediator is not running in 
        GUI mode.

        *CLASS* wave_playback -- class constructor for a concrete
        subclass of WavePlayback, or None if no playback is available

        *CorrectUtteranceEvent correct_evt* -- doorbell used to send an
        event to bring up the correction box asynchronously.

        *STR test_or_suite* -- name of test or test suite to run

        *BOOL global_grammars* -- should this instance use global grammars for 
        regression testing (ignored if test_or_suite is None)

        *BOOL exclusive* -- should this instance use exclusive grammars for 
        regression testing (ignored if test_or_suite is None or
        global_grammars is false)

        *STR profile_prefix* -- prefix for filename for output of profiler,
        or None if not profiling (ignored if test_or_suite is None) 

        *BOOL bypass_sr_recog* -- when testing, bypass natlink for 
        dictation utterances (ignored if test_or_suite is None) 

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file.  Note: the default is to
        use vc_globals.sym_state_file for interactive use, but no file
        for regression testing.

        *BOOL temporary* -- indicates that this NewMediatorObject is a 
        temporary one created by TempConfigNewMediator for a single regression
        test which require their own customized mediator
        """

        debug.trace('NewMediatorObject.__init__', 'invoked')
        self.deep_construct(NewMediatorObject,
                            {'editors': None,
                             'server': server,
                             'external_editors': {},
                             'the_console': console,
                             'wave_playback': wave_playback,
                             'correct_evt': correct_evt,
                             'correct_recent_evt': correct_recent_evt,
                             'reformat_recent_evt': reformat_recent_evt,
                             'interp': interp,
                             'test_or_suite': test_or_suite,
                             'test_space': None,
                             'global_grammars': global_grammars,
                             'alt_sym_file': alt_sym_file,
                             'symbol_match_dlg_regression': \
                                  symbol_match_dlg_regression,
                             'symbol_match_dlg': symbol_match_dlg,
                             'exclusive': exclusive,
                             'profile_prefix': profile_prefix,
                             'bypass_sr_recog': bypass_sr_recog,
                             'num_words_training': num_words_training,
                             'test_suite': None,
                             'testing': 0, 
                             'foreground_testing': 0, 
                             'pickled_interp': None,
                             'config_file': None,
                             'user_config_file': None,
                             'construction_failed': 0,
                             'num_words_training': num_words_training
                            },
                            attrs,
                            {})
        self.add_owned('server')
        self.add_owned('editors')
        self.add_owned('the_console')
        self.add_owned('interp')
        if self.the_console:
            self.the_console.set_mediator(self)
        if self.test_or_suite != None:
            self.test_space = {}
            tests = auto_test.SuiteFactory()
            self.test_space['add_test'] = tests.add_test
            self.test_space['define_suite'] = tests.define_suite
            self.test_space['define_suite_by_range'] = \
                tests.define_suite_by_range
            self.test_space['sort_tests'] = \
                tests.sort
            sys.stderr.write('Loading test definitions...\n')
            sys.stderr.flush()
            tests_def_fname = os.path.join(vc_globals.admin, 'tests_def.py')
            execfile(tests_def_fname, self.test_space)        
            self.test_suite = tests.create_suite(self.test_or_suite)
            if not self.test_suite:
                msg = "Unknown test or suite %s\n" % self.test_or_suite
                sys.stderr.write(msg)
                self.construction_failed = 1
                return

        if self.test_suite or temporary:
            user = 'VCTest'
        else:
# for right now, leave this hard-coded.  Eventually, we want this to be
# configurable, but that would require a separate pre-config file, because we
# need to connect to the speech engine before we can run vc_config and
# user_config.
            user = 'VoiceCode'
        debug.trace('NewMediatorObject.__init__', 
          'connecting to natlink')
        sr_interface.connect(user, mic_state = 'off')        
        debug.trace('NewMediatorObject.__init__', 
          'connected to natlink')
        if self.interp == None:
            debug.trace('NewMediatorObject.__init__', 
              'new interpreter with sym_file %s, temporary = %d' % \
              (repr(alt_sym_file), temporary))
            self.new_interpreter(alt_sym_file = alt_sym_file,
                symbol_match_dlg = symbol_match_dlg, 
                temporary = temporary)
            debug.trace('NewMediatorObject.__init__', 
              'done creating new interpreter')
        else:
            self.interp.set_mediator(self)

        if self.editors == None:
            debug.trace('NewMediatorObject.__init__', 
              'creating new AppMgr')
            self.new_app_mgr()
        if server:
            server.set_mediator(self)

    def new_app_mgr(self):
        """create a new AppMgr if one was not supplied to  the
        constructor
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if a new AppMgr was sucessfully created
        """
        if self.editors != None:
            return 1 # we've already got one!
        correct_words = []
        reformat_words = []
        if self.the_console:
            correct_words = ["Correct"]
            reformat_words = ["Reformat"]
        grammar_factory = \
            sr_grammarsNL.WinGramFactoryNL(correct_words = correct_words, 
                reformat_words = reformat_words, 
                recent_words = ['Recent'], wave_playback = self.wave_playback)
# suppress Correct That if there is no console
# allow Correct Recent, because it has at least a stub implementation
        if self.the_console:
            self.the_console.set_gram_factory(grammar_factory)
        GM_factory = GramMgr.WinGramMgrFactory(grammar_factory, 
            global_grammars = 0, correction = 'basic', text_mode_toggling = 1)
#        res_mgr_factory = \
#            ResMgr.ResMgrStdFactory()
        res_mgr_factory = \
            ResMgr.ResMgrBasicFactory(correct_evt = self.correct_evt,
            correct_recent_evt = self.correct_recent_evt, 
            reformat_recent_evt = self.reformat_recent_evt)
        recog_mgr = RecogStartMgrNL.RecogStartMgrNL(GM_factory = GM_factory,
            res_mgr_factory = res_mgr_factory)
        self.editors = AppMgr.AppMgr(recog_mgr, mediator = self)
        return 1
    
    def new_interpreter(self, alt_sym_file = None,
        symbol_match_dlg = 0, no_circle = 0, temporary = 0):
        """create a new interpreter

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.interp:
# don't automatically clean up the dictionary - only do that when we
# are explicitly requested to do so
#            self.interp.cleanup_dictionary()
# do clean up manually because we are deleting it before we ourselves are
# cleaned up
            self.interp.cleanup()
        m = self
        if no_circle:
            m = None
        sym_file = alt_sym_file
# if no alternative file is specified, use the default SymDict state
# file, or None if we are doing regression testing
        if alt_sym_file is None and not (self.test_suite or temporary):
            sym_file = vc_globals.sym_state_file
        self.interp = \
            CmdInterp.CmdInterp(sym_file = sym_file, 
                disable_dlg_select_symbol_matches = not symbol_match_dlg, 
                mediator = m)

    def console(self):
        """returns a reference to the MediatorConsole which provides the
        GUI correction interfaces.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.the_console
   
    def interpreter(self):
        """return a reference to the mediator's current CmdInterp object

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.interp
   
    def configure(self, config_file = None, user_config_file = None, 
        exclude_interp = 0, testing = 0):
        """Configures a mediator object based on a configuration file.
        Must be called before (and only before) run.
        
        **INPUTS**
        
        *STR* config_file -- Full path of the config file.  Defaults to
        vc_globals.default_config_file 

        *STR* user_config_file -- Full path of the user config file.  
        Defaults to vc_globals.default_user_config_file 

        *BOOL* exclude_interp -- if true, don't re-configure the
        interpreter

        *BOOL* testing -- true if this is part of a regression test,
        otherwise false

        **OUTPUTS**
        
        *BOOL* -- true if mediator configuration succeeded.
        """        

#        print 'Mediator configure:\n'
#        print traceback.extract_stack()
        if self.construction_failed:
            return 0
        exclude = None
        if exclude_interp:
            exclude = ['interp']
        testing = testing or self.test_suite
        okay = self._configure_from_file(config_file = config_file,
            user_config_file = user_config_file,
            exclude = exclude, testing = testing)
        # Generate and cache the grammar specifcations
#        self.interp.gram_spec_spoken_cmd('known_spoken_cmd')
#        self.interp.gram_spec_spoken_symbol('known_spoken_symbol')
        
        # Comment this out if you want to generate training material
        # Actually, need to invoke these AFTER CSC, etc... have been
        # added!        
        self.interp.generate_training_material(self.num_words_training)            
            
        if not okay:
            return okay
        if testing:
# remove the reference to ourselves before pickling
            self.interp.set_mediator(None)
# pickled interpreter is used only for regression testing, so 
# use symbol_match_dlg setting appropriate for regression testing
            old_setting = self.interp.disable_dlg_select_symbol_matches
            self.interp.disable_dlg_select_symbol_matches = not self.symbol_match_dlg_regression
            self.pickled_interp = cPickle.dumps(self.interp)
            self.interp.set_mediator(self)
            self.interp.disable_dlg_select_symbol_matches = old_setting
        return 1


    def define_config_functions(self, names, exclude = None,
            reset = 0, alt_sym_file = None, symbol_match_dlg = None, 
            add_sr_entries_for_LSAs_and_CSCs = 1):
        """Adds the appropriate configuration functions to the  given
        namespace, to allow the configuration file to access the
        appropriate mediator methods.  These functions are generally
        bound methods.
        
        **INPUTS**

        *{STR: ANY}* names -- the dictionary or namespace to which to
        add the functions

        *[STR] exclude* -- list of mediator object attributes objects 
        to ignore during reconfiguration.  Currently, the only recognized 
        attributes are ['editors', 'interp'].
        
        *BOOL reset* -- if true, reset the current interpreter, or
        replace it with a fresh one

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file.  Note: the default is to
        use vc_globals.sym_state_file for interactive use, but no file
        for regression testing.

        *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
        dialog/prompt.  Normally disabled except during regression
        testing.  If None, use current setting.

        **OUTPUTS**
        
        *none* 
        """        
        if exclude == None:
            exclude = []
        self.before_app_mgr_config(names, ignore = 'editors' in exclude)
        sym_dlg = self.symbol_match_dlg
        if symbol_match_dlg != None:
           sym_dlg = symbol_match_dlg
        self.before_interp_config(names, ignore = 'interp' in exclude,
            reset = reset and ('interp' not in exclude),
            alt_sym_file = alt_sym_file,
            symbol_match_dlg = sym_dlg, 
            add_sr_entries_for_LSAs_and_CSCs = add_sr_entries_for_LSAs_and_CSCs)


    def _configure_from_file(self, exclude = None, config_file = None,
        user_config_file = None,
        reset = 0,
        alt_sym_file = None, symbol_match_dlg = None,
        add_sr_entries_for_LSAs_and_CSCs = 1,
        use_pickled_interp = 0, testing = 0):
        """private method used by configure and reconfigure to perform
         actual configuration.

        **INPUTS**
        
        *[STR] exclude* -- list of mediator object attributes objects 
        to ignore during reconfiguration.  Currently, the only recognized 
        attributes are ['editors', 'interp'].
        
        *STR* config_file* -- Full path of the config file.  Defaults to
        the vc_globals.default_config_file 

        *STR* user_config_file -- Full path of the user config file.  
        Defaults to vc_globals.default_user_config_file 

        *BOOL reset* -- if true, reset the current interpreter, or
        replace it with a fresh one

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file.  Note: the default is to
        use vc_globals.sym_state_file for interactive use, but no file
        for regression testing.

        *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
        dialog/prompt.  Normally disabled except during regression
        testing.  If None, use current setting.

        *BOOL use_pickled_interp* -- if true, reset the interpreter by
        unpickling it from the copy pickled on initialization (if one is
        available)

        *BOOL* testing -- true if this is part of a regression test,
        otherwise false

        **OUTPUTS**
        
        *BOOL* -- true if mediator configuration succeeded.
        """   
        debug.trace('NewMediatorObject._configure_from_file', 'config_file = "%s", user_config_file = "%s"' 
              % (config_file, user_config_file))     
        if exclude == None:
            exclude = []
        config_dict = {}
        sym_dlg = self.symbol_match_dlg
        if symbol_match_dlg != None:
           sym_dlg = symbol_match_dlg
        if not 'interp' in exclude:
            if use_pickled_interp and self.pickled_interp:
# clean up manually because we are deleting it before we ourselves are
# cleaned up
                self.interp.cleanup()
                self.interp = cPickle.loads(self.pickled_interp)
                self.interp.set_mediator(self)
                exclude.append('interp')
# if we've unpickled the interpreter, we don't need to re-configure it
        self.define_config_functions(config_dict, exclude,
            reset = reset,
            alt_sym_file = alt_sym_file,
            symbol_match_dlg = sym_dlg,
            add_sr_entries_for_LSAs_and_CSCs = add_sr_entries_for_LSAs_and_CSCs)
        file = config_file
        if not file:
            file = vc_globals.default_config_file
        execfile(file, config_dict)
# doing execfile directly actually provides better error reporting (the
# traceback is actually reported from the proper line, even when it is
# in another module imported from the config file).  (Probably the same
# effect could be achieved if we put a try block in the config file
# itself)
#        try:
#            execfile(file, config_dict)
#        except Exception, err:
#            print 'ERROR: in configuration file %s.\n' % file
#            raise err
        user_file = user_config_file
        debug.trace('NewMediatorObject._configure_from_file', 'initially, user_file="%s", test_next=%s, vc_globals.regression_user_config_file="%s", vc_globals.default_user_config_file="%s"' % 
                    (user_file, not (self.test_suite is None), vc_globals.regression_user_config_file, vc_globals.default_user_config_file))         
        if not user_file:
            if testing or self.test_suite:
                user_file = vc_globals.regression_user_config_file
            else:
                user_file = vc_globals.default_user_config_file
                
        debug.trace('NewMediatorObject._configure_from_file', 'user_file="%s"' % user_file) 

        okay = os.path.exists(user_file)
        if not okay:
            console = self.console()
            if console:
                okay = console.copy_user_config(user_file, vc_globals.sample_config)
            if not okay:
                sys.stderr.write("\n\nVCode ERROR: non-existent user ")
                sys.stderr.write("configuration file \n'%s'\n" % user_file)
                sys.stderr.write("Create this file by copying one of the ")
                sys.stderr.write("sample configuration files from \n")
                sys.stderr.write("%s, and re-run the mediator.\n" \
                    % vc_globals.sample_config)
                sys.stderr.write("\n\n***Note Optionally you can also restrict the languages you can run\n")
                sys.stderr.write("by defining this in 'user_globals.py' in the config directory.\n")
                sys.stderr.write("A sample is given in the 'samples' directory.\n\n")
                sys.stderr.write("If you want to do this, please copy this file\n")
                sys.stderr.write("into the config directory and edit as appropriate.\n")
                return 0
                
        execfile(user_file, config_dict)

# if successful, store file name so the reconfigure method can reuse it
        self.config_file = config_file
        self.user_config_file = user_config_file

        #
        # Compile standard symbols for the different languages
        #
        if not 'interp' in exclude:
            self.interp.finish_config()
        return 1

    def before_app_mgr_config(self, config_dict, ignore = 0):
        """called by configure to add the functions pertaining to
        AppMgr configuration to the configuration dictionary.  If
        ignore is true, this method will instead add dummy versions of those
        functions which will do nothing.

        **INPUTS**

        *{STR: ANY} config_dict* -- dictionary to which to add
        interpreter configuration functions.  This dictionary will be
        used as the namespace for executing the configuration file.

        *BOOL ignore* -- if true, add dummy versions of the application
        manager configuration functions, so that calls to these functions from
        the configuration file will be ignored.  
        """
        if ignore:
            config_dict['add_module'] = do_nothing
            config_dict['trust_current_window'] = do_nothing
            config_dict['add_prefix'] = do_nothing
            config_dict['capitalize_rules'] = do_nothing
        else:
            config_dict['add_module'] = self.add_module
            config_dict['trust_current_window'] = self.trust_current_window
            config_dict['add_prefix'] = self.add_app_prefix
            config_dict['capitalize_rules'] = self.capitalize_rules

    def before_interp_config(self, config_dict, reset = 0, ignore = 0,
        alt_sym_file = None, symbol_match_dlg = None, 
        add_sr_entries_for_LSAs_and_CSCs = 1):
        """called by configure to reset or replace the current interpreter 
        (unless reset is false), and add the functions pertaining to
        interpreter configuration to the configuration dictionary.  If
        ignore is true, this method will add dummy versions of those
        functions which will do nothing.

        **INPUTS**

        *{STR: ANY} config_dict* -- dictionary to which to add
        interpreter configuration functions.  This dictionary will be
        used as the namespace for executing the configuration file.

        *BOOL reset* -- if true, reset the current interpreter, or
        replace it with a fresh one

        *BOOL ignore* -- if true, add dummy versions of the interpreter
        configuration functions, so that calls to these functions from
        the configuration file will be ignored.  Normally, reset should
        be false if ignore is true

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file.  Note: the default is to
        use vc_globals.sym_state_file for interactive use, but no file
        for regression testing.

        *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
        dialog/prompt.  Normally disabled except during regression
        testing.  If None, use current setting.
        """
        if reset:
            sym_dlg = self.symbol_match_dlg
            if symbol_match_dlg != None:
               sym_dlg = symbol_match_dlg
            self.new_interpreter(alt_sym_file = alt_sym_file,
                symbol_match_dlg = sym_dlg)
            self.interp.add_sr_entries_for_LSAs_and_CSCs = \
                add_sr_entries_for_LSAs_and_CSCs
        if ignore:
            config_dict['interpreter'] = None
            config_dict['add_csc'] = do_nothing
            config_dict['add_csc_set'] = do_nothing
            config_dict['add_lsa'] = do_nothing
            config_dict['add_lsa_set'] = do_nothing
            config_dict['add_cmd_set'] = do_nothing
            config_dict['add_capitalization_word'] = do_nothing
            config_dict['add_capitalization_word_set'] = do_nothing
            config_dict['clear_standard_symbols_file_list'] = do_nothing            
            config_dict['standard_symbols_in'] = do_nothing
            config_dict['abbreviations_in'] = do_nothing
            config_dict['add_identifier'] = do_nothing
            config_dict['set_builder_preferences'] = do_nothing
            config_dict['print_abbreviations'] = do_nothing
            config_dict['text_mode_toggling'] = do_nothing
        else:
            config_dict['interpreter'] = self
            config_dict['add_csc'] = self.add_csc
            config_dict['add_csc_set'] = self.add_csc_set
            config_dict['add_lsa'] = self.add_lsa
            config_dict['add_lsa_set'] = self.add_lsa_set
            config_dict['add_cmd_set'] = self.add_cmd_set
            config_dict['add_capitalization_word'] = \
                self.add_capitalization_word
            config_dict['add_capitalization_word_set'] = \
                self.add_capitalization_word_set
            config_dict['has_lsa'] = self.has_lsa
            config_dict['clear_standard_symbols_file_list'] = self.clear_standard_symbols_file_list          
            config_dict['standard_symbols_in'] = self.standard_symbols_in
            config_dict['abbreviations_in'] = self.abbreviations_in
            config_dict['add_identifier'] = self.add_identifier
            config_dict['set_builder_preferences'] = self.set_builder_preferences
            config_dict['print_abbreviations'] = self.print_abbreviations
            config_dict['text_mode_toggling'] = self.text_mode_toggling            

    def reset(self, config_file = None, user_config_file = None, 
        alt_sym_file = None,
        symbol_match_dlg = None, add_sr_entries_for_LSAs_and_CSCs=1,
        use_pickled_interp = 1, exclusive = 0):
        """reset the mediator object to continue regression testing with
        a fresh interpreter

        **INPUTS**

        *STR* config_file* -- Full path of the config file.  If None,
        then use the same one used previously by configure, or
        the vc_globals.default_config_file if configure
        did not record the filename.

        *STR* user_config_file* -- Full path of the user config file.  If None,
        then use the same one used previously by configure, or
        the vc_globals.default_user_config_file if configure
        did not record the filename.

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file, or None to use the same
        file as specified at initial creation of NewMediatorObject

        *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
        dialog/prompt.  Normally disabled except during regression
        testing.  If None, use current setting.
        
        *BOOL add_sr_entries_for_LSAs_and_CSCs=1* -- see [CmdInterp] attribute 
        by the same name.

        *BOOL use_pickled_interp* -- if true, reset the interpreter by
        unpickling it from the copy pickled on initialization
        
        ..[CmdInterp] file:///./CmdInterp.CmdInterp.html"""
        
        debug.trace('NewMediatorObject.reset', 'invoked')
        old_add_entries = self.interp.add_sr_entries_for_LSAs_and_CSCs
        sym_dlg = self.symbol_match_dlg
        if symbol_match_dlg != None:
           sym_dlg = symbol_match_dlg
        self.reconfigure(exclude = ['editors'],
            config_file = config_file, 
            user_config_file = user_config_file, reset = 1, 
            alt_sym_file = alt_sym_file,
            symbol_match_dlg = sym_dlg, 
            add_sr_entries_for_LSAs_and_CSCs = add_sr_entries_for_LSAs_and_CSCs,
            use_pickled_interp = use_pickled_interp)
        self.reset_results_mgr()
        self.editors.set_exclusive(exclusive)
        self.interp.add_sr_entries_for_LSAs_and_CSCs = old_add_entries

    def reconfigure(self, exclude = None, config_file=None,
        user_config_file = None,
        reset = 0,
        alt_sym_file = None, symbol_match_dlg = None,
        add_sr_entries_for_LSAs_and_CSCs = 1,
        use_pickled_interp = 1):
        """reconfigure an existing mediator object.  Unlike configure,
        reconfigure may be called while the mediator object is already
        running.  By default, reconfigure will use the same files used by
        configure, or the vc_globals.default_config_file  and
        vc_globals.default_user_config_file if configure
        did not record the filenames.
        
        **INPUTS**
        
        *[STR] exclude* -- list of mediator object attributes objects 
        to ignore during reconfiguration.  Currently, the only recognized 
        attributes are ['editors', 'interp'].
        
        *STR* config_file* -- Full path of the config file.  If None,
        then use the same one used previously by configure, or
        the vc_globals.default_config_file if configure
        did not record the filename.

        *STR* user_config_file* -- Full path of the user config file.  If None,
        then use the same one used previously by configure, or
        the vc_globals.default_user_config_file if configure
        did not record the filename.

        *BOOL reset* -- if true, reset the current interpreter, or
        replace it with a fresh one

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file, or None to use the same
        file as specified at initial creation of NewMediatorObject

        *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
        dialog/prompt.  Normally disabled except during regression
        testing.  If None, use current setting.

        *BOOL use_pickled_interp* -- if true, reset the interpreter by
        unpickling it from the copy pickled on initialization

        **OUTPUTS**
        
        *none* -- 
        """        
        debug.trace('NewMediatorObject.reconfigure', 'invoked')        
        file = config_file
        if not file:
            file = self.config_file
        user_file = user_config_file
        if not user_file:
            user_file = self.user_config_file
        if alt_sym_file is None:
            alt_sym_file = self.alt_sym_file
        sym_dlg = self.symbol_match_dlg
        if symbol_match_dlg != None:
           sym_dlg = symbol_match_dlg
        self._configure_from_file(exclude = exclude, config_file = file,
            user_config_file = user_config_file,
            reset = reset,
            alt_sym_file = alt_sym_file,
            symbol_match_dlg = sym_dlg, 
            add_sr_entries_for_LSAs_and_CSCs = add_sr_entries_for_LSAs_and_CSCs,
            use_pickled_interp = use_pickled_interp)
                    

    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **NOTE:** subclasses must call their parent class's 
        remove_other_references method, after performing their own duties.
        Also, a class inheriting from two OwnerObject classes MUST
        define remove_other_references and call both subclasses'
        versions

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# subclasses must call their parent class's remove_other_references
# method, after performing their own duties
#        print 'removing'


# for now, don't disconnect from sr_interface -- let the creator do that

        self.correct_evt = None
# correct_evt may have a reference to a method of the application which
# owns NewMediatorObject
        Object.OwnerObject.remove_other_references(self)

    def quit(self, save_speech_files=None, disconnect=1, 
        console_closed = 0):
        """Quit the mediator object

        **INPUTS**
        
        *BOOL* save_speech_files = None -- Indicates whether or not
        speech files should be saved. If *None*, then ask the user.

        *BOOL* disconnect = 1 -- Indicates whether or not to disconnect from
        the SR system.

        *BOOL* console_closed = 0 -- set to true to indicate if the call 
        to quit is occurring because the main GUI console window was closed.
        That way, the quit method will know to stop trying to send user
        messages to the console

        **OUTPUTS**
        
        *none* --
        """            
        debug.trace('NewMediatorObject.quit', 'quit called')

        if console_closed and self.the_console:
# if the call to quit is occurring because the main GUI console window 
# was closed, clean this up early, to avoid sending user messages to
# a non-existant window
            self.the_console.cleanup()
            self.the_console = None

        if self.interp:
            self.interp.save_dictionary()

        if self.server:
            self.server.mediator_closing()

        if self.editors:
            self.editors.cleanup()
            self.editors = None

        disconnect_from_sr(disconnect, save_speech_files)

    def delete_editor_cbk(self, app_name, instance_name, unexpected = 0):
        """callback from the application manager indicating that
        an editor closed or disconnected from the mediator

        **INPUTS**

        *STR app_name* -- name of the editor 

        *STR instance_name* -- name of the application instance

        *BOOL unexpected* -- for external editors, was the editor
        connection broken unexpectedly, or did the editor notify the
        mediator that it was going to close/disconnect?

        **OUTPUTS**

        *none*
        """
# for now, all editor instance-specific data is stored under AppMgr,
# except for external editors, for which we have the instance name in 
# external_editors and the server has a reference to the
# AppStateMessaging interface.
#
# once we add correction, we may have other instance-specific objects 
# which we will need to clean up.
        try:
            del self.external_editors[instance_name]
        except KeyError:
            pass
        else: 
            s = '%s instance %s disconnected' % (app_name, instance_name)
            if unexpected:
                s = s + ' unexpectedly'
# don't use instance argument because instance we've disconnected
            self.user_message(s)
            self.server.delete_instance_cbk(instance_name, 
                unexpected = unexpected)

# Note: if we have no server, we should quit when the last internal
# editor exits.  However, the application which creates
# NewMediatorObject will be running the internal editor, so it should
# know when it exits and should call our cleanup method

    def cancel_testing(self):
        """cancel a regression test if one is running, and return to the
        main message loop.  If no regression test is running, this
        method will have no effect.

        Note: This is an experimental method which may not work.  Even
        if it does, its effect is asynchronous, and there may be a
        substantial delay before the test ceases.  It is primarily intended to
        allow the user to exit the mediator by closing the GUI wxMediator 
        console in the middle of a test which would otherwise not return
        to the main message loop until it finished.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        if self.testing:
            try:
                self.test_space['commands'].should_exit = 1
            except:
                sys.stderr.write("No SimCmdsObj in test_space" + \
                    "- unable to cancel test\n")

    def _new_test_editor(self, app, server = 1, check_window = 1, 
            window_info = None):
        """private method to a new editor application instance and run
        regression tests.  This method should only be called by
        NewMediatorObject.new_editor.

        **INPUTS**

        *AppState* app --  AppState interface corresponding to the new
        instance

        *BOOL* server -- true if this editor instance is connected to the 
        mediator via the server.

        *BOOL* check_window -- should we check to see if the
        current window belongs to this instance?  Normally ignored
        unless self.global_grammars is false, or the application manager
        is unable to create a universal instance

        *(INT, STR, STR) window_info*  -- window id, title, and module of 
        the current window as detected by the TCP server when it
        originally processed the new editor connection, or None to let
        RSM.new_instance check now.  Normally ignored unless
        check_window is true and either self.global_grammars is false
        or the application manager is unable to create a 
        universal instance.

        **OUTPUTS**

        *BOOL* -- true if the regression tests were run
        """
        instance_name = self.editors.new_instance(app, 
            check_window = check_window, window_info = window_info)
        if not instance_name:
            return 0
        app = self.editors.app_instance(instance_name)
# give the user an initial chance to save existing buffers before
# closing them
        app.init_for_test(save = 0)
        app.print_buff_when_changed = 1
        self.test_space['testing'] = \
            regression.PersistentConfigNewMediator(mediator = self,
            editor_name = instance_name, names = self.test_space,
            symbol_match_dlg = self.symbol_match_dlg_regression,
            bypass_sr_recog = self.bypass_sr_recog,            
            correction = 'basic', text_mode_toggling = 1)
        self.interp.enable_symbol_match_dlg(self.symbol_match_dlg_regression)
        self.test_space['temp_factory'] = \
             regression.TempConfigNewMediatorFactory(symbol_match_dlg = \
             self.symbol_match_dlg_regression, 
             pickled_interp = self.pickled_interp)
        self.testing = 1
        if self.console():
            self.console().starting_tests()
        start_time = time.time()
        end_time = start_time
        try:
            try:
                if self.test_suite.foreground_count():
                    if not server:
                        msg = 'WARNING: Unable to run foreground tests with\n' \
                            + 'an internal editor\n'
                        sys.stderr.write(msg)
                    else:
                        self.foreground_testing = 1
                        self.user_message('Starting foreground tests...')
                        time.sleep(3)
                        self.test_suite.run_foreground(profile_prefix =
                            self.profile_prefix)
                        self.user_message('Finished foreground tests...')
                        self.foreground_testing = 0
                        util.bell()
                        time.sleep(3)
                if self.test_suite.background_count():
                    if self.global_grammars:
                        ok = self.editors.make_universal(instance_name, 
                            exclusive = self.exclusive)
                    self.user_message('Starting background tests...')
                    time.sleep(3)
                    self.test_suite.run_background(profile_prefix =
                        self.profile_prefix)
                self.testing = 0
                if self.test_suite.foreground_count() + self.test_suite.background_count() > 1:
                    util.bell(5)
                else:
                    util.bell(1)
                app.mediator_closing()
            except messaging.SocketError:
                sys.stdout.write('**** Test editor disconnected unexpectedly')
                pass
        except:
            traceback.print_exc()
        end_time = time.time()
        self.testing = 0
        self.test_suite = None
        elapsed_time = end_time - start_time     
        print '\n\n\n-----------------------------------------------'
        print 'Test suite completed in:  %s secs' % elapsed_time
        print '-----------------------------------------------'            
        if self.console():
            self.console().finished_tests()
        del self.test_space['testing']
        del self.test_space['temp_factory']
        self.interp.enable_symbol_match_dlg(self.symbol_match_dlg)
        self.editors.delete_instance(instance_name)
        return 1

    def new_editor(self, app, server = 1, check_window = 1, 
            window_info = None, test_editor = 0):
        """add a new editor application instance

        **INPUTS**

        *AppState* app --  AppState interface corresponding to the new
        instance

        *BOOL* server -- true if this editor instance is connected to the 
        mediator via the server.

        *BOOL* check_window -- should we check to see if the
        current window belongs to this instance?

        *(INT, STR, STR) window_info*  -- window id, title, and module of 
        the current window as detected by the TCP server when it
        originally processed the new editor connection, or None to let
        RSM.new_instance check now.  Ignored unless check_window is
        true.

        BOOL *test_editor* -- flag indicating whether or not the editor
        is expecting to be used for regression testing


        **OUTPUTS**

        *STR* -- name of the application instance.  Necessary
        if you want to add windows to the application in the future.
        """
        if test_editor:
            if self.test_suite:
                self._new_test_editor(app, server = server, 
                    check_window = check_window, window_info = window_info)
                return None
        instance_name = self.editors.new_instance(app, 
            check_window = check_window, window_info = window_info)
# don't use instance argument to user_message here, because the editor
# should display its own message on connection
        self.user_message('%s instance %s connected' %
            (self.editors.app_name(instance_name), instance_name))
        if server:
            if self.server == None:
                msg = 'WARNING: new_editor called with server = 1, but mediator has no server\n'
                sys.stderr.write(msg)
            else:
                self.external_editors[instance_name] = 1
        return instance_name

    def editor_instance(self, instance_name):
        """return a reference to the AppState object corresponding to a
        particular instance. **Note:** Use only temporarily.  Storing 
        this reference is unsafe, and may lead to mediator crashes on 
        calls to its methods, and to failure to free resources.

        **INPUTS**

        *STR* instance -- name of the application instance 

        **OUTPUTS**

        *AppState* -- temporary reference to the corresponding AppState
        object
        """
        return self.editors.app_instance(instance_name)

    def add_symbol(self, symbol, user_supplied_spoken_forms=[], \
                   tentative = 1, add_sr_entries=1):
        """Add a symbol to the dictionary

        **INPUTS**
        
        *STR* symbol -- Symbol to add

        *[STR] user_supplied_spoken_forms* -- Spoken forms for the
         symbol which were supplied explicitly by the user. These
         forms are added even if they are not generated automaticly by
         [update_spoken_forms]. This is useful in cases where the user
         has explicitly supplied spoken forms for a symbol that contains very
         short abbreviations (i.e. abbreviations that are rejected by
         [add_abbreviation]). In such cases, the spoken form wouldn't
         automaticaly be generated by [updated_spoken_forms] and must
         therefore be added explictely by add_symbol.

        *BOOL* tentative = 1 -- If true, symbol is added tentatively,
        and can be removed on undo/correction/reformatting

        *BOOL* add_sr_entries = 1 -- If true, adds symbol to the SR vocabulary.
        
        **OUTPUTS**
        
        *none* -- 
        """
        self.interp.add_symbol(symbol,
            user_supplied_spoken_forms, tentative = tentative,
            add_sr_entries = add_sr_entries)

    def add_csc(self, acmd):
        """Add a new Context Sensitive Command.

        [CSCmd] *acmd* is the command to add.      

        .. [CSCmd] file:///./CSCmd.CSCmd.html"""

        self.interp.add_csc(acmd)

    def add_csc_set(self, set):
        """add CSCs from a set

        **INPUTS**

        *CSCmdSet set* -- the set of commands to add

        **OUTPUTS**

        *none*
        """
        self.interp.add_csc_set(set)

    def add_cmd_set(self, set):
        """add CSCs from a set

        **INPUTS**

        *CSCmdSet set* -- the set of commands to add

        **OUTPUTS**

        *none*
        """
        self.interp.add_cmd_set(set)

    def has_lsa(self, spoken_form, language = None):
        """check if there is already an LSA defined with this spoken
        form

        **INPUTS**

        *STR spoken_form* -- spoken form to check

        *STR language* -- name of the language in which to check

        **OUTPUTS**

        *BOOL* -- true if such an LSA exists
        """
        return self.interp.has_lsa(spoken_form, language)

    def add_lsa(self, an_LSA):
        """Add a language specific word.

        **INPUTS**
        
        *LSAlias an_LSA* -- language-specific alias (see CmdInterp)
        
        **OUTPUTS**
        
        *none* -- 
        """
        
        self.interp.add_lsa(an_LSA)

    def add_capitalization_word(self, an_LSA):
        """Add a capitalization word.

        **INPUTS**
        
        *CapitalizationWord word* -- see CmdInterp
        
        **OUTPUTS**
        
        *none* -- 
        """
        
        self.interp.add_capitalization_word(word)

    def add_lsa_set(self, set):
        """add LSAs from a set

        **INPUTS**

        *LSAliasSet set* -- the set of aliases to add

        **OUTPUTS**

        *none*
        """
        self.interp.add_lsa_set(set)

    def add_capitalization_word_set(self, set):
        """add CapitalizationWords from a set

        **INPUTS**

        *CapitalizationWordSet set* -- the set of words to add

        **OUTPUTS**

        *none*
        """
        self.interp.add_capitalization_word_set(set)

    def clear_standard_symbols_file_list(self):
        """Clears the list of files defining standard symbols"""
        self.interp.clear_standard_symbols_file_list()

    def standard_symbols_in(self, file_list):
        """Specify source files defining standard symbols"""
        self.interp.standard_symbols_in(file_list)

    def abbreviations_in(self, file_list):
        """Specify source files defining expansions and abbreviations"""
        self.interp.abbreviations_in(file_list)

    def print_symbols(self, symbols = None):
        self.interp.print_symbols(symbols)

    def print_abbreviations(self, show_unresolved=0):
        self.interp.print_abbreviations(show_unresolved)

    def add_identifier(self, identifier, parent = None):
        """defines a new identifier type for the SymBuilderFactory

        *STR identifier* -- name of the new identifier type (must NOT be
        a known identifier type, or a RuntimeError will be raised)

        *STR parent* -- name of the parent (must be a known identifier
        type, or None, or a RuntimeError will be raised)

        **OUTPUTS**
        """
        self.interp.add_identifier(identifier, parent = parent)
 
    def set_builder_preferences(self, builders, identifier = None, 
        language = None):
        """establishes the preferred order for symbol formatting styles
        for a given language and identifier type

        **INPUTS**

        *[STR] builders* -- prioritized list of names of registered SymBuilder
        objects. If one of the builders is unknown, set_preferences raises 
        a RuntimeError.

        *STR identifier* -- name of the identifier to which these
        preference apply, or None to set general preferences for all
        identifiers without their own preferences.  If the identifier type 
        is unknown, set_preferences raises a RuntimeError.

        *STR language* -- name of the language to which these
        preference apply, or None to set general preferences for all
        languages
        """
        self.interp.set_builder_preferences(builders, identifier =
            identifier, language = language)

    def add_module(self, module):
        """add a new KnownTargetModule to the AppMgr/RecogStartMgr

        **INPUTS**

        *KnownTargetModule* module -- the new module

        **OUTPUTS**

        *BOOL* -- true unless a module of the same name already exists
        """
        return self.editors.add_module(module)

    def window_info(self):
        """find the window id, title, and module of the current window

        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name
        """
        return self.editors.window_info()

    def trust_current_window(self, trust = 1):
        """specifies whether the RecogStartMgr should trust that the current
        window corresponds to the editor when the editor first connects to
        VoiceCode, or when it notifies VoiceCode of a new window.

        **INPUTS**

        *BOOL* trust_current_window -- 1 if RSM should trust that the current
        window corresponds to the editor when the editor first connects to
        VoiceCode, or when it notifies VoiceCode of a new window.

        **OUTPUTS**

        *none*
        """
        self.editors.trust_current(trust)

    def add_app_prefix(self, app_name, title_prefix):
        """specifies a title prefix to use for a given editor application.

        **INPUTS**

        *STR* app_name -- name of the editor application

        *STR* title_prefix  -- a unique string for each application, 
        used as the prefix of the title string (which is in turn 
        included as a substring of the window title, if the editor 
        can do so).  The prefix should be entirely alphabetic and
        contain no spaces or punctuation.

        **OUTPUTS**

        *BOOL* -- false if app_name was already known, or prefix wasn't
        unique
        """
        return self.editors.add_prefix(app_name, title_prefix)

    def capitalize_rules(self, capitalize):
        """specifies whether words in rules for context-free grammars 
        should be capitalized.
        
        Note: This is important for ensuring that the correction grammar
        overrides the corresponding built-in grammars.  capitalize_rules
        should be true for NaturallySpeaking 5 or earlier, but false for
        NaturallySpeaking 6 or later (have to check about v. 5)

        **INPUTS**

        *BOOL* capitalize -- if true, then words in rules like "scratch
        that" should be capitalized.

        **OUTPUTS**

        *none*
        """
        self.editors.capitalize_rules(capitalize)

    def reset_results_mgr(self, instance_name = None):
        """resets the ResMgr objects for a given editor, erasing any 
        stored utterance and corresponding editor state information.  
        Normally called only as part of resetting the mediator for 
        a new regression test

        **INPUTS**

        *STR instance_name* -- the editor whose data should be reset, or
        None to reset ResMgr data for all editors

        **OUTPUTS**

        *none*
        """
        self.editors.reset_results_mgr(instance_name = instance_name)

    def rel_utter_index_to_utter_id(self, rel_index, instance_name):
        """Converts a relative utterance index (i.e. index from
        the end of the list of stored utterances) to an utterance
        id.
        **INPUTS**
        
        INT *rel_index*  -- index to be converted.
        
        STR *instance_name* -- the editor 
        
        
        **OUTPUTS**
        
        INT *utter_id* -- the utterance id
        """

        recent = self.recent_dictation(instance_name)       
        
        if rel_index > len(recent):
            raise BadRelativeUtteranceIndex(rel_index, "too large")
        elif rel_index < 1:
            raise BadRelativeUtteranceIndex(rel_index, "too small")
            
        utter_id = recent[-rel_index][1]
        return utter_id

    def utter_id_to_rel_utter_index(self, utter_id, instance_name):
        """Converts an absolute utterance ID to a relative utterance index 
        (i.e. index from the end of the list of stored utterances) to an utterance
        id.
        **INPUTS**
        
        INT *utter_id*  -- the utterance id to be converted
        
        STR *instance_name* -- the editor 
        
        
        **OUTPUTS**
        
        INT *rel_index* -- the index of that utterance, from the end of the
        recent utterances list.
        """
        recent = self.recent_dictation(instance_name)
        debug.trace('NewMediatorObject.utter_id_to_rel_utter_index', '** invoked, len(recent)=%s, range(1, len(recent)+1)= %s' % (len(recent), range(1, len(recent)+1)))
        for rel_index in range(1, len(recent)+1):
           debug.trace('NewMediatorObject.utter_id_to_rel_utter_index', '** rel_index=%s' % rel_index)
           if recent[-rel_index][1] == utter_id:
              return rel_index
        
        raise BadUtteranceId(utter_id, "not found in recent utterances list")

    def stored_utterances(self, instance_name):
        """queries the ResMgr to see how many dictated utterances have 
        been stored for the specified editor

        **INPUTS**

        *STR instance_name* -- the editor 

        **OUTPUTS**

        *INT* -- number of utterances which can be retrieved with
        recent_dictation
        """
        return self.editors.stored_utterances(instance_name)

    def recent_dictation(self, instance_name, n = None):
        """returns a list of the most recent SpokenUtterance objects for
        the specified editor

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

        *STR instance_name* -- the editor 

        *INT n* -- the number of utterances to return, or None to return 
        all available utterances.

        **OUTPUTS**

        *[(SpokenUtterance, INT, BOOL)]* -- the n most recent dictation 
        utterances (or all available if < n), sorted most recent last, 
        each with a corresponding identifying number and a flag indicating 
        if the utterance can be undone and re-interpreted, 
        or None if no utterances are stored.

        The utterance number is unique, within a given editor instance.

        Note:  These utterances should not be stored permanently, nor
        should they be modified except as part of the correction
        process.  Also, the status of whether a given utterance can be
        re-interpreted may change if the user makes other changes to the 
        """
        return self.editors.recent_dictation(instance_name, n = n)

    def recent_symbols(self, instance_name, n=None):
        """returns a list of the most recently uttered symbols.

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

        *STR instance_name* -- the editor 

        *INT n* -- the number of utterances from which to pull recently dictated symbols.
        If None, then return all of them.

        **OUTPUTS**

        *[SymbolResults]* -- the symbols spoken in the n most recent 
        utterances (or all available if < n), sorted most recent last.
        
        Note:  These symbols should not be stored permanently, nor
        should they be modified except as part of the correction
        process.  Also, the status of whether a given utterance can be
        re-interpreted may change if the user makes other changes to the 
        """
        return self.editors.recent_symbols(instance_name, n = n)

    def scratch_recent(self, instance_name, n = 1):
        """undo the effect of the most recent n utterances into the
        specified editor, if possible.

        **INPUTS**

        *STR instance_name* -- the editor 

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances actually undone
        """
        debug.trace('NewMediatorObject.scratch_recent', 'instance_name=%s, n=%s' % (instance_name, n))
        return self.editors.scratch_recent(instance_name, n = n)

    def reinterpret_recent(self, instance_name, changed, delete_tentative_syms = 1):
        """undo the effect of one or more recent utterances, if
        possible, and reinterpret these utterances (and possibly any
        intervening utterances), making the appropriate changes to the
        editor buffers.

        **Note:** this method does not perform adaption of the changed
        utterances.  The caller should do that itself.

        **INPUTS**

        *[INT] changed* -- the utterance numbers of 
        those utterances which were corrected by the user

        **NOTE:** particular implementations of ResMgr may reinterpret 
        all utterances subsequent to the oldest changed utterance
        
        *BOOL delete_tentative_syms = 1* -- If *TRUE*, then remove any tentative
        symbol that do not exist anymore after reinterpretation.


        **OUTPUTS**

        *[INT]* -- the indices onto the stack of recent utterances 
        actually reinterpreted (including intervening ones), sorted 
        with the oldest first, or None if no utterances could be 
        reinterpreted
        """
        return self.editors.reinterpret_recent(instance_name, changed, delete_tentative_syms)


    def can_correct_that_far_back(self, instance_name, utters_to_correct):
        """
        Indicates whether we can correct all the utterances in a list.
        
        **INPUTS**

        *STR instance_name* -- the name of the editor for which corrections are 
        being done.

        *INT utters_to_correct* -- Relative indices (i.e. offset from the 
        end of the recent utterances list) of the utterances that need correction.

        **OUTPUTS**

        *BOOL can_correct* -- True iif we can correct all the utterances in 
        *utters_to_correct*.
        """
        debug.trace('NewMediatorObject.can_correct_that_far_back', 'utters_to_correct=%s' % utters_to_correct)
        n = self.stored_utterances(instance_name)
        recent = self.recent_dictation(instance_name)
        n_recent = 0
        if recent != None:
            n_recent = len(recent)

        earliest = max(utters_to_correct)
        if n < earliest:
            print "\ncan't correct error %d utterances ago" % earliest
            print "because stored_utterances only goes back %d\n" % n
            sys.stdout.flush()
            return 0
        if n_recent < earliest:
            print "\ncan't correct error %d utterances ago" % earliest
            print "because recent_dictation only goes back %d\n" % n_recent
            sys.stdout.flush()
            return 0
        okay = 1
        for i in utters_to_correct:
            if not self.can_reinterpret(instance_name, i):
                print "\ndon't expect to be able to correct error %d utterances ago" % i
                print "because can_reinterpret returned false, but we'll try anyway\n" 
                sys.stdout.flush()
            if not okay:
                return 0
                
        return 1
        
    def correct_symbol_results(self, instance_name, corrections):
        """Correct a list of recently dictated symbols.
        
        **INPUTS**

        *STR instance_name* -- the name of the editor for which corrections are 
        being done.

        *[SymbolResult]* corrections -- List of symbols to reformat

        **OUTPUTS**

        *None*
        """
        corrections_hash = {}
        for a_symbol in corrections:
           in_utter_with_id = a_symbol.in_utter_interp.utterance.id
           in_utter_with_relative_id = self.utter_id_to_rel_utter_index(in_utter_with_id, instance_name)
           spoken_form = string.join(a_symbol.spoken_phrase())
           old_written = a_symbol.native_symbol()
           new_written = a_symbol.reformatted_to
           
           corrections_hash[in_utter_with_relative_id] = \
                  (spoken_form, old_written, new_written)
                                    
           a_symbol.reformat_to(None)
           
        self.correct_recent_symbols(instance_name, corrections_hash)
        
    def correct_recent_symbols(self, instance_name, corrections):
        """Correct a list of recently dictated symbols.
        
        **INPUTS**

        *STR instance_name* -- the name of the editor for which corrections are 
        being done.

        *{INT: (STR, STR, STR)}* -- Description of the symbol corrections to be made.
        The INT key is the RELATIVE index (i.e. index from end of recent utterances list)
        of the utterance where the correction should be made.
        The value is a 3ple of strings, specifying: 
           - the spoken form of the symbol to be corrected
           - its bad written form
           - its correct written form

        **OUTPUTS**

        *None*
        """
        if len(corrections) == 0:
           return
           
        recent = self.recent_dictation(instance_name)
        utters_to_reinterpret_abs_indices = []
        if self.can_correct_that_far_back(instance_name, corrections.keys()):
           for utter_rel_index in corrections.keys():
              spoken_form_used, bad_written, correct_written  \
                 = corrections[utter_rel_index]
              self.correct_symbol(spoken_form_used, bad_written, correct_written)
              utters_to_reinterpret_abs_indices.append(self.rel_utter_index_to_utter_id(utter_rel_index, instance_name))
           
           #
           # Note: When correcting format, we don't try to delete the 
           #       badly formatted symbol even if it was tentative. 
           #       That's because the same symbol may be formatted
           #       in different ways in the same file.
           #
           #          C++ example: CmdInterp cmd_interp;
           #
           self.reinterpret_recent(instance_name, 
                                   utters_to_reinterpret_abs_indices, 
                                   delete_tentative_syms=0)

                          
    def correct_symbol(self, spoken_form, bad_written_form, correct_written_form):
        """Correct the written form of a symbol. This does not
        replace the symbol in the text.

        **INPUTS**
        
        *STR* spoken_form -- Spoken form of the symbol.

        *STR bad_written_form* -- written form that was matched incorrectly to
        *spoken_form*.
        
        *STR correct_written_form* -- written form that SHOULLD have been used instead
        for *spoken_form*.
        
        **OUTPUTS**
        
        *none* -- 
        
        **SIDE EFFECTS**
        
        If *bad_written_form* is a new symbol that was created tentatively on account
        of *spoken_form* beind said, then this method will remove *bad_written_form*
        from the dictionnary altogether. Otherwise, it will simply change the 
        priority of *bad_written_form* and *correct_written_form* for
        *spoken_form*.        
        """
        self.interp.correct_symbol(spoken_form, bad_written_form, correct_written_form)
            
   
    def can_reinterpret(self, instance_name, n):
        """can we safely reinterpret the nth most recent utterance
        into the specified editor

        **INPUTS**

        *STR instance_name* -- the editor 

        *INT n* -- the depth in the editor state stack of the utterance
        to be reinterpreted

        **OUTPUTS**

        *BOOL* -- true if we can safely reinterpret that utterance
        """
        return self.editors.can_reinterpret(instance_name, n = n)

    def correct_utterance(self, instance_name, utterance_number):
        """initiate user correction of the utterance with a given
        utterance number into the given instance

        NOTE: this is a synchronous method which starts a modal
        correction box, and will not return until the user has 
        dismissed the correction box.  Generally, it should be called
        only in response to a CorrectUtterance event, rather than
        in direct response to a spoken correction command.

        **INPUTS**

        *STR instance_name* -- name of the application instance

        *INT utterance_number* -- the number assigned to the utterance by
        interpret_dictation

        **OUTPUTS**

        *none*
        """
        self.editors.correct_utterance(instance_name, utterance_number)

    def correct_recent(self, instance_name):
        """initiate user selection of a recent utterance to correct

        NOTE: this is a synchronous method which starts a modal
        correction box, and will not return until the user has 
        dismissed the correct recent dialog box.  Generally, it should 
        be called only in response to a CorrectRecent event, rather than
        in direct response to a spoken correction command.

        **INPUTS**

        *STR instance_name* -- name of the application instance

        **OUTPUTS**

        *none*
        """
        self.editors.correct_recent(instance_name)

    def reformat_recent(self, instance_name):
        """initiate user selection of a recent symbol to reformat

        NOTE: this is a synchronous method which starts a modal
        correction box, and will not return until the user has 
        dismissed the correct recent dialog box.  Generally, it should 
        be called only in response to a ReformatRecent event, rather than
        in direct response to a spoken reformatting command.

        **INPUTS**

        *STR instance_name* -- name of the application instance

        **OUTPUTS**

        *none*
        """
        debug.trace('NewMediatorObject.reformat_recent', 'invoked with instance_name=%s' % instance_name)
        self.editors.reformat_recent(instance_name)

    
    def input_error(self, message, fatal = 0):
        """called by CmdInterp to indicate that a serious error 
        occurred while trying to read SymDict information from the 
        persistent dictionary file.  
        
        The message will be displayed in a dialog box, if the GUI
        console is available, or printed to stderr.

        NOTE: This method should not be called by
        other object, nor should it be called after CmdInterp has been
        initiated.

        **INPUTS**

        *STR message* -- the message to display

        *BOOL fatal* -- if true, the error is fatal and the mediator
        should clean up and exit once the user has confirmed seeing it

        **OUTPUTS**

        *none*
        """
        if self.console():
            self.console().message_box(message)
        else:
            sys.stderr.write(message)
            sys.stderr.write('\n')
        if fatal:
            self.construction_failed = 1

    def user_message(self, message, instance = None):
        """displays a user message via the appropriate channel
        (e.g. stdout, or a MediatorConsole status line, or an 
        editor-specific status line if supported.

        **INPUTS**

        *STR message* -- the message

        *STR instance_name* -- the editor from which the message
        originated, or None if it is not associated with a specific
        editor.

        **OUTPUTS**

        *none*
        """
        if self.console():
            sent = self.console().user_message(message, 
                instance = instance)
            if sent and not self.testing:
                return
        print message
                        
    def text_mode_toggling (self, on_spoken_as, off_spoken_as, off_sets_nat_text_to=None):
        self.editors.config_text_mode_toggling(on_spoken_as, off_spoken_as, off_sets_nat_text_to)

               

###############################################################################
# Configuration functions. These are not methods
###############################################################################

        
def associate_language(extension, language):
    """Add an association between a file extension and a programming
    language.

    **INPUTS**

    *STR* extension -- file names that end with this extension
    will be asociated with language *languate*

    *STR* language -- name of the programming language        

    **OUTPUTS**

    *none* -- 
    """
    SourceBuff.file_language[extension] = language


def define_language(name, definition):
    """Defines the syntax of a programming language.

    **INPUTS**

    *STR* name -- name of the programming language

    [LangDef] definition -- language definition 


    **OUTPUTS**

    *none* -- 

    .. [LangDef] file:///./LangDef.LangDef.html"""

    definition.name = name
    SymDict.language_definitions[name] = definition


# defaults for vim - otherwise ignore
# vim:sw=4
