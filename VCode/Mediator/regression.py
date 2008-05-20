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

import debug
import Object
# import mediator
import sim_commands
# import MediatorObject
import NewMediatorObject
import CmdInterp
import EdSim
import sr_interface
import traceback
import cPickle
import time
import AppStateEmacs

"""Classes and methods used to set up the MediatorObject or 
NewMediatorObject for the next new regression test
"""

class PersistentConfig(Object.Object):
    """abstract base class which hides the details of MediatorObject 
    and command initialization for regression tests which use a
    persistent MediatorObject.  
    """
    def __init__(self, **args):
        self.deep_construct(PersistentConfig, {}, args)

    def mediator(self):
        """returns a reference to the MediatorObject (or
        NewMediatorObject)

        **INPUTS**

        *none*

        **OUTPUTS**

        *MediatorObject* or *NewMediatorObject*
        """
        return mediator.the_mediator

    def editor(self):
        """returns a reference to the editor instance being used
        for regression testing
            
        **INPUTS**

        *none*

        **OUTPUTS**

        *AppState* -- the editor instance
        """
        debug.virtual('PersistentConfig.editor')


    def instance_name(self):
        """returns the instance name of the editor instance being used
        for regression testing, if we are using NewMediatorObject,
        otherwise None
            
        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the name of the editor instance, or None if we are
        using the old MediatorObject
        """
        debug.virtual('PersistentConfig.instance_name')

    def correction_available(self):
        """indicates whether correction features are available, so that
        tests which depend on these features can be run conditionally

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *STR* -- 'basic' or 'advanced', or None if no correction is
        available
        """
        debug.virtual('PersistentConfig.correction_available')

    def namespace(self):
        """return a reference to the namespace in which commands are
        found

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR:ANY}* -- the namespace (a dictionary)
        """
        debug.virtual('PersistentConfig.namespace')

    def init_simulator_regression(alt_sym_file = None):
        """re-initialize the mediator, using the same editor application

        **NOTE:**  This method must also ensure that the namespace in
        which the regression test definitions are/will be run with
        execfile contains an object called commands with the
        sim_commands commands as attributes.  commands can be a
        reference to the sim_commands module, if a global old
        MediatorObject is used, or an object with methods.

        **INPUTS**

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file, or None to use the same
        file as specified at initial creation of NewMediatorObject

        **OUTPUTS**

        *none*
        """
        debug.virtual('PersistentConfig.init_simulator_regression')

    def execute_command(self, command):
        """execute a command in the proper namespace, trapping any
        exceptions and printing the traceback

        **INPUTS**

        *STR command*

        **OUTPUTS**

        *none*
        """
        debug.virtual('PersistentConfig.execute_command')

class PersistentConfigNewMediator(Object.Object):
    """implementation of PersistentConfig which resets a pre-existing
    NewMediatorObject

    **INSTANCE ATTRIBUTES**

    *NewMediatorObject the_mediator* -- the existing mediator, which will be
    reset by the init_simulator_regression method

    *{STR:ANY} names* -- the namespace dictionary in which the
    regression test definitions file, tests_def, has been (or will be) 
    run with execfile

    *STR correction* -- type of correction available with this mediator
    ('basic', 'advanced', or None)

    *STR editor_name* -- the name of the editor being used for those
    regression tests using a persistent editor 

    *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
    dialog/prompt.  Normally disabled except during regression

    *BOOL bypass_sr_recog* -- bypass natlink for dictation
    utterances
    """
    def __init__(self, mediator, editor_name, names, 
        symbol_match_dlg = 1, correction = None, text_mode_toggling = None,
        bypass_sr_recog = 0, num_words_training = 0, **args):
        """**INPUTS**

        *{STR:ANY} names* -- the namespace dictionary in which the
        regression test definitions file, tests_def, has been (or will be) 
        run with execfile

        *NewMediatorObject the_mediator* -- the existing mediator, which will be
        reset by the init_simulator_regression method

        *STR editor_name* -- the name of the editor being used for those
        regression tests using a persistent editor 

        *STR correction* -- type of correction available with this mediator
        ('basic', 'advanced', or None)

        *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
        dialog/prompt.  Normally disabled except during regression

        *BOOL bypass_sr_recog* -- bypass natlink for dictation
        utterances
        """
        self.deep_construct(PersistentConfigNewMediator, 
                            {
                             'the_mediator': mediator,
                             'names': names,
                             'correction': correction,
                             'editor_name': editor_name,
                             'symbol_match_dlg': symbol_match_dlg,
                             'bypass_sr_recog': bypass_sr_recog,
                             'num_words_training': num_words_training,
                            }, args)
        self.names['init_simulator_regression'] = \
            self.init_simulator_regression

    def instance_name(self):
        """returns the instance name of the editor instance being used
        for regression testing
            
        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the name of the editor instance
        """
        return self.editor_name

    def editor(self):
        """returns a reference to the editor instance being used
        for regression testing
            
        **INPUTS**

        *none*

        **OUTPUTS**

        *AppState* -- the editor instance
        """
        return self.mediator().editor_instance(self.instance_name())

    def mediator(self):
        """returns a reference to the MediatorObject (or
        NewMediatorObject)

        **INPUTS**

        *none*

        **OUTPUTS**

        *MediatorObject* or *NewMediatorObject*
        """
        return self.the_mediator

    def correction_available(self):
        """indicates whether correction features are available, so that
        tests which depend on these features can be run conditionally

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *STR* -- 'basic' or 'advanced', or None if no correction is
        available
        """
        return self.correction

    def namespace(self):
        """return a reference to the namespace in which commands are
        found

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR:ANY}* -- the namespace (a dictionary)
        """
        return self.names

    def init_simulator_regression(self, alt_sym_file = None,
        exclusive = 1, print_buffer_content_after_commands=1):
        """re-initialize the mediator, using the same editor application

        **NOTE:**  This method must also ensure that the namespace in
        which the regression test definitions are/will be run with
        execfile contains an object called commands with the
        sim_commands commands as attributes.  commands can be a
        reference to the sim_commands module, if a global old
        MediatorObject is used, or an object with methods.

        **INPUTS**

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file, or None to use the same
        file as specified at initial creation of NewMediatorObject

        BOOL *exclusive* -- true unless the test is a foreground test
        and cannot use exclusive grammars

        **OUTPUTS**

        *none*
        """
# thought this would fix problems with tests_def.no_sr_user, but
# disconnecting still unloads all existing grammars and re-connecting
# doesn't re-load them.
#        sr_interface.connect()
        debug.trace('PersistentConfigNewMediator.init_simulator_regression',
            'about to reset persistent mediator')
        self.mediator().reset(alt_sym_file = alt_sym_file,
            symbol_match_dlg = self.symbol_match_dlg, 
            add_sr_entries_for_LSAs_and_CSCs=0,
            exclusive = exclusive)

        editor = self.mediator().editor_instance(self.editor_name)
        editor.init_for_test()
        interp = self.mediator().interpreter()
        commands = sim_commands.SimCmdsObj(editor, interp, self.names,
            bypass_sr_recog = self.bypass_sr_recog, testing = 1)
        commands.bind_methods(self.names)
        self.names['commands'] = commands

    def execute_command(self, command):
        """execute a command in the proper namespace, trapping any
        exceptions and printing the traceback

        **INPUTS**

        *STR command*

        **OUTPUTS**

        *none*
        """
# right now, this method doesn't detect quit_flag, because neither
# does mediator.execute_command
        try:
            exec command in self.names
        except Exception, err:
            traceback.print_exc()
            
    def kbd_event_sim_factory(self, app):
       """Returns a [KbdEventSim] that can be used to simulate keyboard events
       a given client editor.
    
       **INPUTS**
    
       [AppState] *app* -- the client editor on which we want to simulate
       keyboard events
    
       **OUTPUT**
       
       [KbdEventSim] -- an object through which we can simulate keyboard events
       for that editor.
       
       ..[KbdEventSim] file:///./regression.KbdEventSim.html
       ..[AppState] file:///./AppState.AppState.html"""
       
       # For now, this factory method always returns a MS-Windows style 
       # simulator for all editors.
#       if isinstance(app, AppStateEmacs.AppStateEmacs):
# Temporarily disable creation of Emacs style kbd simulator.
       if 0:
          debug.trace('PersistentConfigNewMediator.kbd_event_sim_factory', 'Emacs style')       
          return KbdEventSimEmacs()
       else:
          debug.trace('PersistentConfigNewMediator.kbd_event_sim_factory', 'Windows style')
          return KbdEventSimWindowsStyle()

class TempConfig(Object.Object):
    """abstract base class which hides the details of the internal
    structure of a temporary MediatorObject from regression tests
    which create their own MediatorObject.  
    """
    def __init__(self, **args):
        self.deep_construct(TempConfig, {}, args)

    def mediator(self):
        """returns a reference to the TempConfig's MediatorObject or
        NewMediatorObject 
        """
        debug.virtual('TempConfig.mediator')

    def interpreter(self):
        """returns a reference to the MediatorObject's CmdInterp object
        """
        debug.virtual('TempConfig.interpreter')

    def editor(self):
        """returns a reference to the MediatorObject's internal 
        AppState (usually EdSim) editor 
        """
        debug.virtual('TempConfig.editor')

    def quit(self):
        """cleanup the underlying MediatorObject
        """
        debug.virtual('TempConfig.quit')


class TempConfigFactory(Object.Object):
    """abstract base class which hides the details of MediatorObject 
    and command initialization for regression tests which create their
    own MediatorObject.  
    """
    def __init__(self, **args):
        self.deep_construct(TempConfigFactory, {}, args)

    def new_config(self, editor = None, skip_config = 0, 
        alt_sym_file = None):
        """create a new TempConfig object

        **INPUTS**

        *AppState editor* -- the internal test editor to use, or None to
        create a new EdSim instance

        *BOOL skip_config* -- flag allowing you to create a
        MediatorObject without configuring it 

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file, or None to use the same
        file as specified at initial creation of NewMediatorObject
        """
        debug.virtual('TempConfigFactory.new_config')

class TempConfigNewMediator(Object.Object):
    """implementation of TempConfigFactory using NewMediatorObject

    **INSTANCE ATTRIBUTES**

    *NewMediatorObject the_mediator* -- the temporary NewMediatorObject

    *AppState the_editor* -- the editor used for tests with this
    mediator
    """
    def __init__(self, mediator, editor, **args):
        self.deep_construct(TempConfigNewMediator, 
                            {
                             'the_mediator': mediator,
                             'the_editor': editor
                            }, args)

    def mediator(self):
        """returns a reference to the TempConfig's MediatorObject or
        NewMediatorObject 
        """
        return self.the_mediator

    def interpreter(self):
        """returns a reference to the MediatorObject's CmdInterp object
        """
        return self.the_mediator.interpreter()

    def editor(self):
        """returns a reference to the MediatorObject's internal 
        AppState (usually EdSim) editor 
        """
        return self.the_editor

    def quit(self):
        """cleanup the underlying MediatorObject
        """
        self.the_editor = None
        self.the_mediator.quit(save_speech_files=0, 
            disconnect=0)
        self.the_mediator.cleanup()
        self.the_mediator = None

class TempConfigNewMediatorFactory(Object.Object):
    """implementation of TempConfigFactory using NewMediatorObject

    **INSTANCE ATTRIBUTES**

    *BOOL symbol_match_dlg* -- use a CmdInterp with symbol match 
    dialog/prompt.  Normally disabled except during regression

    *STR pickled_interp* -- CmdInterp as originally configured,
    pickled to speed up regression testing
    """
    def __init__(self, symbol_match_dlg = 1, pickled_interp = None, **args):
        self.deep_construct(TempConfigNewMediatorFactory, 
            {'symbol_match_dlg': symbol_match_dlg,
             'pickled_interp': pickled_interp}, args)

    def new_config(self, editor = None, skip_config = 0, 
        alt_sym_file = None):
        """create a new TempConfig object

        **INPUTS**

        *AppState editor* -- the internal test editor to use, or None to
        create a new EdSim instance

        *BOOL skip_config* -- flag allowing you to create a
        MediatorObject without configuring it 

        STR *alt_sym_file=None* -- Name of an alternative file
        containing the persistent version of the symbols dictionary, to
        be used in place of the default file, or None to use the same
        file as specified at initial creation of NewMediatorObject

        **OUTPUTS**

        *TempConfigNewMediator* --  the TempConfigNewMediator object
        """
        interp = None
        if not skip_config and self.pickled_interp:
            interp = cPickle.loads(self.pickled_interp)
        a_mediator = \
            NewMediatorObject.NewMediatorObject(interp = interp, 
                symbol_match_dlg = self.symbol_match_dlg,
                alt_sym_file = alt_sym_file, temporary = 1)
        exclude_interp = 0
        if self.pickled_interp:
            exclude_interp = 1
        if not skip_config:
            a_mediator.configure(exclude_interp = exclude_interp, testing = 1)
        if editor == None:
            app = EdSim.EdSim()
        else:
            app = editor
        a_mediator.new_editor(app, server = 0, check_window = 0)
# we do NOT call this a test_editor, otherwise we're in for some nasty
# recursion (well, maybe not, since we didn't specify test_args, but
# anyway)
        return TempConfigNewMediator(mediator = a_mediator, editor = app)


class KbdEventSim(Object.Object):
    """Class for simulating keyboard user actions (e.g. typing text, 
    changing selection).

    **INSTANCE ATTRIBUTES**
    
    *none*

    **CLASS ATTRIBUTES**
        
    *none*

    """

    def __init__(self, **args):
        self.deep_construct(TempConfigNewMediatorFactory, 
            {}, args)

    def echo_kbd_event(self, evt_name, *args):
       echo_msg = "Got simulated kbd event: %s(" % evt_name
       for an_arg in args:
           echo_msg = echo_msg + "%s," % an_arg
       echo_msg = echo_msg + ")"
       print echo_msg


    def set_selection_by_kbd(self, direction, length, echo_evt=1):
        debug.virtual('KbdEventSim.set_selection_by_kbd')
        
    def move_cursor_by_kbd(self, direction, num_steps, echo_evt=1):
        debug.virtual('KbdEventSim.move_cursor_by_kbd')    
        
    def type_text(self, text, echo_evt=1):
        debug.virtual('KbdEventSim.type_text')
        

class KbdEventSimWindowsStyle(KbdEventSim):
    """Class for simulating keyboard user actions (e.g. typing text, 
    changing selection) using windows-style keyboard key sequences.

    **INSTANCE ATTRIBUTES**
    
    *none*

    **CLASS ATTRIBUTES**
        
    *none*

    """

    def __init__(self, **args_super):
        self.deep_construct(KbdEventSimWindowsStyle, 
                            {}, 
                            args_super, 
                            {})
                                
                            
    def move_cursor_by_kbd(self, direction, num_steps, echo_evt=1):
        if echo_evt: self.echo_kbd_event('move_cursor_by_kbd', direction, num_steps)
        string_to_send = ''
        move_with_key = "{%s" % direction
        string_to_send = '%s %d}' % (move_with_key, num_steps)
        sr_interface.send_keys(string_to_send, 1)
        
                        
    def set_selection_by_kbd(self, direction, length, echo_evt=1):    
        if echo_evt: self.echo_kbd_event('set_selection_by_kbd', direction, length)
        string_to_send = ''
        move_with_key = '{Shift+Ext%s' % direction
        string_to_send = '%s %d}' % (move_with_key, length)
        sr_interface.send_keys(string_to_send, 1)
        
        
    def type_text(self, text, echo_evt=1):
        if echo_evt: self.echo_kbd_event('type_text', text)
        sr_interface.send_keys(text)
        


class KbdEventSimEmacs (KbdEventSimWindowsStyle):
    """Class for simulating keyboard user actions (e.g. typing text, 
    changing selection) for Emacs.

    **INSTANCE ATTRIBUTES**
    
    *none*

    **CLASS ATTRIBUTES**
        
    *none*

    """

    def __init__(self, **args_super):
        self.deep_construct(KbdEventSimEmacs, 
                            {}, 
                            args_super, 
                            {})
                                                                               
    def set_selection_by_kbd(self, direction, length, echo_evt=1):    
        """For Emacs, sending {Shift+Right} and {Shift+Left} from NatSpeak do not change the 
        selection. This is true whether it's done by VoiceCode or a NatSpeak macro.
        
        Rather than trying to figure out why that is, we use native Emacs key sequences for 
        changing the selection.
        """
        if echo_evt: self.echo_kbd_event('set_selection_by_kbd', direction, length)
        sr_interface.send_keys('{Ctrl+Space}', 1)
        self.move_cursor_by_kbd(direction, length, echo_evt=0)
        time.sleep(5)
        
        
    def type_text(self, text, echo_evt=1):
        if echo_evt: self.echo_kbd_event('stype_text', text)
        sr_interface.send_keys(text)



# defaults for vim - otherwise ignore
# vim:sw=4
