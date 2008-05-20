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
# (C)2000, David C. Fox
#
##############################################################################

"""abstract class defining interface for an object which receives 
recognition-starting (or onBegin/gotBegin) callbacks, figures out which
application and buffer are active, and tells the GramMgr to activate the
appropriate grammars.
"""

import debug
import natlink
import string
import re
import sys
from Object import Object, OwnerObject

import GramMgr
import ResMgr

import TargetWindow, KnownTargetModule, WinIDClient
import messaging

class KnownInstance(Object):
    """class which stores data about instances known to RecogStartMgr

    **INSTANCE ATTRIBUTES**

    *STR* module_name -- name of the module corresponding to this
    instance (all windows of an instance belong to the same
    module)
    
    *[INT]* instance_windows -- list of handles of known windows
    corresponding to this instance

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, initial_window = None, module_name = None, **args):
        """create the KnownInstance object

        **INPUTS**

        *INT* initial_window -- handle of the initial window for the
        instance, if any

        *STR* module_name -- name of the module of the initial window
        """
        self.deep_construct(KnownInstance,
                            {'module_name': module_name,
                             'instance_windows': []
                            },
                            args)
        if initial_window != None:
            self.instance_windows.append(initial_window)

    def set_module(self, module_name):
        """sets the name of the module corresponding to this
        instance, if it was previously unknown.  **NOTE:**  This 
        method is included only to allow the caller to 
        supply a module name for an instance whose module name was
        unknown when KnownInstance was created.  Once the module name
        is known, it should never change, and therefore set_module will 
        ignore the request unless the current module name is None.

        **INPUTS**

        *STR* module_name -- the name of the module

        **OUTPUTS**

        *BOOL* -- true if the module name was set.  false if it was not
        (because it had previously been set to a value other than None)

        """
        if self.module_name == None:
            self.module_name = module_name
            return 1
        return 0

    def module(self):
        """return the name of the module corresponding to this instance

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the name of the module, or None if it is unknown
        because the instance has no known windows yet
        """
        return self.module_name

    def windows(self):
        """return the windows known to be associated with this module

        **INPUTS**

        *none*

        **OUTPUTS**

        *[INT]* -- the list of window handles associated with the
        instance
        """
        return self.instance_windows

    def add_window(self, window):
        """add a new window to the list of windows associated with this
        module

        **INPUTS**

        *INT* window -- handle of the new window

        **OUTPUTS**

        *BOOL* -- true if the window was not already known
        """
        if window in self.instance_windows:
            return 0
        self.instance_windows.append(window)
        return 1

    def delete_window(self, window):
        """remove a window from the list of windows associated with this
        module

        **INPUTS**

        *INT* window -- handle of the new window

        **OUTPUTS**

        *BOOL* -- true if the window was known
        """
        try:
            self.instance_windows.remove(window)
            return 1
        except ValueError:
            return 0

    def reset_results_mgr(self):
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
        debug.virtual('RecogStartMgr.reset_results_mgr')
  

# Note: While handling recognition-starting callbacks is the primary function 
# of the recognition starting manager, the abstract base class RecogStartMgr 
# does not set the callback, or define the function which handles it directly.
# This is because the number, type, and meaning of arguments passed to that 
# callback function may depend on the speech engine and/or the platform.
# Instead, it defines the private method _recognition_starting which
# the concrete subclass's callback function should invoke.

class RecogStartMgr(OwnerObject):
    """abstract class defining interface for an object which receives 
    recognition-starting (or onBegin/gotBegin) callbacks, figures out which
    application and buffer are active, and tells the GramMgr to activate the
    appropriate grammars.

    **INSTANCE ATTRIBUTES**

    AppMgr *editors* -- the parent AppMgr object, which provides
    information about editor application instances

    BOOL *trust_current_window* -- 1 if RSM should trust that the current
    window corresponds to the editor when the editor first connects to
    VoiceCode, or when it notifies VoiceCode of a new window.
    
    BOOL *is_in_text_mode = 0* -- TRUE iif code dictation is disabled. In that case,
    dictation utterances should be typed as normal text.

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, trust_current_window = 0, **args):
        """
        Note: after construction, you must construct an AppMgr with this
        RecogStartMgr, which will call set_app_mgr to give the
        RecogStartMgr a reference to its parent AppMgr.

        **INPUTS**

        *BOOL* trust_current_window -- 1 if RSM should trust that the current
        window corresponds to the editor when the editor first connects to
        VoiceCode, or when it notifies VoiceCode of a new window.
        """
        self.deep_construct(RecogStartMgr,
                            {'editors': None,
                             'trust_current_window': trust_current_window,
                             'is_in_text_mode': 0
                            },
                            args)
        self.name_parent('editors')
        
    def set_exclusive(self, exclusive = 1, instance = None):
        """makes the grammars exclusive (or not).  Generally used only
        for background regression testing

        **INPUTS**

        *BOOL* exclusive -- true if all grammars for a given instance
        should be exclusive

        *STR instance* -- name of the editor instance, or None to change
        for all instances

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.set_exclusive')

    def user_message(self, message, instance = None):
        """sends a user message up the chain to the NewMediatorObject to
        be displayed

        **INPUTS**

        *STR message* -- the message

        *STR instance_name* -- the editor from which the message
        originated, or None if it is not associated with a specific
        editor.

        **OUTPUTS**

        *none*
        """
        self.editors.user_message(message, instance = instance)

    def set_app_mgr(self, manager):
        """fill in the reference to the parent AppMgr

        **INPUTS**

        *AppMgr* manager -- the parent AppMgr

        **OUTPUTS**

        *none*
        """
        self.editors = manager
    
    def app_instance(self, instance):
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
        return self.editors.app_instance(instance)
          
        
    def set_text_mode(self, set_to):
        """Sets text mode on/off. In text mode, dictation utterances are
        typed as regular text instead of being translated to code.

        **INPUTS**

        BOOL *set_to* -- Set text mode on or off depending on this argument.

        **OUTPUTS**

        *none*
        """
        self.is_in_text_mode = set_to

        
        #
        # Note: Enable NatText when enabling text mode. We could also disable it
        #       when text mode is disabled, but what if the user wants
        #       NatText to stay on in other applications?
        #
        #       Ideally, you would want to remember the state of NatText
        #       and reset NatText to that state when you disable text mode.
        #
        if set_to:
           natlink.execScript('SetNaturalText 1')


    def interpret_dictation(self, instance, result, initial_buffer = None):
        """interpret the result of recognition by a dictation grammar,
        and store the relevant information to allow for correction.

        **INPUTS**

        *STR instance* -- name of the editor instance whose grammar received 
        the recognition

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.interpret_dictation')

    def interpreter(self):
        """return a reference to the mediator's current CmdInterp object

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.editors.interpreter()
    
    def scratch_recent(self, instance, n):
        """undo the effect of the n most recent dictation utterances into 
        the given editor, if possible

        **INPUTS**

        *STR instance* -- name of the editor instance 

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances successfully undone
        """
        debug.virtual('RecogStartMgr.scratch_recent')

    def correct_last(self, instance):
        """initiate user correction of the most recent dictation utterance 
        into the given editor, if possible

        **INPUTS**

        *STR instance* -- name of the editor instance 

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.correct_last')

    def correct_recent(self, instance):
        """initiate user correction of one or more recent dictation 
        utterances into the given editor, if possible

        **INPUTS**

        *STR instance* -- name of the editor instance 

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.correct_recent')

    def correct_utterance(self, instance_name, utterance_number):
        """initiate user correction of the utterance with a given
        utterance number into the given instance

        NOTE: this is a synchronous method which starts a modal
        correction box, and will not return until the user has 
        dismissed the correction box.  Generally, it should be called
        only in response to a CorrectUtterance event, rather than
        in direct response to a spoken correction command.

        **INPUTS**

        *INT utterance_number* -- the number assigned to the utterance by
        interpret_dictation

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.correct_utterance')

    def trust_current(self, trust = 1):
        """specifies whether the RecogStartMgr should trust that the current
        window corresponds to the editor when the editor first connects to
        VoiceCode, or when it notifies VoiceCode of a new window.

        **INPUTS**

        *BOOL* trust -- 1 if RSM should trust that the current
        window corresponds to the editor when the editor first connects to
        VoiceCode, or when it notifies VoiceCode of a new window.

        **OUTPUTS**

        *none*
        """
        self.trust_current_window = trust
        
    def window_info(self):
        """find the window id, title, and module of the current window

        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name.  The
        module name should be converted to all lowercase
        """
        debug.virtual('RecogStartMgr.window_info')

    def activate(self):
        """activate the RecogStartMgr

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if activated successfully
        """
        debug.virtual('RecogStartMgr.activate')

    def deactivate(self):
        """deactivate the RecogStartMgr, and disable all window-specific
        grammars

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.deactivate')

    def remove_other_references(self):
        self.deactivate()
        OwnerObject.remove_other_references(self)

    def add_module(self, module):
        """add a new KnownTargetModule object

        **INPUTS**

        *KnownTargetModule* module -- the new module

        **OUTPUTS**

        *BOOL* -- true unless a module of the same name already exists
        """
        debug.virtual('RecogStartMgr.add_module')

    def known_window(self, window):
        """is window a known window ID?

        **INPUTS**
    
        *INT* window -- window handle of the window

        **OUTPUTS**
        
        *BOOL* -- true if window is a known window 
        """
        debug.virtual('RecogStartMgr.known_window')

    def shared_window(self, window):
        """is window a shared window?

        **INPUTS**
    
        *INT* window -- window handle of the window

        **OUTPUTS**
        
        *BOOL* -- true if window is a known window  and is shared (or
        shareable).  None if window is unknown
        """
        debug.virtual('RecogStartMgr.shared_window')

    def single_display(self, window):
        """is window a single-window display?

        **INPUTS**
    
        *INT* window -- window handle of the window

        **OUTPUTS**
        
        *BOOL* -- true if window is a known window  and is a
        single-window display.  None if window is unknown
        """
        debug.virtual('RecogStartMgr.single_display')

    def known_windows(self, instance = None):
        """list of windows known to be associated with  a particular
        named application instance.

        **INPUTS**
    
        *STR* instance -- list names of windows corresponding to this
        instance name (or list all known windows if instance is
        None)

        **OUTPUTS**
        
        *[INT]* -- list of window handles
        """
        debug.virtual('RecogStartMgr.known_windows')


    def known_module(self, module):
        """is this module known?

        **INPUTS**

        *STR* module -- name of the module (executable as seen by the
        local system)

        **OUTPUTS**

        *BOOL* -- true if the module name is known
        """
        debug.virtual('RecogStartMgr.known_module')
    
    def known_instance(self, instance):
        """is this instance known?

        **INPUTS**

        *STR* instance -- the name of the instance

        **OUTPUTS**

        *BOOL* -- true if the instance is known
        """
        debug.virtual('RecogStartMgr.known_instance')

    def known_instances(self):
        """returns the list of known instances

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of names of known instances
        """
        debug.virtual('RecogStartMgr.known_instances')

    def window_instances(self, window):
        """returns a list of the known instances associated with a given
        window

        **INPUTS**

        *INT* window -- the window handle 

        **OUTPUTS**

        *[STR]* -- list of names of instances associated with the
        window, or None if the window is unknown
        """
        debug.virtual('RecogStartMgr.window_instances')

    def instance_module(self, instance):
        """returns the module associated with the given instance

        **INPUTS**

        *STR* instance -- the name of the instance

        **OUTPUTS**

        *STR* -- the name of the module associated with the instance, or
        None if it is unknown (because the instance has not yet been
        associated with any windows)
        """
        debug.virtual('RecogStartMgr.instance_module')
    
    def new_instance(self, instance, check_window = 1, window_info = None):
        """method called by AppMgr to notify RecogStartMgr that a new
        editor instance has been added, and (optionally) to tell it to 
        check if the current window belongs to (or contains) that instance
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        *BOOL* check_window -- should we check to see if the
        current window belongs to this instance?

        *(INT, STR, STR) window_info*  -- window id, title, and module of 
        the current window as detected by the TCP server when it
        originally processed the new editor connection, or None to let
        RSM.new_instance check now.  Ignored unless check_window is
        true.

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.new_instance')
    
    def rename_buffer_cbk(self, instance, old_buff_name, new_buff_name):
        """callback from AppMgr which notifies us that the given editor
        instance has renamed a buffer

        **INPUTS**

        *STR* instance -- name of the editor instance 

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.rename_buffer_cbk')

    def close_buffer_cbk(self, instance, buff_name):
        """callback from AppMgr which notifies us that the application
        has closed a buffer

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the buffer which was closed

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr.close_buffer_cbk')

    def new_universal_instance(self, instance, exclusive = 1):
        """method called by AppMgr to notify RecogStartMgr that a new
        test instance has been added which should use global grammars
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        *BOOL* exclusive -- should the instance use exclusive grammars
        as well?

        **OUTPUTS**

        *BOOL* -- true if the instance was added as a universal instance.
        False if there was already such a universal instance, in which case the
        new instance will be added normally, or if the instance name was
        already known.
        """
        debug.virtual('RecogStartMgr.new_universal_instance')

    def delete_instance(self, instance):
        """method called by AppMgr to notify RecogStartMgr that an
        editor instance has been deleted
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        **OUTPUTS**

        *BOOL* -- true if instance was known
        """
        debug.virtual('RecogStartMgr.delete_instance')

    def specify_window(self, instance, window_id = None):
        """called to indicate that user has manually identified a
        known instance with the current window 

        **INPUTS**

        *STR* instance -- name of the application instance

        *INT* window_id -- handle which must match that of the current
        window (otherwise specify_window will ignore the current window
        and return 0), or None if the caller does not know the handle 

        **OUTPUTS**

        *BOOL* -- true if window is added
        """
        debug.virtual('RecogStartMgr.specify_window')

    def app_new_window(self, instance):
        """called when the editor notifies us of a new window for the 
        specified instance

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *BOOL* -- true if window is added
        """
        debug.virtual('RecogStartMgr.app_new_window')

    def delete_window(self, instance, window):
        """remove window from list of known windows
        corresponding to an editor application instance.

        **INPUTS**

        *STR* instance -- name of the application instance 
    
        *INT* window -- window handle of the window

        **OUTPUTS**

        *BOOL* -- true if window and instance are known (otherwise, does
        nothing)
        """
        debug.virtual('RecogStartMgr.delete_window')
    
    def activate_instance_window(self, instance, window):
        """raise instance to front of list of most recently active instances 
        for that window

        **INPUTS**

        *STR* instance -- name of the application instance 

        *INT* window -- window handle of the window

        **OUTPUTS**

        *BOOL* -- true if window and instance are known (otherwise, does
        nothing)
        """
        debug.virtual('RecogStartMgr.activate_instance_window')

    def _recognition_starting(self, window, title, module_name = None):
        """private method which a concrete subclass will call to handle
        the recognition starting event.

        **INPUTS**

        *INT* window -- window handle (unique identifier) of the current 
        window

        *STR* title -- title of the window 

        *STR* module -- filename of the application corresponding to
        this window, or None if the particular subclass of RecogStartMgr
        cannot detect it.  **Note**: the module may not
        be the name of the editor.  For example, for remote editors, the
        module will generally be the name of the telnet/X server
        program, and any application written in python will show up as PYTHON.

        **OUTPUTS**

        *none*
        """
        debug.virtual('RecogStartMgr._recognition_starting')
        
    def config_text_mode_toggling(self, on_spoken_as, off_spoken_as, off_sets_nat_text_to):
        """See [GramMgrFactory.config_text_mode_toggling]
        
        .. [GramMgrFactory.config_text_mode_toggling] file:///./GramMgr.GramMgrFactory.html#config_text_mode_toggling"""
        self.GM_factory.config_text_mode_toggling(on_spoken_as, off_spoken_as, off_sets_nat_text_to)
        
    
class RSMInfrastructure(RecogStartMgr):
    """abstract class defining interface for an object which receives 
    recognition-starting (or onBegin/gotBegin) callbacks, figures out which
    application and buffer are active, and tells the GramMgr to activate the
    appropriate grammars.

    **INSTANCE ATTRIBUTES**

    *{INT : TargetWindow}* windows -- map from currently known window 
    IDs to TargetWindow objects

    *{STR : KnownTargetModule}* modules -- map from currently known
    module names to KnownTargetModule objects

    *{STR : ResMgr}* results -- map from instance names to
    ResMgr objects which manage results of the dictation
    grammars for that editor instance

    *{STR : [INT]}* instances -- map from instance names to 
    KnownInstance objects

    *{STR: GramMgr}* grammars -- map from instance names to 
    grammar managers for creating, activating, and deactivating 
    grammars for that instance

    *GramMgrFactory* GM_factory -- GramMgrFactory to create GramMgr
    objects for new instances

    *ResMgrFactory* res_mgr_factory -- factory to create ResMgr objects
    for new instances

    *BOOL* active -- flag indicating whether the RecogStartMgr is
    active

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, GM_factory, res_mgr_factory = None, **args):
        """
        **INPUTS**

        *GramMgrFactory* GM_factory -- GramMgrFactory to create GramMgr
        objects for new instances

        *ResMgrFactory* res_mgr_factory -- factory for creating new
        ResMgr objects

        """
        self.deep_construct(RSMInfrastructure,
                            {'active': 0,
                             'GM_factory': GM_factory,
                             'res_mgr_factory': res_mgr_factory,
                             'grammars': {},
                             'windows': {},
                             'results': {},
                             'modules': {},
                             'instances': {}
                            },
                            args)
        self.add_owned('grammars')
        self.add_owned('results')
        if self.res_mgr_factory is None:
            self.res_mgr_factory = ResMgr.ResMgrStdFactory()
        
    def remove_other_references(self):
        self.res_mgr_factory = None
        RecogStartMgr.remove_other_references(self)

    def activate(self):
        """activate the RecogStartMgr

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if activated successfully
        """
        if not self.active and hasattr(self, '_activate_detection'):
            self._activate_detection()
        self.active = 1

    def deactivate(self):
        """deactivate the RecogStartMgr, and disable all window-specific
        grammars

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self._deactivate_all_grammars()
        if self.active and hasattr(self, '_deactivate_detection'):
            self._deactivate_detection()
        self.active = 0

    def set_exclusive(self, exclusive = 1, instance = None):
        """makes the grammars exclusive (or not).  Generally used only
        for background regression testing

        **INPUTS**

        *BOOL* exclusive -- true if all grammars for a given instance
        should be exclusive

        *STR instance* -- name of the editor instance, or None to change
        for all instances

        **OUTPUTS**

        *none*
        """
        if instance is None:
            for instance in self.known_instances():
                self.set_exclusive(exclusive, instance)
        elif self.known_instance(instance):
            self.grammars[instance].set_exclusive(exclusive)

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
        self.GM_factory.capitalize_rules(capitalize)

    def add_module(self, module):
        """add a new KnownTargetModule object

        **INPUTS**

        *KnownTargetModule* module -- the new module

        **OUTPUTS**

        *BOOL* -- true unless a module of the same name already exists
        """
        module_name = module.name()
        # win9x and NT differ on case-convention here, so standardize
        module_name = string.lower(module_name) 
        if self.known_module(module_name):
            return 0
        self.modules[module_name] = module
        return 1

    def known_window(self, window):
        """is window a known window ID?

        **INPUTS**
    
        *INT* window -- window handle of the window

        **OUTPUTS**
        
        *BOOL* -- true if window is a known window 
        """
        if not self.windows.has_key(window):
            return 0
#        if self.windows[window].instances() == 0:
#            return 0
        return 1

    def shared_window(self, window):
        """is window a shared window?

        **INPUTS**
    
        *INT* window -- window handle of the window

        **OUTPUTS**
        
        *BOOL* -- true if window is a known window  and is shared (or
        shareable).  None if window is unknown
        """
        if self.known_window(window):
            return self.windows[window].shared()
        return None

    def single_display(self, window):
        """is window a single-window display?

        **INPUTS**
    
        *INT* window -- window handle of the window

        **OUTPUTS**
        
        *BOOL* -- true if window is a known window  and is a
        single-window display.  None if window is unknown
        """
        if self.known_window(window):
            return self.windows[window].single_display()
        return None

    def known_windows(self, instance = None):
        """list of windows known to be associated with  a particular
        named application instance.

        **INPUTS**
    
        *STR* instance -- list names of windows corresponding to this
        instance name (or list all known windows if instance is
        None)

        **OUTPUTS**
        
        *[INT]* -- list of window handles
        """
        if instance == None:
            return self.windows.keys()

        if not self.instances.has_key(instance):
            return []
        return self.instances[instance].windows()

    def known_module(self, module):
        """is this module known?

        **INPUTS**

        *STR* module -- name of the module (executable as seen by the
        local system)

        **OUTPUTS**

        *BOOL* -- true if the module name is known
        """
        if self.modules.has_key(module):
            return 1
        return 0
    
    def known_instance(self, instance):
        """is this instance known?

        **INPUTS**

        *STR* instance -- the name of the instance

        **OUTPUTS**

        *BOOL* -- true if the instance is known
        """
        if self.instances.has_key(instance):
            return 1
        return 0

    def known_instances(self):
        """returns the list of known instances

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of names of known instances
        """
        return self.instances.keys()

    def window_instances(self, window):
        """returns a list of the known instances associated with a given
        window

        **INPUTS**

        *INT* window -- the window handle 

        **OUTPUTS**

        *[STR]* -- list of names of instances associated with the
        window, or None if the window is unknown
        """
        if not self.known_window(window):
            return None
        return self.windows[window].instance_names()

    def instance_module(self, instance):
        """returns the module associated with the given instance

        **INPUTS**

        *STR* instance -- the name of the instance

        **OUTPUTS**

        *STR* -- the name of the module associated with the instance, or
        None if it is unknown (because the instance has not yet been
        associated with any windows)
        """
        if not self.known_instance(instance):
            return None
        return self.instances[instance].module()
    
    def interpret_dictation(self, instance, result, initial_buffer = None):
        """interpret the result of recognition by a dictation grammar,
        and store the relevant information to allow for correction.

        **INPUTS**

        *STR instance* -- name of the editor instance whose grammar received 
        the recognition

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        debug.trace('RSMInfrastructure.interpret_dictation', 
            'instance = %s, result = %s, initial_buffer=%s' % (instance, repr(result.words()), initial_buffer))
        if self.known_instance(instance):
            debug.trace('RSMInfrastructure.interpret_dictation', 
                'known instance')
            try:
                self.results[instance].interpret_dictation(result,
                    initial_buffer = initial_buffer)
            except messaging.SocketError:
                debug.trace('RSMInfrastructure.interpret_dictation', 
                    'socket error while interpreting dictation')
                self.editors.close_app_cbk(instance, unexpected = 1)
                debug.trace('RSMInfrastructure.interpret_dictation', 
                    'closed editor %s' % instance)

    def correct_last(self, instance):
        """initiate user correction of the most recent dictation utterance 
        into the given editor, if possible

        **INPUTS**

        *STR instance* -- name of the editor instance 

        **OUTPUTS**

        *none*
        """
        if self.known_instance(instance):
            self.results[instance].correct_last()

    def correct_recent(self, instance):
        """initiate user correction of one or more recent dictation 
        utterances into the given editor, if possible

        **INPUTS**

        *STR instance* -- name of the editor instance 

        **OUTPUTS**

        *none*
        """
        if self.known_instance(instance):
            self.results[instance].correct_recent()

    def reformat_recent(self, instance):
        if self.known_instance(instance):
            self.results[instance].reformat_recent()
                        
    def correct_recent_synchronous(self, instance):
        """initiate user correction of one or more recent dictation 
        utterances into the given editor, if possible

        **INPUTS**

        *STR instance* -- name of the editor instance 

        **OUTPUTS**

        *none*
        """
        if self.known_instance(instance):
            self.results[instance].correct_recent_synchronous()

    def correct_utterance(self, instance_name, utterance_number):
        """initiate user correction of the utterance with a given
        utterance number into the given instance

        NOTE: this is a synchronous method which starts a modal
        correction box, and will not return until the user has 
        dismissed the correction box.  Generally, it should be called
        only in response to a CorrectUtterance event, rather than
        in direct response to a spoken correction command.

        **INPUTS**

        *INT utterance_number* -- the number assigned to the utterance by
        interpret_dictation

        **OUTPUTS**

        *none*
        """
        if self.known_instance(instance_name):
            self.results[instance_name].correct_utterance(utterance_number)
            
    def reformat_recent_synchronous(self, instance):
        """initiate user reformatting of one or more recent symbols 
        uttered into the given editor, if possible

        **INPUTS**

        *STR instance* -- name of the editor instance 

        **OUTPUTS**

        *none*
        """
        if self.known_instance(instance):
            self.results[instance].reformat_recent_synchronous()

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
        if instance_name != None:
            if self.known_instance(instance_name):
                self.results[instance_name] = \
                    self.res_mgr_factory.new_manager(recog_mgr = self,
                        instance_name = instance_name)
        else:
            for name in self.known_instances():
                self.reset_results_mgr(name)

    def console(self):
        """returns a reference to the MediatorConsole which provides the
        GUI correction interfaces.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.editors.console()

  
    def stored_utterances(self, instance_name):
        """queries the ResMgr to see how many dictated utterances have 
        been stored for the specified editor

        **INPUTS**

        *STR instance_name* -- the editor 

        **OUTPUTS**

        *INT* -- number of utterances which can be retrieved with
        recent_dictation
        """
        try:
            return self.results[instance_name].stored_utterances()
        except KeyError:
            return 0

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
        try:
            return self.results[instance_name].recent_dictation(n = n)
        except KeyError:
            return None
            
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
        try:
            return self.results[instance_name].recent_symbols(n = n)
        except KeyError:
            return None


    def scratch_recent(self, instance_name, n = 1):
        """undo the effect of the most recent n utterances into the
        specified editor, if possible.

        **INPUTS**

        *STR instance_name* -- the editor 

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances actually undone
        """
        debug.trace('RSMInfrastructure.scratch_recent', 'instance_name=%s, n=%s' % (instance_name, n) )
        try:
            debug.trace('RSMInfrastructure.scratch_recent', 
                'instance %s' % instance_name)
            return self.results[instance_name].scratch_recent(n = n)
        except KeyError:
            debug.trace('RSMInfrastructure.scratch_recent', 
                'unknown instance %s' % instance_name)
            return 0

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
        try:
            return self.results[instance_name].reinterpret_recent(changed, delete_tentative_syms)
        except KeyError:
            return None
   
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
        try:
            return self.results[instance_name].can_reinterpret(n = n)
        except KeyError:
            return 0
   
    def _add_instance(self, instance, window_id = None, module = None):
        """private method to add a new instance with a given target
        window and module

        **INPUTS**

        *STR* instance -- the name of the instance

        *INT* window_id -- the initial window handle of the instance,
        or None if the instance has no associated window yet

        *STR* module -- the module name corresponding to the window, 
        or None if the instance has no associated window yet

        **OUTPUTS**

        *BOOL* -- true if instance is added successfully.  False if
        there is already an instance with that name.

        """
        if self.known_instance(instance):
            return 0
        debug.trace('RSMInfrastructure._add_instance', 'adding instance name: %s' % instance)
        self.instances[instance] = KnownInstance(window_id, module)
        app = self.app_instance(instance)
        debug.trace('RecogStartMgr._add_instance', 'new manager')
        self.grammars[instance] = \
            self.GM_factory.new_manager(app, instance_name = instance, 
                recog_mgr = self)
        self.results[instance] = self.res_mgr_factory.new_manager(recog_mgr = self,
            instance_name = instance)
        return 1

    def _add_known_window(self, window_id, window, instance):
        """private method called internally to add a new
        TargetWindow object to the map of windows

        **INPUTS **

        *INT* window_id -- the window handle of the newly identified
        window

        *TargetWindow* window -- the TargetWindow object

        *STR* instance -- the name of the corresponding instance

        **OUTPUTS**

        *BOOL* -- true if the window was added successfully
        """
        if self.known_window(window_id):
            return 0
        self.windows[window_id] = window
        old_module = self.instances[instance].module()
        success = self.instances[instance].add_window(window_id)
        if success and old_module == None:
            app = self.app_instance(instance)
            module_name = window.module_name()
            self.instances[instance].set_module(module_name)
            module = self.modules[module_name]
            title_escape = module.title_escape_sequence()
            app.title_escape_sequence(title_escape[0], title_escape[1])
        if success:
            self.grammars[instance].new_window(window_id)
        return success

    
    def _add_instance_to_window(self, window, instance):
        """private method called internally to add a new
        instance to a TargetWindow object 

        **INPUTS **

        *INT* window -- the window handle of the newly identified
        window

        *STR* instance -- the name of the corresponding instance

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
        if not self.known_window(window):
            return 0
        if not self.known_instance(instance):
            return 0
        if not self.windows[window].add_instance(instance):
            return 0
        self.instances[instance].add_window(window)
        old_module = self.instances[instance].module()
        if old_module == None:
            app = self.app_instance(instance)
            module_name = self.windows[window].module_name()
            self.instances[instance].set_module(module_name)
            module = self.modules[module_name]
            title_escape = module.title_escape_sequence()
            app.title_escape_sequence(title_escape[0], title_escape[1])
        self.grammars[instance].new_window(window)
        return 1

    def _new_instance_known_window(self, window, title, instance, 
        trust = 0):
        """private method called internally to verify that the named
        instance belongs to the given known window, and if so to add 
        it to the window 

        **INPUTS**

        *INT* window -- the handle of the known window

        *STR* title -- the current title of the window

        *STR* instance -- the name of the instance

        *BOOL* trust -- trust that instance belongs to the window, even
        if we can't verify it definitively, because the user has
        manually specified the window

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
        debug.virtual('RSMInfrastructure._new_instance_known_window')

    def _new_instance_known_module(self, window, title, instance, module_name,
        trust = 0):
        """private method called internally to verify that the named
        instance belongs to the given unknown window.  If so, a 
        TargetWindow object is created and added to the 
        known windows map.

        **INPUTS**

        *INT* window -- the handle of the known window

        *STR* title -- the current title of the window

        *STR* instance -- the name of the instance

        *STR* module_name -- name of the module 

        *BOOL* trust -- trust that instance belongs to the window, even
        if we can't verify it definitively, because the user has
        manually specified the window

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
        debug.virtual('RSMInfrastructure._new_instance_known_module')

    def new_instance(self, instance, check_window = 1, window_info = None):
        """method called by AppMgr to notify RecogStartMgr that a new
        editor instance has been added, and (optionally) to tell it to 
        check if the current window belongs to (or contains) that instance
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        *BOOL* check_window -- should we check to see if the
        current window belongs to this instance?

        *(INT, STR, STR) window_info*  -- window id, title, and module of 
        the current window as detected by the TCP server when it
        originally processed the new editor connection, or None to let
        RSM.new_instance check now.  Ignored unless check_window is
        true.

        **OUTPUTS**

        *none*
        """
        if not self._add_instance(instance):
            return
        if check_window:
            if window_info == None:
                window, title, module_name = self.window_info()
            else:
                window, title, module_name = window_info
            module_name = string.lower(module_name) 
            if self.known_window(window):
                debug.trace('RSMInfrastructure.new_instance',
                            'window_info = %s' % window_info)             
                self._new_instance_known_window(window, title, instance,
                                                trust = self.trust_current_window)
            elif self.known_module(module_name):
                self._new_instance_known_module(window, title, 
                    instance, module_name, trust = self.trust_current_window)
            else:
                debug.trace('RecogStartMgr.new_instance', 
                    'neither window nor module is known')

    def rename_buffer_cbk(self, instance, old_buff_name, new_buff_name):
        """callback from AppMgr which notifies us that the given editor
        instance has renamed a buffer

        **INPUTS**

        *STR* instance -- name of the editor instance 

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        if not self.known_instance(instance):
            return 
        self.grammars[instance].rename_buffer_cbk(old_buff_name, new_buff_name)
        self.results[instance].rename_buffer_cbk(old_buff_name, new_buff_name)

    def close_buffer_cbk(self, instance, buff_name):
        """callback from AppMgr which notifies us that the application
        has closed a buffer

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the buffer which was closed

        **OUTPUTS**

        *none*
        """
        if not self.known_instance(instance):
            return 
        self.grammars[instance].buffer_closed(buff_name)
        self.results[instance].close_buffer_cbk(buff_name)

    def delete_instance(self, instance):
        """method called by AppMgr to notify RecogStartMgr that an
        editor instance has been deleted
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        **OUTPUTS**

        *BOOL* -- true if instance was known
        """
        if not self.instances.has_key(instance):
            return 0
        windows = self.known_windows(instance)
        for window in windows:
            self.windows[window].delete_instance(instance)
            if self.windows[window].instances() == 0:
                del self.windows[window]
        debug.trace('RSMInfrastructure.delete_instance', 'deleting instance name: %s' % instance)
        del self.instances[instance]
        self.grammars[instance].cleanup()
        self.results[instance].cleanup()
        del self.grammars[instance]
        del self.results[instance]

    def specify_window(self, instance, window_id = None):
        """called to indicate that user has manually identified a
        known instance with the current window 

        **INPUTS**

        *STR* instance -- name of the application instance

        *INT* window_id -- handle which must match that of the current
        window (otherwise specify_window will ignore the current window
        and return 0), or None if the caller does not know the handle 

        **OUTPUTS**

        *BOOL* -- true if window is added
        """
# we still want to check for consistency
#        print 'rsm specify'
#        print instance
#        print self.instances.keys()
        if not self.known_instance(instance):
            return 0
        window, title, module_name = self.window_info()
        if window_id != None and window != window_id:
            return 0
#        print self.window_info()
        if self.known_window(window):
#            print 'specify - known window'
            return self._new_instance_known_window(window, 
                title, instance, trust = 1)
        elif self.known_module(module_name):
#            print 'specify - known module'
            return self._new_instance_known_module(window, title, 
                instance, module_name, trust = 1)
#        print 'specify - unknown module'


    def app_new_window(self, instance):
        """called when the editor notifies us of a new window for the 
        specified instance

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *BOOL* -- true if window is added
        """
        if not self.known_instance(instance):
            return 0
        window, title, module_name = self.window_info()
        if self.known_window(window):
            return 0
        if not self.known_module(module_name):
            return 0
        old_module = self.instances[instance].module()
        if old_module != None:
            if module_name != old_module:
                return 0
        return self._new_instance_known_module(window, title, 
                instance, module_name, trust = self.trust_current_window)

    def delete_window(self, instance, window):
        """remove window from list of known windows
        corresponding to an editor application instance.

        **INPUTS**

        *STR* instance -- name of the application instance 
    
        *INT* window -- window handle of the window

        **OUTPUTS**

        *BOOL* -- true if window and instance are known (otherwise, does
        nothing)
        """
        if not self.known_instance(instance):
            return 0
        if window not in self.known_windows(instance):
            return 0
        self.windows[window].delete_instance(instance)
        if self.windows[window].instances() == 0:
            del self.windows[window]
        self.instances[instance].delete_window(window)
        self.grammars[instance].delete_window(window)
        return 1
    
    def activate_instance_window(self, instance, window):
        """raise instance to front of list of most recently active instances 
        for that window

        **INPUTS**

        *STR* instance -- name of the application instance 

        *INT* window -- window handle of the window

        **OUTPUTS**

        *BOOL* -- true if window and instance are known (otherwise, does
        nothing)
        """
        if not self.known_instance(instance):
            return 0
        if window not in self.known_windows(instance):
            return 0
        return self.windows[window].activate_instance(instance)

    def _activate_grammars(self, app, instance_name, window):
        """private method used to activate grammars for the current
        buffer of an identified editor and window, assuming that the
        buffer is VoiceCode-enabled
        
        **INPUTS**

        *AppState* app -- the editor application interface

        *STR* instance_name -- instance name

        *INT* window -- window handle of the window to be speech-enabled

        **OUTPUTS**

        *none*
        """
# ensure that AppState is synchronized with the editor
#        app.synchronize_with_app()
#        self.activate_instance_window(instance_name, window)
#        buff_name = app.curr_buffer_name()
#        dictation_allowed = app.recog_begin(window)

# try this order to see if it fixes problem with Emacs with multiple
# windows
        self.activate_instance_window(instance_name, window)
        dictation_allowed = app.recog_begin(window)
        app.synchronize_with_app()
        buff_name = app.curr_buffer_name()
        if app.active_field() == None and dictation_allowed:
            self.grammars[instance_name].activate(buff_name, window, is_in_text_mode = self.is_in_text_mode)
        else:
            self.grammars[instance_name].deactivate_all(window)
        others = self.windows[window].instance_names()
        if self.GM_factory.using_global():
            others = self.known_instances()
        for editor in others:
            if editor != instance_name:
                self.grammars[editor].deactivate_all(window)
            
    def _deactivate_all_grammars(self):
        """private method used to deactivate all grammars 
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        for instance in self.instances.keys():
            self.grammars[instance].deactivate_all()
        
    def _deactivate_grammars(self, window):
        """private method used to deactivate all grammars for a given
        window
        
        **INPUTS**

        *INT* window -- window handle 

        **OUTPUTS**

        *none*
        """
        for instance in self.windows[window].instance_names():
            self.grammars[instance].deactivate_all(window)
        
    def _recognition_starting_known_window(self, window, title):
        """private method called by _recognition_starting when the
        current window is known.

        **INPUTS**

        *INT* window -- window handle (unique identifier) of the current 
        window

        *STR* title -- title of the window 

        **OUTPUTS**

        *none*
        """
        win = self.windows[window]
        debug.trace('RecogStartMgr._recognition_starting_known_window',
            'known window: %s' % repr(win))
        instance = win.active_instance(title, self.editors)
        debug.trace('RecogStartMgr._recognition_starting_known_window',
            'active instance = "%s"' % instance)
        if instance == None:
            self._deactivate_grammars(window)
            return
        app = self.app_instance(instance)
        if app == None:
            debug.trace('RecogStartMgr._recognition_starting_known_window',
                'unknown instance')
            self._deactivate_grammars(window)
            return
        debug.trace('RecogStartMgr._recognition_starting_known_window',
            'activating grammars')
        try:
            self._activate_grammars(app, instance, window)
        except messaging.SocketError:
            self.editors.close_app_cbk(instance, unexpected = 1)
            self._deactivate_grammars(window)
        return

    def _recognition_starting_known_module(self, window, title, module_name):
        """private method called by _recognition_starting when the
        current window is not a previously known window, but the 
        current module is known.

        **INPUTS**

        *INT* window -- window handle (unique identifier) of the current 
        window

        *STR* title -- title of the window 

        *STR* module_name -- filename of the application corresponding to
        this window.  **Note**: the module may not
        be the name of the editor.  For example, for remote editors, the
        module will generally be the name of the telnet/X server
        program, and any application written in python will show up as PYTHON.

        **OUTPUTS**

        *none*
        """
        debug.virtual('RSMInfrastructure._recognition_starting_known_module')
    
    def _recognition_starting(self, window, title, module_name = None):
        """private method which a concrete subclass will call to handle
        the recognition starting event.

        **INPUTS**

        *INT* window -- window handle (unique identifier) of the current 
        window

        *STR* title -- title of the window 

        *STR* module -- filename of the application corresponding to
        this window, or None if the particular subclass of RecogStartMgr
        cannot detect it.  **Note**: the module may not
        be the name of the editor.  For example, for remote editors, the
        module will generally be the name of the telnet/X server
        program, and any application written in python will show up as PYTHON.

        **OUTPUTS**

        *none*
        """
        if self.active:
            debug.trace('RSMInfrastructure.new_instance',
                        'window, title, module_name = %d, %s, %s' % (window, title, module_name)) 
            if self.known_window(window):
                self._recognition_starting_known_window( window, title)
            elif self.known_module(module_name):
                self._recognition_starting_known_module(window, title, 
                    module_name)
    
    def _assign_ID_client(self, module_name, window, title):
        """check if there is a WinIDClient not yet assigned to a window.
        If so, attempt to assign it to the specified window, and return
        a TargetWindow object.

        **INPUTS**

        *STR* module_name -- the name of the module corresponding to the
        window

        *INT* window -- the window handle of the window

        *STR* title -- the title of the window
    
        **OUTPUTS**

        *TargetWindow* -- an object of subclass of TargetWindow managed
        by the unassigned WinIDClient, or None if there is no unassigned 
        client, or the given window is not managed by that client.
        """
# for now, WinIDClient objects are handled by the AppMgr
        return self.editors._assign_ID_client(module_name, window, title)

class RSMBasic(RSMInfrastructure):
    """partially concrete subclass of RSMInfrastructure, defining the
    basic algorithms for handling recognition-starting 
    (or onBegin/gotBegin) callbacks

    **INSTANCE ATTRIBUTES**

    *STR* universal -- name of a universal instance using global
    grammars for testing purposes, or None if there is none

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, **args):
        """
        see RecogStartMgr

        """
        self.deep_construct(RSMBasic,
                            {'universal': None
                            },
                            args)
        
    def _new_instance_known_window(self, window, title, instance, 
        trust = 0):
        """private method called internally to verify that the named
        instance belongs to the given known window, and if so to add 
        it to the window 

        **INPUTS**

        *INT* window -- the handle of the known window

        *STR* title -- the current title of the window

        *STR* instance -- the name of the instance

        *BOOL* trust -- trust that instance belongs to the window, even
        if we can't verify it definitively, because the user has
        manually specified the window

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
        debug.trace('RecogStartMgr._new_instance_known_window', 
            'known window')
        win = self.windows[window]
        if not win.shared():
# window is known and dedicated, so it cannot belong to the new instance
            return 0
        app = self.app_instance(instance)
        if win.single_display() or app.shared_window():
            verified = win.verify_new_instance(title, instance, self.editors)
            if verified == 1 or (trust and verified == None):
                return self._add_instance_to_window(window, instance)
            return 0
# otherwise, instance says it uses dedicated windows, whereas the window
# is shared, and is not a single-window display, so
# it cannot belong to the new instance
        return 0


    def _new_instance_known_module(self, window, title, instance, module_name,
        trust = 0):
        """private method called internally to verify that the named
        instance belongs to the given unknown window.  If so, a 
        TargetWindow object is created and added to the 
        known windows map.

        **INPUTS**

        *INT* window -- the handle of the known window

        *STR* title -- the current title of the window

        *STR* instance -- the name of the instance

        *STR* module_name -- name of the module 

        *BOOL* trust -- trust that instance belongs to the window, even
        if we can't verify it definitively, because the user has
        manually specified the window

        **OUTPUTS**

        *BOOL* -- true if the instance was added successfully
        """
# unknown window but known module
        debug.trace('RecogStartMgr._new_instance_known_module', 
            'known module')
        module = self.modules[module_name]
        if module.dedicated():
            if module.editor() != self.editors.app_name(instance):
                return 0
        if module.single_display(window, title):
# check if we have a WinIDClient not yet assigned to a window.
            target = self._assign_ID_client(module_name, window, title)
            if target == None:
                return 0
            return self._new_instance_known_window(window, title, 
                instance, trust)

# otherwise, verify
        verified = module.verify_new_instance(window, title, instance, 
            self.editors)
        if verified == 1 or (trust and verified == None):
            target = module.new_window(window, title, self.editors, instance)
            if target != None:
                return self._add_known_window(window, target, instance)
            return 0
        return 0

    def _recognition_starting_known_module(self, window, title, module_name):
        """private method called by _recognition_starting when the
        current window is not a previously known window, but the 
        current module is known.

        **INPUTS**

        *INT* window -- window handle (unique identifier) of the current 
        window

        *STR* title -- title of the window 

        *STR* module_name -- filename of the application corresponding to
        this window.  **Note**: the module may not
        be the name of the editor.  For example, for remote editors, the
        module will generally be the name of the telnet/X server
        program, and any application written in python will show up as PYTHON.

        **OUTPUTS**

        *none*
        """
# unknown window
        debug.trace('RecogStartMgr._recognition_starting_known_module',
            'known module %s' % module_name)
        module = self.modules[module_name]
#        print 'rs known module'
        if module.single_display(window, title):
# check if we have a WinIDClient not yet assigned to a window.
            target = self._assign_ID_client(module_name, window, title)
            if target == None:
                return 
            self._recognition_starting_known_window(window, title)
            return
#        print 'rs known module: not single'
        debug.trace('RecogStartMgr._recognition_starting_known_module',
            'not single')
        for instance in self.known_instances():
#            print 'rs known module: instance ', instance
            debug.trace('RecogStartMgr._recognition_starting_known_module',
                'checking instance "%s"' % instance)
            info = self.instances[instance]
            editor = self.app_instance(instance)
            instance_module = info.module()
            if module.dedicated():
#                print 'rs known module: dedicated'
                debug.trace('RecogStartMgr._recognition_starting_known_module',
                    'module is dedicated')
                app_name = self.editors.app_name(instance)
                if module.editor() != app_name or editor.shared_window():
                    debug.trace('RecogStartMgr._recognition_starting_known_module', 
                        'wrong editor or shared window')
#                    print 'wrong editor or shared window'
                    continue
                debug.trace('RecogStartMgr._recognition_starting_known_module',
                    'right module')
            if instance_module == None or \
               (instance_module == module_name and editor.multiple_windows()):
# if the instance has no module, it must be a fresh instance with no
# windows yet, so the current window might belong to it. Alternatively,
# if the instance's module matches that of the current window, and the
# editor supports multiple windows (and the module is not a
# single-window-display, which we've already ruled out above) then this
# might be a new window for the existing instance.  In either case,
# attempt to verify
#                print 'possible window'
                debug.trace('RecogStartMgr._recognition_starting_known_module',
                    'possible_window')
                verified = module.verify_new_instance(window, title, 
                    instance, self.editors)
                if verified == 1:
#                    print 'verified - creating new window'
                    debug.trace('RecogStartMgr._recognition_starting_known_module',
                        'verified - creating new window')
                    target = module.new_window(window, title, 
                        self.editors, instance)
                    if target != None:
                        if self._add_known_window(window, target, instance):
                            self._recognition_starting_known_window(window, 
                                title)
                    return
#                print 'not verified'
                debug.trace('RecogStartMgr._recognition_starting_known_module',
                    'not verified')
                continue

    def new_universal_instance(self, instance, exclusive = 1):
        """method called by AppMgr to notify RecogStartMgr that a new
        test instance has been added which should use global grammars
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        *BOOL* exclusive -- should the instance use exclusive grammars
        as well?

        **OUTPUTS**

        *BOOL* -- true if the instance was added as a universal instance.
        False if there was already such a universal instance, in which case the
        new instance will be added normally, or if the instance name was
        already known.
        """
        if self.known_instance(instance):
            return 0
        if self.universal == None:
            debug.trace('RSMBasic.new_universal_instance', 'adding instance name: %s' % instance)
            self.instances[instance] = KnownInstance()
            app = self.app_instance(instance)
            debug.trace('RecogStartMgr.new_universal_instance', 
                'new global manager')
            self.grammars[instance] = \
                self.GM_factory.new_global_manager(app, 
                    instance_name = instance, 
                    recog_mgr = self, exclusive = exclusive)
            self.results[instance] = \
                self.res_mgr_factory.new_manager(recog_mgr = self,
                instance_name = instance)
            self.universal = instance
            return 1
        self.new_instance(instance)
        return 0

    def make_universal(self, instance, exclusive = 1):
        """make an existing instance into a universal instance using
        global grammars

        **INPUTS**

        *STR* instance -- the name of the instance

        *BOOL* exclusive -- should the instance use exclusive grammars

        **OUTPUTS**

        *BOOL* -- true if the instance existed and was made into a universal 
        instance.  False if the named isntance didn't exist, or if there was 
        already another universal instance
        """
        if not self.known_instance(instance):
            return 0
        if self.universal != None:
            if self.universal == instance:
# if the instance is already the universal instance, do nothing, but return true
                return 1
            return 0
        app = self.app_instance(instance)
        debug.trace('RecogStartMgr.make_universal', 
            'new global manager')
        self.grammars[instance].cleanup()
        self.grammars[instance] = \
            self.GM_factory.new_global_manager(app, 
                instance_name = instance, 
                recog_mgr = self, exclusive = exclusive)
        self.universal = instance
        return 1

    def delete_instance(self, instance):
        """method called by AppMgr to notify RecogStartMgr that an
        editor instance has been deleted
    
        **INPUTS**

        *STR* instance -- name of the editor instance

        **OUTPUTS**

        *BOOL* -- true if instance was known
        """
        if instance == self.universal:
            self.universal = None
        return RSMInfrastructure.delete_instance(self, instance)

    def _recognition_starting(self, window, title, module_name = None):
        """private method which a concrete subclass will call to handle
        the recognition starting event.

        **INPUTS**

        *INT* window -- window handle (unique identifier) of the current 
        window

        *STR* title -- title of the window 

        *STR* module -- filename of the application corresponding to
        this window, or None if the particular subclass of RecogStartMgr
        cannot detect it.  **Note**: the module may not
        be the name of the editor.  For example, for remote editors, the
        module will generally be the name of the telnet/X server
        program, and any application written in python will show up as PYTHON.

        **OUTPUTS**

        *none*
        """
        if self.universal == None:
            RSMInfrastructure._recognition_starting(self, window, title,
                module_name)
        else:
            app = self.app_instance(self.universal)
            self._activate_universal_grammars(app, self.universal, window)

    def _activate_universal_grammars(self, app, instance_name, window):
        """private method used to activate global grammars for the 
        universal instance, assuming that the buffer is VoiceCode-enabled
        
        **INPUTS**

        *AppState* app -- the editor application interface

        *STR* instance_name -- instance name

        *INT* window -- current window handle (used only to know which
        other grammars to deactivate 

        **OUTPUTS**

        *none*
        """
        try:
            debug.trace('RSMInfrastructure._activate_universal_grammars',
                'about to call recog_begin')
            dictation_allowed = app.recog_begin(None)
            debug.trace('RSMInfrastructure._activate_universal_grammars',
                'recog_begin returned %d' % dictation_allowed)
            app.synchronize_with_app()
            debug.trace('RSMInfrastructure._activate_universal_grammars',
                'synchronized')
            buff_name = app.curr_buffer_name()
            debug.trace('RSMInfrastructure._activate_universal_grammars',
                'buff_name is %s' % buff_name)
            active_field = app.active_field()
            debug.trace('RSMInfrastructure._activate_universal_grammars',
                'active field is %s' % active_field)
            dictation_allowed = dictation_allowed and \
                (active_field == None)
        except messaging.SocketError:
            debug.trace('RSMInfrastructure._activate_universal_grammars',
                'Socket Error during synch')
            self.grammars[instance_name].activate_sink(-1)
            debug.trace('RSMInfrastructure._activate_universal_grammars',
                'activated dictation sink')
        else:
            if dictation_allowed:
                debug.trace('RSMInfrastructure._activate_universal_grammars',
                    'activating dictation grammar')
                self.grammars[instance_name].activate(buff_name, -1, is_in_text_mode = self.is_in_text_mode)
            else:
                msg = "recog_begin returned false unexpectedly during\n"
                msg = msg + "regression tests.  Will abort test\n"
                sys.stderr.write(msg)
                self.grammars[instance_name].activate_sink(-1)
                debug.trace('RSMInfrastructure._activate_universal_grammars',
                    'activated dictation sink')
                self.editors.cancel_testing()
#                debug.trace('RSMInfrastructure._activate_universal_grammars',
#                    'deactivating dictation grammar')
#                self.grammars[instance_name].deactivate_all()
        others = []
        if self.known_window(window):
            others = self.windows[window].instance_names()
        if self.GM_factory.using_global():
            others = self.known_instances()
        for editor in others:
            if editor != instance_name:
                self.grammars[editor].deactivate_all(window)
            
            
    
                
class CurrWindow(Object):
    """abstract class for supplying information about the current window

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(CurrWindow, {}, args)
    
    def window_info(self):
        """find the window id, title, and module of the current window

        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name.  The
        module name should be converted to all lowercase
        """
        debug.virtual('CurrWindow.window_info')

class CurrWindowDummy(Object):
    """dummy implementation of CurrWindow for testing purposes.

    **INSTANCE ATTRIBUTES**

    
    *INT* window -- the window id of the current window

    *STR* module -- the module name of the current window

    *AppState* instance -- the current instance

    *STR* app_name -- application name

    *STR* alt_title -- alternate title if instance can't set window
    title

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, window = None, module = None, instance = None, 
        app_name = None, alt_title = "", **args):
        """Initialize with specified window information

        **INPUTS**

        *INT* window -- the window id of the current window

        *STR* module -- the module name of the current window

        *AppState* instance -- the current instance
        
        *STR* app_name -- application name

        *STR* alt_title -- alternate title if instance can't set window
        title

        **OUTPUTS**

        *none*
        """

        self.deep_construct(CurrWindowDummy, 
                            {'window': window,
                             'module': module,
                             'instance': instance,
                             'app_name': app_name,
                             'alt_title': alt_title
                            }, args)
    
    def window_info(self):
        """find the window id, title, and module of the current window

        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name.  The
        module name should be converted to all lowercase
        """
#        print 'current is ', (self.window, self.title, self.module)
        title = ""
        if self.alt_title != "":
            title = self.alt_title
        if self.instance != None:
            ts = self.instance.instance_string()
            if ts != None:
                if self.app_name != None:
                    title = self.app_name + " - "
                title = title + self.instance.instance_string()
                if self.instance.curr_buffer_name():
                    title = title + " - " + self.instance.curr_buffer_name()

        return (self.window, title, string.lower(self.module))

    def set_info(self, window = None, module = None, instance = None, 
        app_name = None, alt_title = ""):
        """set specified window information

        **INPUTS**

        *INT* window -- the window id of the current window

        *STR* module -- the module name of the current window

        *AppState* instance -- the current instance
        
        *STR* app_name -- application name

        *STR* alt_title -- alternate title if instance can't set window
        title

        **OUTPUTS**

        *none*
        """
        self.window = window
        self.module = module
        self.instance = instance
        self.app_name = app_name
        self.alt_title = alt_title

class RSMExtInfo(RSMBasic):
    """subclass of RSMBasic which uses an external object to
    get window information

    **INSTANCE ATTRIBUTES**

    *CurrWindow* find_info -- CurrWindow object for querying the current
    window

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, win_info, **args):
        """
        **INPUTS**

        *AppMgr* editors -- the editors AppMgr object, which provides
        information about editor application instances

        *GramMgrFactory* GM_factory -- GramMgrFactory to create GramMgr
        objects for new instances
        
        """
        self.deep_construct(RSMExtInfo,
                            {'find_info': win_info
                            },
                            args)
        
    def window_info(self):
        """find the window id, title, and module of the current window

        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name.  The
        module name should be converted to all lowercase
        """
        return self.find_info.window_info()

# defaults for vim - otherwise ignore
# vim:sw=4

