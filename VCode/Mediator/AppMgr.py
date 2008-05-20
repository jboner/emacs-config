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

"""class which owns and manages AppState objects corresponding to
editor instances
"""

import debug
import string
from Object import Object, OwnerObject
import RecogStartMgr
import TargetWindow, WinIDClient
import AppState

class InstanceInfo(Object):
    """class for storing information about editor application instances

    **INSTANCE ATTRIBUTES**

    *STR* instance_name -- unique name assigned by AppMgr

    *STR* application_name -- name of the corresponding editor
    application
    """
    def __init__(self, name, application_name, **args):
        self.deep_construct(InstanceInfo, 
                           {'instance_name': name,
                           'application_name': application_name
                           },
                           args)
        
    def name(self):
        """return name of instance

        **INPUTS**

        *none*

        **OUTPUTS**

        **STR** -- the instance name
        """
        return self.instance_name

    def application(self):
        """return name of application

        **INPUTS**

        *none*

        **OUTPUTS**

        **STR** -- the application name
        """
        return self.application_name

class AppMgr(OwnerObject, AppState.AppCbkHandler):
    """class defining basic interface for keeping track of 
    target applications and windows

    **INSTANCE ATTRIBUTES**

    *NewMediatorObject* mediator -- reference to the mediator which owns
    this AppMgr

    *RecogStartMgr* recog_mgr -- reference to a RecogStartMgr object

    *{STR : [STR]}* instance_names -- map from editor application name 
    to currently managed instance names

    *{STR : INT}* past_instances -- map from editor application name 
    to number of current and past instances

    *{STR : STR}* title_prefixes -- map from editor application name 
    to title prefix for that application.  A title prefix is a unique 
    string for each application, used as the prefix of the title 
    string (which is in turn included as a substring of the window 
    title, if the editor can do so)

    *{INT : [STR]}* windows -- map from currently known windows to
    lists of associated instance names, sorted in the order of instances
    most recently known to be active

    *{STR : InstanceInfo}* instance_data  -- map from instance names to
    corresponding instance-specific data

    *{STR : AppState}* instances -- map from instance names to
    corresponding AppState interfaces

    **CLASS ATTRIBUTES**

    *[STR]* unknown_app_prefixes -- list of title prefixes to use with
    editors without predefined title_prefixes
    
    """
    unknown_app_prefixes = ['Arthur', 'Bryan', 'Charlie', 'David', 'Eric',
        'Franklin', 'Gordon', 'Harry', 'Isaac', 'Joshua', 'Kelly', 'Larry',
        'Michael', 'Neville', 'Oscar', 'Peter', 'Roger', 'Steven', 'Thomas',
        'Walther']
    unknown_app_prefixes.reverse()

    def __init__(self, recog_mgr, mediator = None, **args):
        """
        **INPUTS**

        *RecogStartMgr* recog_mgr -- reference to a RecogStartMgr object
        
        *NewMediatorObject* mediator -- reference to the mediator which owns
        this AppMgr, or None if this AppMgr is used only for testing
        purposes and has no mediator to notify of callbacks from the
        applications
        """
        self.deep_construct(AppMgr,
                            {
                             'mediator': mediator,
                             'instance_names': {}, 
                             'instances': {}, 
                             'instance_data': {},
                             'title_prefixes': {},
                             'recog_mgr': recog_mgr,
                             'past_instances' : {}
                            },
                            args)
        self.name_parent('mediator')
        self.add_owned_list(['recog_mgr', 'instances'])
        self.recog_mgr.set_app_mgr(self)
        self.recog_mgr.activate()

    def remove_other_references(self):
        self.recog_mgr.deactivate()

# need to do this before cleanup because it is the only way to make sure
# that the server removes its references to these instances
        for instance in self.instances.keys():
            self.mediator.delete_editor_cbk(self.app_name(instance), 
                instance, unexpected = 0)

        OwnerObject.remove_other_references(self)

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
        self.recog_mgr.capitalize_rules(capitalize)

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
        self.recog_mgr.set_exclusive(exclusive, instance)

    def cancel_testing(self):
        """cancel a regression test if one is running, and return to the
        main message loop.  If no regression test is running, this
        method will have no effect.

        Note: This is an experimental method which may not work.  Even
        if it does, its effect is asynchronous, and there may be a
        substantial delay before the test ceases.  

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.mediator.cancel_testing()

    def app_instances(self, app_name = None):
        """names of application instances being managed
        **INPUTS**
        
        *STR* app_name -- list names of instances corresponding only to
        this application name (or list all instances if app_name is
        None)

        **OUTPUTS**
        
        *[STR]* -- list of names of applications being managed (e.g.
        "Emacs (Win)", "jEdit")
        """
        if app_name == None:
            all_instances = []
            for application in self.app_names():
                all_instances.extend(self.app_instances(application))
            return all_instances
        if app_name not in self.app_names():
            return []
        return self.instance_names[app_name]

    def interpreter(self):
        """return a reference to the mediator's current CmdInterp object

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.mediator.interpreter()
    
    def console(self):
        """returns a reference to the MediatorConsole which provides the
        GUI correction interfaces.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.mediator.console()
         
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
        self.mediator.user_message(message, instance = instance)

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
        self.recog_mgr.reset_results_mgr(instance_name = instance_name)
    
    def stored_utterances(self, instance_name):
        """queries the ResMgr to see how many dictated utterances have 
        been stored for the specified editor

        **INPUTS**

        *STR instance_name* -- the editor 

        **OUTPUTS**

        *INT* -- number of utterances which can be retrieved with
        recent_dictation
        """
        return self.recog_mgr.stored_utterances(instance_name)

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
        return self.recog_mgr.recent_dictation(instance_name, n = n)
        
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
        return self.recog_mgr.recent_symbols(instance_name, n = n)

    def scratch_recent(self, instance_name, n = 1):
        """undo the effect of the most recent n utterances into the
        specified editor, if possible.

        **INPUTS**

        *STR instance_name* -- the editor 

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances actually undone
        """
        debug.trace('AppMgr.scratch_recent', 'instance_name=%s, n=%s' % (instance_name, n))
        return self.recog_mgr.scratch_recent(instance_name, n = n)

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
        return self.recog_mgr.reinterpret_recent(instance_name, 
                                                 changed, delete_tentative_syms)
   
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
        return self.recog_mgr.can_reinterpret(instance_name, n = n)
   
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
        if not self.known_instance(instance_name):
            return
        self.recog_mgr.correct_utterance(instance_name, utterance_number)

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
        if not self.known_instance(instance_name):
            return
        self.recog_mgr.correct_recent_synchronous(instance_name)

    def reformat_recent(self, instance_name):
        """initiate user selection of a recent symbol to reforma

        NOTE: this is a synchronous method which starts a modal
        correction box, and will not return until the user has 
        dismissed the reformat recent dialog box.  Generally, it should 
        be called only in response to a ReformatRecent event, rather than
        in direct response to a spoken correction command.

        **INPUTS**

        *STR instance_name* -- name of the application instance

        **OUTPUTS**

        *none*
        """
        debug.trace('AppMgr.reformat_recent', '** instance_name=%s' % instance_name)
        if not self.known_instance(instance_name):
            debug.trace('AppMgr.reformat_recent', "** not known instance")
            return
        self.recog_mgr.reformat_recent_synchronous(instance_name)



    def add_prefix(self, app_name, title_prefix):
        """add a title prefix for an editor application

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

        if app_name in self.title_prefixes.keys():
            return 0
        if title_prefix in self.title_prefixes.values():
            return 0
        if title_prefix in self.unknown_app_prefixes:
            return 0
        self.title_prefixes[app_name] = title_prefix
        
    def app_names(self):
        """names of applications being managed
        **INPUTS**
        
        *none* 

        **OUTPUTS**
        
        *[STR]* -- list of names of applications being managed (e.g.
        "Emacs (Win)", "jEdit")
        """
        return self.instance_names.keys()

    def app_name(self, instance):
        """name of application corresponding to an instance

        **INPUTS**
        
        *STR* instance -- name of the application instance

        **OUTPUTS**
        
        *STR* -- name of corresponding application, or None if the
        instance is unknown
        """
        if not self.instance_data.has_key(instance):
            return None
        return self.instance_data[instance].application()

    def add_module(self, module):
        """add a new KnownTargetModule object to the RecogStartMgr

        **INPUTS**

        *KnownTargetModule* module -- the new module

        **OUTPUTS**

        *BOOL* -- true unless a module of the same name already exists
        """
        self.recog_mgr.add_module(module)

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
        self.recog_mgr.trust_current(trust)
        
    def known_window(self, window):
        """is window a known window ID?

        **INPUTS**
    
        *INT* window -- window handle of the window

        **OUTPUTS**
        
        *BOOL* -- true if window is a known window associated with one or more
        editor instances
        """
        return self.recog_mgr.known_window(window)

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
        return self.recog_mgr.known_windows(instance)

    def window_info(self):
        """find the window id, title, and module of the current window

        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, STR, STR)* -- the window id, title, and module name
        """
        return self.recog_mgr.window_info()

    def _add_new_instance(self, app):
        """private method called internally to do the work of
        new_instance, except for notifying the recog_mgr.

        **INPUTS**

        *AppState* app --  AppState interface corresponding to the new
        instance

        **OUTPUTS**

        *STR* -- name of the application instance.  Necessary
        if you want to add windows to the application in the future.
        """
        app_name = app.app_name
        if app_name not in self.app_names():
            self.instance_names[app_name] = []
            self.past_instances[app_name] = 0
            if app_name not in self.title_prefixes.keys():
                self.title_prefixes[app_name] = self.unknown_app_prefixes.pop()
        n = self.past_instances[app_name]
        title_prefix = self.title_prefixes[app_name]
        new_name = app_name + "(%d)" % (n)
        self.past_instances[app_name] = n + 1
        debug.trace('AppMgr._add_new_instance', 'adding instance name: %s' % new_name)
        self.instances[new_name] = app
        app.set_manager(self)
        app.set_name(new_name)
        self.instance_names[app_name].append(new_name)
        self.instance_data[new_name] = InstanceInfo(new_name, app_name)
        app.set_instance_string("(%s %d)" % (title_prefix, n))
#        print 'new instance ' + new_name + ' app = '
#        print repr(app)
        return new_name

    def new_instance(self, app, check_window = 1,
            window_info = None):
        """add a new application instance

        **INPUTS**

        *AppState* app --  AppState interface corresponding to the new
        instance

        *BOOL* check_window -- should we check to see if the
        current window belongs to this instance?

        *(INT, STR, STR) window_info*  -- window id, title, and module of 
        the current window as detected by the TCP server when it
        originally processed the new editor connection, or None to let
        RSM.new_instance check now.  Ignored unless check_window is
        true.

        **OUTPUTS**

        *STR* -- name of the application instance.  Necessary
        if you want to add windows to the application in the future.
        """
        new_name = self._add_new_instance(app)
        self.recog_mgr.new_instance(new_name, check_window, window_info)
#        print repr(self.app_instance(new_name))
        return new_name

    def new_universal_instance(self, app,
        exclusive = 1):
        """add a new application instance

        **INPUTS**

        *AppState* app --  AppState interface corresponding to the new
        instance

        *BOOL* exclusive -- use exclusive grammars?

        **OUTPUTS**

        *STR* -- name of the application instance.  Necessary
        if you want to add windows to the application in the future.
        """
        new_name = self._add_new_instance(app)
        if self.recog_mgr.new_universal_instance(new_name, exclusive):
            return new_name
        else:
            self.delete_instance(new_name)
            return None

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
        return self.recog_mgr.make_universal(instance, exclusive)

    def delete_instance(self, instance):
        """called by NewMediatorObject to remove named instance 
        from management.  (because the call comes from the mediator, we
        don't need to send a delete_editor_cbk to the mediator)

        **INPUTS**

        *STR* instance -- name of the application instance to be removed
    
        **OUTPUTS**

        *none*
        """
        if self.instances.has_key(instance):
            app_name = self.instance_data[instance].application()
            self.instance_names[app_name].remove(instance)
            self.recog_mgr.delete_instance(instance)
            del self.instance_data[instance]
            self.instances[instance].cleanup()
            debug.trace('AppMgr.delete_instance', 'deleting instance name: %s' % instance)
            del self.instances[instance]

    def close_app_cbk(self, instance, unexpected = 0):
        """callback from AppState which indicates that the application has 
        closed or disconnected from the mediator

        **INPUTS**

        *STR* instance -- name of the application instance to be removed
    
        **OUTPUTS**

        *none*
        """
        if self.known_instance(instance):
            if self.mediator:
                self.mediator.delete_editor_cbk(self.app_name(instance), 
                    instance, unexpected = unexpected)
            self.delete_instance(instance)

    def close_buffer_cbk(self, instance, buff_name):
        """callback from AppState which notifies us that the application
        has closed a buffer

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the buffer which was closed

        **OUTPUTS**

        *none*
        """
        # For now, all buffer-specific information is stored under 
        # AppMgr/RecogStartMgr, so we don't need to notify NewMediatorObject
        self.recog_mgr.close_buffer_cbk(instance, buff_name)

    def open_buffer_cbk(self, instance, buff_name):
        """callback from AppState which notifies us that the application
        has opened a new buffer 

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the buffer which was opened

        **OUTPUTS**

        *none*
        """
        # this should call NewMediatorObject, unless all buffer-specific
        # information is stored under AppMgr.  Since I haven't decided
        # yet where that information will be stored, do nothing for now
        pass

    def curr_buff_name_cbk(self, instance, buff_name):
        """callback from AppState which notifies us that the current
        buffer has changed

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* buff_name -- name of the newly current buffer 

        **OUTPUTS**

        *none*
        """
# I don't think we ever need to do anything with this call (it is only
# included in AppState for completeness of ClientEditor)
        pass


    def rename_buffer_cbk(self, instance, old_buff_name, new_buff_name):
        """callback from AppState which notifies us that the application
        has renamed a buffer

        **INPUTS**

        *STR* instance -- name of the application instance 

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        # For now, all buffer-specific information is stored under 
        # AppMgr/RecogStartMgr, so we don't need to notify NewMediatorObject
        self.recog_mgr.rename_buffer_cbk(instance, old_buff_name, new_buff_name)

    def new_window(self, instance):
        """called when the editor notifies us of a new window for the 
        specified instance

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *BOOL* -- true if window is added
        """
        if self.known_instance(instance): 
#            print 'am new window'
#            print instance, self.app_instance(instance)
            return self.recog_mgr.app_new_window(instance)
        return 0

    def suspend_cbk(self, instance):
        """called when the editor notifies us that its process is about
        to be suspended

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *none*
        """
# AppStateMessaging keeps track of the state of the editor -- AppState
# calls this callback only for the benefit of client editor
        pass

    def resume_cbk(self, instance):
        """called when the editor notifies us that its process has 
        resumed after having been suspended 

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *none*
        """
# AppStateMessaging keeps track of the state of the editor -- AppState
# calls this callback only for the benefit of client editor
        pass
    
    def specify_window(self, instance):
        """called to indicate that user has manually identified a
        known instance with the current window 

        **INPUTS**

        *STR* instance -- name of the application instance

        **OUTPUTS**

        *BOOL* -- true if window is added
        """
# we still want to check for consistency
#        print 'app specify'
        if not self.known_instance(instance):
#            print 'app specify unknown instance'
            return 0
        return self.recog_mgr.specify_window(instance)

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
        if self.known_instance(instance): 
            return self.recog_mgr.delete_window(instance, window)
        return 0

    def known_instance(self, instance):
        """checks whether a specific instance name is known

        **INPUTS**

        *STR* instance -- name of the instance

        **OUTPUTS**

        *BOOL* -- true if an instance by that name is being managed
        """
        return self.instances.has_key(instance)

    def window_instances(self, window):
        """returns the list of known instances corresponding to a given
        window handle, in the order of most recent activation

        **INPUTS**

        *INT* window -- the window handle 

        **OUTPUTS**

        *[STR]* -- list of names of instances associated with the
        window, or None if the window is unknown
        """
        return self.recog_mgr.window_instances(window)
 
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
        if not self.instances.has_key(instance):
            return None
        return self.instances[instance]

    def instance_module(self, instance):
        """returns the module associated with the given instance

        **INPUTS**

        *STR* instance -- the name of the instance

        **OUTPUTS**

        *STR* -- the name of the module associated with the instance, or
        None if it is unknown (because the instance has not yet been
        associated with any windows)
        """
        if self.known_instance(instance): 
            return self.recog_mgr.instance_module(instance)
        return None
     
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
# WinIDClient support is not yet implemented
        return None


    def config_text_mode_toggling(self, on_spoken_as, off_spoken_as, off_sets_nat_text_to):
        self.recog_mgr.config_text_mode_toggling(on_spoken_as, off_spoken_as, off_sets_nat_text_to)

# defaults for vim - otherwise ignore
# vim:sw=4
