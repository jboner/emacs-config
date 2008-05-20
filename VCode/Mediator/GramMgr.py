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

"""abstract class defining interface for an object managing buffer-specific 
grammars (dictation and selection grammars)
"""
import debug
import string
from Object import Object, OwnerObject
from exceptions import IndexError

class GramMgr(OwnerObject):
    """abstract class defining basic grammar management interface.

    **INSTANCE ATTRIBUTES**

    *AppState* app -- the application to which the buffers belong
    
    *STR instance_name* -- the name of this AppState instance 
    
    *RecogStartMgr recog_mgr* -- the RecogStartMgr which owns this
    grammar manager
    
    *[STR] text_mode_on_spoken_as=None* -- list of spoken forms for
    commands to set text mode on.
    
    *[STR] text_mode_off_spoken_as=None* -- list of spoken forms for
    commands to set text mode off.
    
    *0-1 text_mode_off_sets_nat_text_to=1* -- when text mode is
    turned off, set NatText to that value.
    

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, recog_mgr, app, instance_name, 
                 text_mode_on_spoken_as=None, text_mode_off_spoken_as=None,
                 text_mode_off_sets_nat_text_to=1, **args):
        """constructor
        
        **INPUTS**

        *RecogStartMgr recog_mgr* -- the RecogStartMgr which owns this
        grammar manager
        
        *AppState* app -- the application to which the buffers belong

        *STR instance_name* -- the name of this AppState instance 
        """
        self.deep_construct(GramMgr,
                            {'recog_mgr': recog_mgr, 'app': app,
                             'instance_name': instance_name,
                             'text_mode_on_spoken_as': text_mode_on_spoken_as,
                             'text_mode_off_spoken_as': text_mode_off_spoken_as,
                             'text_mode_off_sets_nat_text_to': text_mode_off_sets_nat_text_to,
                            },
                            args)
        self.name_parent('recog_mgr')


    def set_exclusive(self, exclusive = 1):
        """makes the grammars exclusive (or not).  Generally used only
        for background regression testing

        **INPUTS**

        *BOOL* exclusive -- true if the grammar should be exclusive

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.set_exclusive')

    def name(self):
        """returns the name of the AppState editor instance 

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *STR* -- the instance name
        """
        return self.instance_name

    def user_message(self, message):
        """sends a user message up the chain to the NewMediatorObject to
        be displayed

        **INPUTS**

        *STR message* -- the message

        **OUTPUTS**

        *none*
        """
        if self.recog_mgr:
            self.recog_mgr.user_message(message, 
                instance = self.name())

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the application
        has renamed a buffer

        **INPUTS**

        *STR* instance -- name of the application instance 

        **OUTPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        *none*
        """
        debug.virtual('GramMgr.rename_buffer_cbk')

    def interpreter(self):
        """return a reference to the mediator's current CmdInterp object

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.recog_mgr.interpreter()
    
    def interpret_dictation(self, result, initial_buffer = None):
        """interpret the result of recognition by a dictation grammar,
        and store the relevant information to allow for correction.

        **INPUTS**

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.interpret_dictation')

    def scratch_recent(self, n):
        """undo the effect of the n most recent utterances into 
        this application, if possible

        **INPUTS**

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances successfully undone
        """
        name = self.name()
        debug.trace('GramMgr.scratch_recent', 
            'instance name = %s' % name)
        return self.recog_mgr.scratch_recent(name, n)

    def correct_last(self):
        """initiate user correction of the most recent utterance, if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        name = self.name()
        self.recog_mgr.correct_last(name)

    def correct_recent(self):
        """initiate user correction of one or more recent utterances,
        if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        name = self.name()
        self.recog_mgr.correct_recent(name)

    def reformat_recent(self):
        name = self.name()
        self.recog_mgr.reformat_recent(name)

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
        self.deactivate_all()

    def activate_sink(self, window):
        """activate dummy dictation grammar as a sink to intercept
        an utterance directed to a disconnected editor, and
        deactivate all other buffer/window-specific grammars

        **INPUTS**

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.activate_sink')
    
    def activate(self, buffer, window, is_in_text_mode = 0):
        """activate grammars for a buffer displayed in a particular
        window, and deactivate all other buffer/window-specific grammars

        **INPUTS**

        *STR* buffer -- name of buffer

        *BOOL* is_in_text_mode = 0 -- if TRUE, then activate only the grammars
        for putting VoiceCode in text mode

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.activate')
    
    def deactivate_all(self, window = None):
        """de-activate all buffer-specific grammars which would be
        active in window, or all grammars if window is omitted.

        **INPUTS**

        *INT* window --
        identifier of current window.  If grammars are window-specific,
        then only grammars associated with that window need be
        explicitly de-activated.  If window is omitted, de-activate all
        grammars.
        
        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.deactivate_all')

    def new_buffer(self, buffer, window = None):
        """add grammars for new buffer/window

        **INPUTS**

        *STR* buffer -- name of buffer

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle.

        Note: if grammars are window-specific, and window is omitted, 
        then new_buffer may not be created until activate is called.

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.new_buffer')

    def new_window(self, window):
        """add a new window

        **INPUTS**

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle.

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.new_window')

    def delete_window(self, window):
        """clean up and destroy all grammars for a window which 
        has been deleted.

        **INPUTS**

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle

        **OUTPUTS**

        *none*
        """

        debug.virtual('GramMgr.delete_window')

    def buffer_closed(self, buffer):
        """clean up and destroy all grammars for a buffer which 
        has been closed.

        **INPUTS**

        *STR* buffer -- name of buffer

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgr.buffer_closed')
    
    def using_global(self):
        """checks whether GramMgr creates global grammars, rather than 
        window-specific ones

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the GramMgr produces global grammars
        """
        debug.virtual('GramMgr.using_global')

        
    
class GramMgrFactory(Object):
    """factory which produces GramMgr objects for new application
    instances

    **INSTANCE ATTRIBUTES**

    *[STR] text_mode_on_spoken_as=None* -- list of spoken forms for
    commands to set text mode on.
    
    *[STR] text_mode_off_spoken_as=None* -- list of spoken forms for
    commands to set text mode off.
    
    *0-1 text_mode_off_sets_nat_text_to=1* -- when text mode is
    turned off, set NatText to that value.

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self,**args):
        """abstract class, no arguments
        """
        self.deep_construct(GramMgrFactory, {}, args)

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
        debug.virtual('GramMgrFactory.capitalize_rules')
        
    def config_text_mode_toggling(self, on_spoken_as, off_spoken_as, off_sets_nat_text_to=None):
        """Configure the factory to generate grammars with
        appropriate settings for for toggling text mode on/off.
        
        *[STR] on_spoken_as* -- list of spoken forms for the command that turns
        text mode on.
        
        *[STR] off_spoken_as* -- list of spoken forms for the command that turns
        text mode off.
        
        *BOOL off_sets_nat_text_to=None* -- specifies what the grammar should do
        with NatText when text mode is turned off. If 1 or 0, tell the grammar
        to set NatText to that value. If *None*, then don't change
        the current behaviour of the toggling grammar.        

        """
        self.text_mode_on_spoken_as = on_spoken_as
        self.text_mode_off_spoken_as = off_spoken_as
        self.text_mode_off_sets_nat_text_to = off_sets_nat_text_to        
        

    def new_manager(self, editor, instance_name, recog_mgr):
        """creates a new GramMgr

        **INPUTS**

        *AppState* editor -- AppState object for which to manage
        grammars

        *STR instance_name* -- the name of this AppState instance 

        *RecogStartMgr recog_mgr* -- the RecogStartMgr which owns this
        grammar manager

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgrFactory.new_manager')

    def new_global_manager(self, editor, instance_name, 
        recog_mgr, exclusive = 1):
        """creates a new GramMgr using global grammars (regardless of
        the value of self.global_grammars)

        **INPUTS**

        *AppState* editor -- AppState object for which to manage
        grammars

        *STR instance_name* -- the name of this AppState instance 

        *RecogStartMgr recog_mgr* -- the RecogStartMgr which owns this
        grammar manager

        **OUTPUTS**

        *none*
        """
        debug.virtual('GramMgrFactory.new_global_manager')
        
        
class GramMgrDictContext(GramMgr):
    """implements finding of dictation context

    **INSTANCE ATTRIBUTES**

    *none*
    
    **CLASS ATTRIBUTES**
    
    *none*
    """
    def interpret_dictation(self, result, initial_buffer = None):
        """interpret the result of recognition by a dictation grammar,
        and store the relevant information to allow for correction.

        **INPUTS**

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        debug.trace('GramMgrDictContext.interpret_dictation', 'initial_buffer=%s' % initial_buffer)
        name = self.name()
# buffer == 0 is used for special dictation sinks whose results are
# ignored
        if initial_buffer != 0:
            self.recog_mgr.interpret_dictation(name, result,
                initial_buffer = initial_buffer)

    def find_context(self, buffer):
        """Find context for dictation grammar

        **INPUTS**

        *STR buffer* -- name of the current buffer

        **OUTPUTS**

        (STR, STR) -- (two-word) context before and after the current
        selection
        """
#  find dictation context
        current = self.app.cur_pos(buff_name = buffer)
        selection = self.app.get_selection(buff_name = buffer)
#        print current
        buff = self.app.find_buff(buff_name = buffer)
        start = buff.cur_pos()
        try:
            for count in range(2):
                start = buff.char_search('\S', direction = -1, pos = start)
                start = buff.char_search('\s', direction = -1, pos = start)
            start = buff.char_search('\S', direction = -1, pos = start) + 1
        except IndexError:
            start = 0
        before = self.app.get_text(start, current, buff_name = buffer)
        end = buff.cur_pos()
        try:
            end = buff.char_search('\S', pos = end)
            end = buff.char_search('\s', pos = end)
        except IndexError:
            end = buff.len()
        after = self.app.get_text(current, end, buff_name = buffer)
        debug.trace('GramMgrDictContext.find_context', 
            'before, after = [%s], [%s]' % (before, after))
        return before, after


class WinGramMgr(GramMgrDictContext):
    """implementation of GramMgr using window-specific grammars from
    a WinGramFactory.

    **INSTANCE ATTRIBUTES**

    *{INT : {STR : WinDictGram}}* dict_grammars -- map from window handles to
    map from buffer names to dictation grammars

    *{INT : {STR : DictThroughCmdWinGramNL}}* dict_cmd_grammars -- map from 
    window handles to map from buffer names to dictation through command grammars

    *{INT : SelectWinGram}* sel_grammars -- map from window handles to
    to selection grammars

    *{INT : BasicCorrectWinGram}* correction_grammars -- map from 
    window handles to grammars containing basic correction commands

    *{INT : SymbolReformattingWinGram}* reformatting_grammars -- map from 
    window handles to grammars containing symbol reformatting commands
    
    *{INT: TextModeTogglingGram}* text_mode_toggling_grammars -- map from
    window handles to command grammars for turning text mode on-off.

    *WinGramFactory* factory -- factory which supplies WinGramMgr
    with new window-specific dictation and selection grammars.

    *STR* correction -- string indicating the type of correction
    which is available: 'basic' or 'advanced', or None if no 
    correction is available
    
    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, factory, exclusive = 0, correction = None, global_grammars = 0,
                       text_mode_toggling = None, **args):
        """
        
        **INPUTS**
        
        *WinGramFactory* factory -- factory which will supply WinGramMgr
        with new window-specific dictation and selection grammars.

        *BOOL* global_grammars -- use global grammars, instead of
        window-specific ones (only for testing purposes)

        *BOOL* exclusive -- use exclusive grammars which prevent 
        non-exclusive grammars from getting results (only for testing purposes)

        *STR* correction -- string indicating the type of correction
        which is available: 'basic' or 'advanced', or None if no 
        correction is available
        
        *BOOL text_mode_toggling* -- true IIF toggling of text mode is supported.                    
        """
        debug.trace("GramMgr.WinGramMgr", "exlusive=%s" % exclusive)
        self.deep_construct(WinGramMgr,
                            {
                            'factory': factory, 
                            'global_grammars': global_grammars,
                            'exclusive': exclusive,
                            'dict_grammars' : {},
                            'dict_cmd_grammars': {},
                            'sel_grammars' : {},
                            'correction_grammars' : {},
                            'reformatting_grammars' : {},
                            'text_mode_toggling_grammars': {},
                            'correction': correction, 
                            'text_mode_toggling': text_mode_toggling
                            },
                            args)
        debug.trace("GramMgr.WinGramMgr", "** after deep_construct")                            
        self.add_owned('dict_grammars')
        self.add_owned('dict_cmd_grammars')        
        self.add_owned('sel_grammars')
        self.add_owned('correction_grammars')
        self.add_owned('reformatting_grammars')

    
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
        self.deactivate_all()

    def set_text_mode(self, set_to):
        """Sets text mode on/off. In text mode, dictation utterances are
        typed as regular text instead of being translated to code.

        **INPUTS**

        BOOL *set_to* -- Set text mode on or off depending on this argument.

        **OUTPUTS**

        *none*
        """
        self.recog_mgr.set_text_mode(set_to)
                   
        
    def set_exclusive(self, exclusive = 1):
        """makes the grammars exclusive (or not).  Generally used only
        for background regression testing

        **INPUTS**

        *BOOL* exclusive -- true if the grammar should be exclusive

        **OUTPUTS**

        *none*
        """
        self.exclusive = exclusive
        for buffers in self.dict_grammars.values():
            for grammar in buffers.values():
                grammar.set_exclusive(exclusive)
        for buffers in self.dict_cmd_grammars.values():
            for grammar in buffers.values():
                grammar.set_exclusive(exclusive)                
        for grammar in self.sel_grammars.values():
            grammar.set_exclusive(exclusive)
        for grammar in self.correction_grammars.values():
            grammar.set_exclusive(exclusive)
        for grammar in self.reformatting_grammars.values():
            grammar.set_exclusive(exclusive)
        for grammar in self.text_mode_toggling_grammars.values():
            grammar.set_exclusive(exclusive)


    def activate(self, buffer, window, is_in_text_mode = 0):
        """activate grammars for a buffer displayed in a particular
        window, and deactivate all other buffer/window-specific grammars

        **INPUTS**

        *STR* buffer -- name of buffer

        *BOOL* is_in_text_mode = 0 -- if TRUE, then activate only the grammars
        for putting VoiceCode in text mode         

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle

        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgr.activate', 'buffer=%s' % buffer)
        if not self.dict_grammars.has_key(window):
            self.new_window(window)
# this also creates a new dictation grammar and selection grammar
        if not self.dict_grammars[window].has_key(buffer):
            self.new_buffer(buffer, window)
            
        for buff_name in self.dict_grammars[window].keys():
            if buff_name != buffer:
                debug.trace('WinGramMgr.activate', 'deactivating dictation and dict_cmd grammars for buff_name=%s' % buff_name)
                self.dict_grammars[window][buff_name].deactivate()
                self.dict_cmd_grammars[window][buff_name].deactivate()

# if the dictation grammars are actually global, we need to deactivate 
# all the rest, even if they are stored under other windows in dict_grammars
        if self.global_grammars:
            debug.trace('WinGramMgr.activate', 'Grammars are global. Disabling window-specific dictation grammars.')
            for a_window in self.dict_grammars.keys():
                if a_window != window:
                    for buff_name in self.dict_grammars[a_window].keys():
                        self.dict_grammars[a_window][buff_name].deactivate()
                        self.dict_cmd_grammars[a_window][buff_name].deactivate()


#  set visible range and buffer for selection grammar
        self.sel_grammars[window].activate(buffer)
# if the selection grammars are actually global, we need to deactivate 
# all the rest, even if they are stored under other windows in sel_grammars
        if self.global_grammars:
            debug.trace('WinGramMgr.activate', 'Grammars are global. Disabling window specific selection grammars..')
            for a_window in self.sel_grammars.keys():
                if a_window != window:
                    self.sel_grammars[a_window].deactivate()

        if self.correction:
            if is_in_text_mode:
                debug.trace('WinGramMgr.activate', 'Grammars are global. Disabling window specific selection grammars..')
                self.correction_grammars[window].deactivate()
            else:
                self.correction_grammars[window].activate()
                
        if is_in_text_mode:
            self.reformatting_grammars[window].deactivate()
        else:
            self.reformatting_grammars[window].activate()

        if self.text_mode_toggling:
            self.text_mode_toggling_grammars[window].activate()            

# if the grammars are actually global, we need to deactivate 
# all the rest, even if they are stored under other windows
        if self.global_grammars:
            if self.correction:
                for a_window in self.correction_grammars.keys():
                    if a_window != window:
                        self.correction_grammars[a_window].deactivate()

            for a_window in self.reformatting_grammars.keys():
                if a_window != window:
                    self.reformatting_grammars[a_window].deactivate()
                        
            
            if self.text_mode_toggling:
                for a_window in self.text_mode_toggling_grammars.keys():
                    if a_window != window:
                        self.text_mode_toggling_grammars[a_window].deactivate()
                        

        if is_in_text_mode:
            self.dict_grammars[window][buffer].deactivate()
            self.dict_cmd_grammars[window][buffer].deactivate()
        else:
#  set dictation context
            before, after = self.find_context(buffer)
            self.dict_grammars[window][buffer].set_context(before, after)
            self.dict_grammars[window][buffer].activate()
            
            debug.trace('WinGramMgr.activate', 'activating self.dict_cmd_grammars[window][buffer]=%s:' % self.dict_cmd_grammars[window][buffer])
            self.dict_cmd_grammars[window][buffer].activate()            
    
    def activate_sink(self, window):
        """activate dummy dictation grammar as a sink to intercept
        an utterance directed to a disconnected editor, and
        deactivate all other buffer/window-specific grammars

        **INPUTS**

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle

        **OUTPUTS**

        *none*
        """
        self.deactivate_all()
        a_window = window
        if self.global_grammars:
            a_window = None
# use an initial_buffer "name" of 0, so that when we get results back we know to
# ignore them, rather than passing them on 
        self.dict_grammars[window][0] = \
            self.factory.make_dictation(self, self.app, 0, 
                window = a_window, exclusive = self.exclusive)
        self.dict_grammars[window][0].activate()
        self.dict_cmd_grammars[window][0] = \
            self.factory.make_dictation_through_cmd(self, self.app, 0, 
                window = a_window, exclusive = self.exclusive)
        self.dict_cmd_grammars[window][0].activate()


    def _deactivate_all_window(self, window):
        """de-activate all buffer-specific grammars which would be
        active in window

        **INPUTS**

        *INT* window --
        identifier of current window.  Only grammars associated with 
        that window will be explicitly de-activated.  
        
        **OUTPUTS**

        *none*
        """
        debug.trace('GramMgr._deactivate_all_window', 'invoked')
        if self.dict_grammars.has_key(window):
            self.sel_grammars[window].deactivate()
            if self.correction:
                self.correction_grammars[window].deactivate()
            self.reformatting_grammars[window].deactivate()
            if self.text_mode_toggling:
                self.text_mode_toggling_grammars[window].deactivate()
            buffers = self.dict_grammars[window].keys()
            buffers.sort()
            for a_buffer in buffers:
                self.dict_grammars[window][a_buffer].deactivate()
                self.dict_cmd_grammars[window][a_buffer].deactivate()                

    def deactivate_all(self, window = None):
        """de-activate all buffer-specific grammars which would be
        active in window, or all grammars if window is omitted.

        **INPUTS**

        *INT* window --
        identifier of current window.  If grammars are window-specific,
        then only grammars associated with that window need be
        explicitly de-activated.  If window is omitted, de-activate all
        grammars.
        
        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgr.deactivate_all', 'window=%s' % window)
        if window == None or self.global_grammars:
            windows = self.dict_grammars.keys()
            windows.sort()
            for a_window in windows:
                self._deactivate_all_window(a_window)
        else:
            self._deactivate_all_window(window)

    def new_buffer(self, buffer, window = None):
        """add grammars for new buffer/window

        **INPUTS**

        *STR* buffer -- name of buffer

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle.

        Note: if window is omitted, 
        then new_buffer may not be created until activate is called.

        **OUTPUTS**

        *none*
        """
        if window != None:
            if not self.dict_grammars.has_key(window):
                self.new_window(window, buffer)
            if not self.dict_grammars[window].has_key(buffer):
                a_window = window
                if self.global_grammars:
                    a_window = None
                debug.trace('WinGramMgr.new_buffer', 
                    'window, a_window: %s, %s' % (str(window), str(a_window)))
                self.dict_grammars[window][buffer] = \
                    self.factory.make_dictation(self, self.app, 
                    buffer, window = a_window, 
                    exclusive = self.exclusive)
                self.dict_cmd_grammars[window][buffer] = \
                    self.factory.make_dictation_through_cmd(self, self.app, 
                    buffer, window = a_window, 
                    exclusive = self.exclusive)
                    
    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the application
        has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        if old_buff_name == new_buff_name:
            return

        for window, buffer_grammars in self.dict_grammars.items():
            try:
                grammar = buffer_grammars['old_buff_name']
                grammar.rename_buffer_cbk(new_buff_name)
                buffer_grammars['new_buff_name'] = grammar
                del buffer_grammars['old_buff_name']
            except KeyError:
                pass

        for window, buffer_grammars in self.dict_as_cmd_grammars.items():
            try:
                grammar = buffer_grammars['old_buff_name']
                grammar.rename_buffer_cbk(new_buff_name)
                buffer_grammars['new_buff_name'] = grammar
                del buffer_grammars['old_buff_name']
            except KeyError:
                pass

    def new_window(self, window, buffer = None):
        """add a new window

        **INPUTS**

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle.

        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgr.new_window', 'window=%s' % window)
        if not self.dict_grammars.has_key(window):
            self.dict_grammars[window] = {}
            self.dict_cmd_grammars[window] = {}            
        if not self.sel_grammars.has_key(window):
            a_window = window
            if self.global_grammars:
                a_window = None
            debug.trace('WinGramMgr.new_window', 
                'window, a_window: %s, %s' % (str(window), str(a_window)))
            self.sel_grammars[window] = \
                self.factory.make_selection(manager = self, app = self.app, 
                window = a_window, exclusive = self.exclusive)
        if self.correction and not self.correction_grammars.has_key(window):
            a_window = window
            if self.global_grammars:
                a_window = None
            self.correction_grammars[window] = \
                self.factory.make_correction(manager = self, window = a_window, 
                exclusive = self.exclusive)
#            print self.correction_grammars[window]

        if not self.reformatting_grammars.has_key(window):
            a_window = window
            if self.global_grammars:
                a_window = None
            self.reformatting_grammars[window] = \
                self.factory.make_reformatting(manager = self, window = a_window, 
                exclusive = self.exclusive)
#            print self.reformatting_grammars[window]


        if self.text_mode_toggling and not self.text_mode_toggling_grammars.has_key(window):
            a_window = window
            if self.global_grammars:
                a_window = None
#NEW                
            self.text_mode_toggling_grammars[window] = \
                self.factory.make_text_mode(manager = self, window = a_window, 
                                            exclusive=self.exclusive,
                                            on_spoken_as=self.text_mode_on_spoken_as,
                                            off_spoken_as=self.text_mode_off_spoken_as,
                                            off_sets_nat_text_to=self.text_mode_off_sets_nat_text_to)
#ORIG
#            self.text_mode_toggling_grammars[window] = \
#                self.factory.make_text_mode(manager = self, window = a_window, 
#                                            exclusive=self.exclusive)
                                            
    

    def delete_window(self, window):
        """clean up and destroy all grammars for a window which 
        has been deleted.

        **INPUTS**

        *INT* window -- 
        number identifying the current window  displaying
        the buffer.  In Microsoft Windows, this will be the window
        handle

        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgr.delete_window', 'window=%s' % window)
        if self.sel_grammars.has_key(window):
            self._deactivate_all_window(window)
            del self.sel_grammars[window]
            if self.text_mode_toggling:
                del self.text_mode_toggling_grammars[window]            
            if self.correction:
                del self.correction_grammars[window]
            del self.reformatting_grammars[window]
                
        if self.dict_grammars.has_key(window):
            for a_buffer in self.dict_grammars[window].keys():
                self.dict_grammars[window][a_buffer].cleanup()
                self.dict_cmd_grammars[window][a_buffer].cleanup()
                del self.dict_grammars[window][a_buffer]                
                del self.dict_cmd_grammars[window][a_buffer]
                
            del self.dict_grammars[window]
            del self.dict_cmd_grammars[window]
            

    def buffer_closed(self, buffer):
        """clean up and destroy all grammars for a buffer which 
        has been closed.

        **INPUTS**

        *STR* buffer -- name of buffer

        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgr.buffer_closed', 'buffer=%s' % buffer)
        for a_window in self.dict_grammars.keys():
            buffers = self.dict_grammars[a_window]
            if self.dict_grammars[a_window].has_key(buffer):
                self.dict_grammars[a_window][buffer].cleanup()
                del self.dict_grammars[a_window][buffer]
                self.dict_cmd_grammars[a_window][buffer].cleanup()
                del self.dict_cmd_grammars[a_window][buffer]


    def using_global(self):
        """checks whether GramMgr creates global grammars, rather than 
        window-specific ones

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the global_grammars flag has been set to
        produce global grammars
        """
        return self.global_grammars

class WinGramMgrFactory(GramMgrFactory):
    """implements GramMgrFactory for WinGramMgr

    **INSTANCE ATTRIBUTES**

    *WinGramFactory* gram_factory -- factory which will supply each
    WinGramMgr with new window-specific dictation and selection grammars.

    *BOOL* global_grammars -- use global grammars, instead of
    window-specific ones (only for testing purposes)

    *BOOL* exclusive -- use exclusive grammars which prevent 
    non-exclusive grammars from getting results (only for testing purposes)

    *STR* correction -- string indicating the type of correction
    which is available: 'basic' or 'advanced', or None if no 
    correction is available

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, gram_factory, global_grammars = 0,
        exclusive = 0, correction = None, text_mode_toggling=None, **args):
        """create a GramMgrFactory which creates WinGramMgr objects for
        new editors

        **INPUTS**

        *WinGramFactory* gram_factory -- factory which will supply each
        WinGramMgr with new window-specific dictation and selection grammars.

        *BOOL* global_grammars -- use global grammars, instead of
        window-specific ones (only for testing purposes)

        *BOOL* exclusive -- use exclusive grammars which prevent 
        non-exclusive grammars from getting results (only for testing purposes)

        *STR* correction -- string indicating the type of correction
        which is available: 'basic' or 'advanced', or None if no 
        correction is available
        
        *BOOL text_mode_toggling* -- true IIF toggling of text mode is supported.
        
        """
        self.deep_construct(WinGramMgrFactory, 
                            {'gram_factory': gram_factory,
                             'global_grammars': global_grammars,
                             'exclusive': exclusive,
                             'correction': correction,
                             'text_mode_toggling': text_mode_toggling,
                             'text_mode_on_spoken_as': None,
                             'text_mode_off_spoken_as': None,
                             'text_mode_off_sets_nat_text_to': None,
                            }, args)

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
        self.gram_factory.capitalize_rules(capitalize)

    def using_global(self):
        """checks whether the GramMgr objects created by the factory use
        global grammars, rather than window-specific ones

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the factory has been set to produce GramMgr
        objects with the global_grammars flag set
        """
        return self.global_grammars

    def new_manager_ORIG(self, editor, instance_name, recog_mgr):
        """creates a new GramMgr

        **INPUTS**

        *AppState* editor -- AppState object for which to manage
        grammars

        *STR instance_name* -- the name of this AppState instance 
    
        *RecogStartMgr recog_mgr* -- the RecogStartMgr which owns this
        grammar manager

        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgrFactory.new_manager', 
            'new manager: global = ' + str(self.global_grammars))
        return WinGramMgr(app = editor, instance_name = instance_name,
            recog_mgr = recog_mgr,
            factory = self.gram_factory,
            global_grammars = self.global_grammars, exclusive = self.exclusive,
            correction = self.correction, text_mode_toggling = self.text_mode_toggling)

#NEW
    def new_manager(self, editor, instance_name, recog_mgr):
        """creates a new GramMgr

        **INPUTS**

        *AppState* editor -- AppState object for which to manage
        grammars

        *STR instance_name* -- the name of this AppState instance 
    
        *RecogStartMgr recog_mgr* -- the RecogStartMgr which owns this
        grammar manager

        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgrFactory.new_manager', 
            'new manager: global = ' + str(self.global_grammars))
        return WinGramMgr(app = editor, instance_name = instance_name,
            recog_mgr = recog_mgr,
            factory = self.gram_factory,
            global_grammars = self.global_grammars, exclusive = self.exclusive,
            correction = self.correction, text_mode_toggling = self.text_mode_toggling,
            text_mode_on_spoken_as=self.text_mode_on_spoken_as, 
            text_mode_off_spoken_as=self.text_mode_off_spoken_as, 
            text_mode_off_sets_nat_text_to=self.text_mode_off_sets_nat_text_to)
            
            
    def new_global_manager(self, editor, instance_name, recog_mgr, 
        exclusive = 1):
        """creates a new GramMgr using global grammars (regardless of
        the value of self.global_grammars)

        **INPUTS**

        *AppState* editor -- AppState object for which to manage
        grammars

        *STR instance_name* -- the name of this AppState instance 
    
        *RecogStartMgr recog_mgr* -- the RecogStartMgr which owns this
        grammar manager

        **OUTPUTS**

        *none*
        """
        debug.trace('WinGramMgrFactory.new_global_manager', 
            'new global manager')
        return WinGramMgr(app = editor, instance_name = instance_name,
            recog_mgr = recog_mgr,
            factory = self.gram_factory,
            global_grammars = 1, exclusive = exclusive,
            correction = self.correction, text_mode_toggling = self.text_mode_toggling,
            text_mode_on_spoken_as=self.text_mode_on_spoken_as, 
            text_mode_off_spoken_as=self.text_mode_off_spoken_as, 
            text_mode_off_sets_nat_text_to=self.text_mode_off_sets_nat_text_to)
            

# defaults for vim - otherwise ignore
# vim:sw=4

