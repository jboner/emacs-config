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

"""classes for managing results of the dictation grammar 
"""

from Object import Object, OwnerObject
import debug
import re
import string
import threading
import exceptions
from SymbolResult import SymbolResult

import CmdInterp, AppState
from SpokenUtterance import *


class ResMgr(OwnerObject):
    """abstract class defining interface for an object which manages
    results of the dictation grammar for a particular editor instance.

    **INSTANCE ATTRIBUTES**

    *RecogStartMgr* recog_mgr -- the parent RecogStartMgr object, 
    which provides information about editor application instances, as
    well as access to the CmdInterp interpreter object

    *STR* name -- the name of the editor instance

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, recog_mgr, instance_name, **args):
        """
        **INPUTS**

        *RecogStartMgr* re
        cog_mgr -- the parent RecogStartMgr object, 
        which provides information about editor application instances, as
        well as access to the CmdInterp interpreter object

        *STR* instance_name -- the name of the editor instance
        """

        self.deep_construct(ResMgr,
                            {'recog_mgr': recog_mgr,
                             'name': instance_name
                            },
                            args)
        self.name_parent('recog_mgr')
        
    def interpreter(self):
        """return a reference to the mediator's current CmdInterp object

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.recog_mgr.interpreter()

    def console(self):
        """returns a reference to the MediatorConsole which provides the
        GUI correction interfaces.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.recog_mgr.console()
    
    def editor(self):
        """return a reference to the editor corresponding to this ResMgr

        **INPUTS**

        *none*

        **OUTPUTS**

        *AppState* -- the AppState interface to the editor responding
        to this ResMgr
        """
        return self.recog_mgr.app_instance(self.name)

    def interpret_dictation(self, result, initial_buffer = None,
        utterance_number = None):
        """interpret the result of recognition by a dictation grammar,
        and store the relevant information to allow for correction.

        **INPUTS**

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        *INT utterance_number* -- number previously assigned to the
        utterance, if we are re-interpreting an earlier utterance, or
        None if result is from a new utterance and should be assigned 
        a new number

        **OUTPUTS**

        *none*
        """
        debug.virtual('ResMgr.interpret_dictation')

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the given editor
        instance has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        debug.virtual('ResMgr.rename_buffer_cbk')

    def close_buffer_cbk(self, buff_name):
        """callback which notifies us that a buffer was closed

        **INPUTS**

        *STR* buff_name -- name of the buffer which was closed

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        debug.virtual('ResMgr.close_buffer_cbk')

    def stored_utterances(self):
        """tells how many dictated utterances have been stored

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- number of utterances which can be retrieved with
        recent_dictation
        """
        debug.virtual('ResMgr.stored_utterances')

    def recent_dictation(self, n = None):
        """returns a list of SpokenUtterance objects

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

        *INT n* -- the number of utterances to return, or None to return 
        all available utterances.

        **OUTPUTS**

        *[(SpokenUtterance, INT, BOOL, UtteranceInterpretation)]* -- the n most recent dictation 
        utterances (or all available if < n), sorted most recent last, 
        each with:
          - a corresponding identifying number 
          - a flag indicating if the utterance can be undone and re-interpreted
            (or None if no utterances are stored)
          - results of the interpretation of that utterance

        The utterance number is unique, within a given editor instance.

        Note:  These utterances should not be stored permanently, nor
        should they be modified except as part of the correction
        process.  Also, the status of whether a given utterance can be
        re-interpreted may change if the user makes other changes to the 
        buffer during correction.
        """
        debug.virtual('ResMgr.recent_dictation')
        
    def recent_symbols(self, n=None):
        """returns a list of the most recently uttered symbols.

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

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
        debug.virtual('ResMgr.recent_symbols')

    def correct_last(self):
        """initiate user correction of the most recent dictation utterance 
        into the given editor, if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('ResMgr.correct_last')

    def correct_recent(self):
        """initiate user correction of one or more recent dictation 
        utterances into the given editor, if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('ResMgr.correct_recent')

    def scratch_recent(self, n = 1):
        """undo the effect of the most recent n utterances, if possible.

        **INPUTS**

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances actually undone
        """
        debug.virtual('ResMgr.scratch_recent')

    def reinterpret_recent(self, changed, delete_tentative_syms = 1):
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
        debug.virtual('ResMgr.reinterpret_recent')
   
    def can_reinterpret(self, n):
        """can we safely reinterpret the nth most recent utterance

        **INPUTS**

        *INT n* -- the depth in the editor state stack of the utterance
        to be reinterpreted

        **OUTPUTS**

        *BOOL* -- true if we can safely reinterpret that utterance
        """
        debug.virtual('ResMgr.can_reinterpret')
   

class ResMgrStd(ResMgr):
    """implementation of ResMgr including the standard processing of 
    dictation utterances.

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(ResMgrStd,
                            {
                            },
                            args)
        
    def _std_interp(self, result, app, initial_buffer = None, 
        clear_state = 0, before = None, after = None):
        """internal method for the standard sequence of calls to
        interpret the result of recognition by a dictation grammar,
        with callbacks to allow the caller to store the relevant 
        information to allow for correction.

        **INPUTS**

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *AppState app* -- the AppState interface to the editor (or a
        proxy object)

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting
        
        *BOOL clear_state* -- if true, tell the interpreter to clear
        its spacing and formatting state before starting to interpret
        the utterance
        
        *FCT* before -- a callback function, 
        before(*AppState* app, *STR* initial_buffer), to be called before
        interpretation starts, or None to do nothing

        *FCT* after -- a callback function, 
        after(*AppState* app, *STR* initial_buffer), to be called after
        interpretation is done, or None to do nothing

        **OUTPUTS**

        *UtteranceInterpretation* -- the object returned by
        CmdInterp.interpret_cmd_tuples
        """
        debug.trace('ResMgrStd._std_interp', 'standard interpretation, initial_buffer=%s' % initial_buffer)
        if before:
            debug.trace('ResMgrStd._std_interp', 'about to call before')
            before(app, initial_buffer = initial_buffer)
        interp = self.interpreter()
        interpreted = interp.interpret_utterance(result, app, 
            initial_buffer = initial_buffer, clear_state = clear_state)

        if after:
            debug.trace('ResMgrStd._std_interp', 'about to call after')
            after(app, initial_buffer = initial_buffer)
        app.print_buff_if_necessary(buff_name = initial_buffer)
        return interpreted

    def interpret_dictation(self, result, initial_buffer = None,
        utterance_number = None):
        """interpret the result of recognition by a dictation grammar,
        and store the relevant information to allow for correction.

        **INPUTS**

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        *INT utterance_number* -- number previously assigned to the
        utterance, if we are re-interpreting an earlier utterance, or
        None if result is from a new utterance and should be assigned 
        a new number

        **OUTPUTS**

        *none*
        """
# we don't store utterances, so we can ignore the utterance_number
        app = self.editor()
        debug.trace('ResMgrStd.interpret_dictation', 'about to interpret, initial_buffer=%s' % initial_buffer)
        self._std_interp(result, app, initial_buffer = initial_buffer)

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the given editor
        instance has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
# no information stored, so do nothing, but subclasses will need to
# define this
        pass

    def close_buffer_cbk(self, buff_name):
        """callback which notifies us that a buffer was closed

        **INPUTS**

        *STR* buff_name -- name of the buffer which was closed

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
# no information stored, so do nothing, but subclasses will need to
# define this
        pass

    def stored_utterances(self):
        """tells how many dictated utterances have been stored

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- number of utterances which can be retrieved with
        recent_dictation
        """
        return 0
# no information stored, but subclasses will need to
# define this

    def recent_dictation(self, n = None):
        """returns a list of SpokenUtterance objects

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

        *INT n* -- the number of utterances to return, or None to return 
        all available utterances.

        **OUTPUTS**

        *[(SpokenUtterance, INT, BOOL, UtteranceInterpretation)]* -- the n most recent dictation 
        utterances (or all available if < n), sorted most recent last, 
        each with:
          - a corresponding identifying number 
          - a flag indicating if the utterance can be undone and re-interpreted
            (or None if no utterances are stored)
          - results of the interpretation of that utterance
        The utterance number is unique, within a given editor instance.

        Note:  These utterances should not be stored permanently, nor
        should they be modified except as part of the correction
        process.  Also, the status of whether a given utterance can be
        re-interpreted may change if the user makes other changes to the 
        buffer during correction.
        """
        return None
        
    def recent_symbols(self, n=None):
        """returns a list of the most recently uttered symbols.

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

        *INT n* -- the number of utterances from which to pull recently dictated symbols.
        If None, then return all of them.

        **OUTPUTS**

        *[SymbolToReformat]* -- the symbols spoken in the n most recent 
        utterances (or all available if < n), sorted most recent last.
        
        Note:  These symbols should not be stored permanently, nor
        should they be modified except as part of the correction
        process.  Also, the status of whether a given utterance can be
        re-interpreted may change if the user makes other changes to the 
        """
        return None
        
# no information stored, but subclasses will need to
# define this

    def scratch_recent(self, n = 1):
        """undo the effect of the most recent n utterances, if possible.

        **INPUTS**

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances actually undone
        """
# no information stored, but subclasses will need to
# define this
        debug.trace('ResMgrStd.scratch_recent', 'n=%s' % n)
        return 0

    def reinterpret_recent(self, changed, delete_tentative_syms = 1):
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
# no information stored, but subclasses will need to
# define this
        return None
   
    def can_reinterpret(self, n):
        """can we safely reinterpret the nth most recent utterance

        **INPUTS**

        *INT n* -- the depth in the editor state stack of the utterance
        to be reinterpreted

        **OUTPUTS**

        *BOOL* -- true if we can safely reinterpret that utterance
        """
# no information stored, but subclasses will need to
# define this
        return 0
   

   
   

class BufferStates(OwnerObject):
    """Abstract interface which collects source buffer cookies for all
    buffers which currently exist in an application.
    """
    def __init__(self, **args):
        self.deep_construct(BufferStates, {}, args)
    
    def known_buffer(self, buff_name):
        """does the state have a cookie for a buffer with a given buff_name?

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *BOOL* -- true if the buffer is known
        """
        debug.virtual('BufferStates.known_buffer')

    def known_buffers(self):
        """returns a list of the names of known buffers

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of names of buffers for which we have cookies
        """
        debug.virtual('BufferStates.known_buffers')

    def cookie(self, buff_name):
        """returns the state cookie associated with a given buff_name

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *SourceBuffCookie* -- state cookie (see SourceBuff), or None if
        the buffer is unknown.  Note: this may be used temporarily but
        should not be stored permanently.  Also, note that 
        SourceBuffCookie is a dummy class.  The actual return type will 
        vary with the SourceBuff subclass.
        """ 
        debug.virtual('BufferStates.cookie')

    def valid_cookies(self, app, ignore_deleted = 0):
        """checks whether our state cookies are still valid
        If the state corresponding to a cookie has
        been lost, valid_cookies will return false.

        **INPUTS**

        *BOOL ignore_deleted* -- should we ignore buffers which no
        longer exist in the current state?

        **OUTPUTS**

        *BOOL* -- true if our cookies are valid
        """
        debug.virtual('BufferStates.valid_cookies')

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the editor
        has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        debug.virtual('BufferStates.rename_buffer_cbk')

    def compare_with_current(self, app, selection = 0, 
        ignore_new = 1, ignore_deleted = 0):
        """compares the stored state to the current one.
        
        **INPUTS**

        *AppState app* -- the editor whose states should be compared

        *BOOL* selection -- compare selection as well as contents

        *BOOL ignore_new* -- should we ignore new buffers (ones in the
        current state but not in the stored state)?

        *BOOL ignore_deleted* -- should we ignore buffers which no
        longer exist in the current state?

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of cookies
        """
        debug.virtual('BufferStates.compare_with_current')

    def changed_buffers(self, app, selection = 0, 
        ignore_new = 1, ignore_deleted = 0):
        """reports which buffers have changed between the stored state 
        and the current one.
        
        **INPUTS**

        *AppState app* -- the editor whose states should be compared

        *BOOL* selection -- compare selection as well as contents

        *BOOL ignore_new* -- should we ignore new buffers (ones in the
        current state but not in the stored state)?

        *BOOL ignore_deleted* -- should we ignore buffers which no
        longer exist in the current state?

        **OUTPUTS**

        *[STR]* -- list of names of affected buffers.  Note: if no
        buffers have been changed, an empty list will be returned, but
        if any cookies have expired or are invalid, changed_buffers will
        return None.  The caller should be careful to distinguish 
        between these two cases
        """
        debug.virtual('BufferStates.changed_buffers')

    def restore_state(self, app, buffers = None):
        """restores the editor to its stored state

        **NOTE:** restore_state never re-creates deleted buffers or
        modifies new ones, even if the deleted buffers are included in
        the buffers argument.
        
        **INPUTS**

        *AppState app* -- the editor whose states should be restored

        *[STR] buffers* -- list of buffers to restore, or None to
        restore all known buffers (except deleted ones)

        **OUTPUTS**

        *BOOL* -- true if restore was successful
        """
        debug.virtual('SourceBuff.restore_state')

class UnexpectedRestoreFailure(RuntimeError):
    """exception used internally by BufferStatesBasic.restore_state 
    to signal that restore failed on a particular buffer, and to 
    trigger an attempt to un-restore all the buffers previously 
    restored

    This exception should be caught by the restore_state method, 
    so it should never be seen outside that method.
    """
    def __init__(self, buff_name):
        """
        **INPUTS**

        *STR buff_name* -- name of the buffer which could not be
        restored
        """
        self.buff_name = buff_name
        RuntimeError.__init__(self, self.generate_message)

    def generate_message(self):
        warning = \
            'BufferStatesBasic.restore_state: unable to restore\n'
        warning = warning + \
            'state of buffer %s despite earlier check\n' % self.buff_name
        warning = warning + \
            'indicating that the cookie was valid\n'
        return warning
    
class BufferStatesBasic(BufferStates):
    """implementation of BufferStates

    **INSTANCE ATTRIBUTES**

    *{STR: SourceBuffCookie} cookies* -- map from buffer names to
    cookies representing the state of the buffer
    """
    def __init__(self, app, buffers = None, **args):
        """
        **INPUTS**

        *AppState app* -- the editor whose state should be stored

        *[STR] buffers* -- a list of buffers whose states should be
        stored, or None to store all buffers
        """
        self.deep_construct(BufferStatesBasic,
                            {
                             'cookies': {}
                            },
                            args)
        self.add_owned('cookies')
        if buffers is None:
# really, we don't want to get this from the editor:
# for storing the state before interpretation, we will already have
# synchronized with the editor.
# for storing the state after interpretation, we don't want to give the
# editor any chance to send updates.
            buffers = app.open_buffers_from_app()
        for buff_name in buffers:
            buffer = app.find_buff(buff_name)
            if buffer != None:
                self.cookies[buff_name] = buffer.store_current_state()

    def known_buffer(self, buff_name):
        """does the state have a cookie for a buffer with a given buff_name?

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *BOOL* -- true if the buffer is known
        """
        return self.cookies.has_key(buff_name)

    def known_buffers(self):
        """returns a list of the names of known buffers

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of names of buffers for which we have cookies
        """
        return self.cookies.keys()

    def cookie(self, buff_name):
        """returns the state cookie associated with a given buff_name

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *SourceBuffCookie* -- state cookie (see SourceBuff), or None if
        the buffer is unknown.  Note: this may be used temporarily but
        should not be stored permanently.  Also, note that 
        SourceBuffCookie is a dummy class.  The actual return type will 
        vary with the SourceBuff subclass.
        """ 
        if self.known_buffer(buff_name):
            return self.cookies[buff_name]
        return None

    def valid_cookies(self, app, ignore_deleted = 0):
        """checks whether our state cookies are still valid
        If the state corresponding to a cookie has
        been lost, valid_cookies will return false.

        **INPUTS**
        
        *AppState app* -- the editor to which our cookies belong

        *BOOL ignore_deleted* -- should we ignore buffers which no
        longer exist in the current state?

        **OUTPUTS**

        *BOOL* -- true if our cookies are valid
        """
        for buff_name in self.known_buffers():
            buffer = app.find_buff(buff_name)
            if buffer is None:
                if ignore_deleted:
                    continue
                return 0
            if not buffer.valid_cookie(self.cookies[buff_name]):
                return 0
        return 1

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the editor
        has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        if old_buff_name == new_buff_name:
            return
        try:
            self.cookies[new_buff_name] = self.cookies[old_buff_name]
            del self.cookies[old_buff_name]
        except KeyError:
            pass

    def close_buffer_cbk(self, buff_name):
        """callback which notifies us that a buffer has been closed

        **INPUTS**

        *STR* buff_name -- name of the buffer which was closed

        **OUTPUTS**

        *none*
        """
        try:
            del self.cookies[buff_name]
        except KeyError:
            pass

    def compare_current_buffer(self, app, selection = 0):
        """compares the stored state of the current buffer to its
        current state.
        
        **INPUTS**

        *AppState app* -- the editor whose states should be compared

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of cookies
        """
        buff_name = app.curr_buffer_name()
        if not self.known_buffer(buff_name):
            return 0

        buffer = app.find_buff(buff_name)
        if buffer is None:
            return 0
        if not buffer.compare_with_current(self.cookies[buff_name],
            selection = selection):
            return 0
        return 1

    def compare_with_current(self, app, selection = 0,
        ignore_new = 1, ignore_deleted = 0):
        """compares the stored state to the current one.
        
        **INPUTS**

        *AppState app* -- the editor whose states should be compared

        *BOOL* selection -- compare selection as well as contents

        *BOOL ignore_new* -- should we ignore new buffers (ones in the
        current state but not in the stored state)?

        *BOOL ignore_deleted* -- should we ignore buffers which no
        longer exist in the current state?

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of cookies
        """

        if not ignore_new:
            current_buffers = app.open_buffers_from_app()
            for buff_name in current_buffers:
                if not self.known_buffer(buff_name):
                    debug.trace('BufferStatesBasic.compare_with_current',
                        'unknown buffer %s' % buff_name)
                    return 0

        for buff_name in self.known_buffers():
            buffer = app.find_buff(buff_name)
            if buffer is None:
                if ignore_deleted:
                    continue
                debug.trace('BufferStatesBasic.compare_with_current',
                    'SourceBuff for known buffer %s not found' % buff_name)
                return 0
            if not buffer.compare_with_current(self.cookies[buff_name],
                selection = selection):
                debug.trace('BufferStatesBasic.compare_with_current',
                    'buffer %s compared false' % buff_name)
                return 0
        return 1

    def changed_buffers(self, app, selection = 0, 
        ignore_new = 1, ignore_deleted = 0):
        """reports which buffers have changed between the stored state 
        and the current one.
        
        **INPUTS**

        *AppState app* -- the editor whose states should be compared

        *BOOL* selection -- compare selection as well as contents

        *BOOL ignore_new* -- should we ignore new buffers (ones in the
        current state but not in the stored state)?

        *BOOL ignore_deleted* -- should we ignore buffers which no
        longer exist in the current state?

        **OUTPUTS**

        *[STR]* -- list of names of affected buffers.  Note: if no
        buffers have been changed, an empty list will be returned, but
        if any cookies have expired or are invalid, changed_buffers will
        return None.  The caller should be careful to distinguish 
        between these two cases
        """
        changed = []
# add new buffers to changed list, unless we are ignoring new buffers
        if not ignore_new:
            current_buffers = app.open_buffers_from_app()
            for buff_name in current_buffers:
                if not self.known_buffer(buff_name):
                    changed.append(buff_name)

        for buff_name in self.known_buffers():
            cookie = self.cookies[buff_name]
            buffer = app.find_buff(buff_name)
# add deleted buffers to changed list, unless we are ignoring deleted buffers
            if not buffer:
                if not ignore_deleted:
                    changed.append(buff_name)
                continue
            if not buffer.valid_cookie(cookie):
# but return None if any known and undeleted buffers lack a valid cookie
                return None
# now, the buffer should still exist and the cookie should be valid, so a 
# comparison should only fail if the buffer was indeed changed
            if not buffer.compare_with_current(cookie, selection = selection):
                changed.append(buff_name)

        return changed

    def restore_state(self, app, buffers = None):
        """restores the editor to its stored state

        **NOTE:** restore_state never re-creates deleted buffers or
        modifies new ones, even if the deleted buffers are included in
        the buffers argument.
        
        **INPUTS**

        *AppState app* -- the editor whose states should be restored

        *[STR] buffers* -- list of buffers to restore, or None to
        restore all known buffers (except deleted ones)

        **OUTPUTS**

        *BOOL* -- true if restore was successful
        """
# we have to check all our cookies first, so that we don't restore some
# buffers before realizing that others can't be restored,
# leaving the editor in a mixed up state

        debug.trace('BufferStatesBasic.restore_state', 'app=%s' % app)
        if not self.valid_cookies(app, ignore_deleted = 1):
            return 0

        debug.trace('BufferStatesBasic.restore_state',
                    'cookies valid, proceeding to restore')
        temporary_cookies = {}
        if buffers is None:
            buffers = self.known_buffers()
        try:
            for buff_name in buffers:
                buffer = app.find_buff(buff_name)
                if buffer is None:
# when restoring, always ignore deleted buffers
                    continue
                temporary_cookies[buff_name] = buffer.store_current_state()
                debug.trace('BufferStatesBasic.restore_state',
                    'restoring %s' % buff_name)
                if not buffer.restore_state(self.cookies[buff_name]):
                    raise UnexpectedRestoreFailure(buff_name = buff_name)
            return 1
        except UnexpectedRestoreFailure, e:
            failed_to_fix = []
            for buff_name, cookie in temporary_cookies.items():
                if buff_name == e.buff_name:
                    continue
                buffer = app.find_buff(buff_name)
                if buffer is None:
# when restoring, always ignore deleted buffers
                    continue
# here, ignore errors (we want to un-restore the rest of the buffers,
# and besides, what else can we do but try)
                if not buffer.restore_state(temporary_cookies[buff_name]):
                    failed_to_fix.append(buff_name)
            warning = e.generate_message()
            debug.critical_warning(warning)
            if len(failed_to_fix) != 0:
                failed = 'Failed to un-restore the following buffers:\n'
                for buff_name in failed_to_fix:
                    failed = failed + '%s\n' % buff_name
                debug.critical_warning(failed_to_fix)
            return 0
        
class StateStack(OwnerObject):
    """Abstract interface for storing a stack of
    BufferStates representing the state of an editor's buffers before
    interpretation of the most recent dictation utterances

    **NOTE:** To maintain the integrity of the StateStack, before_interp
    MUST be called before every dictation utterance which is stored in
    the utterance stack, after_interp MUST be called after the utterance
    and before any other updates (besides those in direct response to
    the interpretation) from the editor are processed, and no other
    methods of StateStack may be called between the calls to
    before_interp and after_interp.
    """
    def __init__(self, **args):
        self.deep_construct(StateStack, {}, args)
    
    def before_interp(self, app, initial_buffer = None):
        """method which must be called before interpretation of a
        dictation utterance to store editor state (or compare with the
        state stored after the previous utterance).

        This method will compare the state after the most recent dictation 
        utterance to the current state.  If they do not match,
        before_interp will clear the stack.  Then, it will push the 
        current state onto the stack.

        After this method returns, the top of the stack will be the 
        editor state before the utterance is interpreted.

        **NOTE:** To maintain the integrity of the StateStack, before_interp
        MUST be called before every dictation utterance which is stored in
        the utterance stack, after_interp MUST be called after the utterance
        and before any other updates (besides those in direct response to
        the interpretation) from the editor are processed, and no other
        methods of StateStack may be called between the calls to
        before_interp and after_interp.
            
        **INPUTS**

        *AppState app* -- the editor into which the user is dictating

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        debug.virtual('StateStack.before_interp')

    def after_interp(self, app, initial_buffer = None):
        """method which must be called after interpretation of a
        dictation utterance to store editor state.

        This method will store the current state, but not on the stack.

        **NOTE:** To maintain the integrity of the StateStack, before_interp
        MUST be called before every dictation utterance which is stored in
        the utterance stack, after_interp MUST be called after the utterance
        and before any other updates (besides those in direct response to
        the interpretation) from the editor are processed, and no other
        methods of StateStack may be called between the calls to
        before_interp and after_interp.
        
        **INPUTS**

        *AppState app* -- the editor into which the user is dictating

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        debug.virtual('StateStack.after_interp')

    def safe_depth(self, app):
        """returns the number of entries in the stack which can be
        safely restored

        **INPUTS**

        *AppState app* -- the editor 

        **OUTPUTS**

        *INT* -- the depth in the state stack to which we can
        safely restore the editor
        """
        debug.virtual('StateStack.safe_depth')

    def safe_reinterp_depth(self, app):
        """returns the number of entries in the stack which can be
        safely restored and then reinterpreted

        **INPUTS**

        *AppState app* -- the editor 

        **OUTPUTS**

        *INT* -- the depth in the state stack to which we can
        safely restore the editor and then reinterpret the undone utterances
        """
        debug.virtual('StateStack.safe_reinterp_depth')

    def undo_manual_changes(self, app):
        """restores the editor to its state just after the most recent 
        dictated utterance, but prior to any subsequent manual changes.
        (Any new buffers will not be removed, nor will buffers deleted
        since the utterance be restored).

        **NOTE:** use this method with extreme caution, as the
        StateStack provides no means to redo these changes, and the 
        user may not expect manual changes to vanish.

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        **OUTPUTS**

        *BOOL* -- true if we successfully restored to that state
        """
        debug.virtual('StateStack.undo_manual_changes')

    def can_restore(self, app, n):
        """can we safely restore the editor to its nth most recent
        stored state (popping n entries off our stack)?

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        *INT n* -- the depth in the editor state stack to which we are 
        trying to restore the state (n = 1 refers to the top entry)

        **OUTPUTS**

        *BOOL* -- true if we can safely restore to that state
        """
        debug.virtual('StateStack.can_restore')

    def can_reinterpret(self, app, n):
        """can we safely restore the editor to its nth most recent
        stored state (popping n entries off our stack), and then
        reinterpret the corresponding utterances?

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        *INT n* -- the depth in the editor state stack to which we are 
        trying to restore the state (n = 1 refers to the top entry)

        **OUTPUTS**

        *BOOL* -- true if we can safely restore to that state, and then
        reinterpret the undone utterances
        """
        debug.virtual('StateStack.can_reinterpret')

    def pop(self, app, n):
        """restores the editor to its nth most recent stored state, if
        this can be done safely, popping n entries off our stack.

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        *INT n* -- the depth in the editor state stack to which we are 
        trying to restore the state (n = 1 refers to the top entry)

        *BOOL* -- true if we sucessfully restored the editor to that state
        """
        debug.virtual('StateStack.pop')

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the editor
        has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        debug.virtual('StateStack.rename_buffer_cbk')

    def close_buffer_cbk(self, buff_name):
        """callback which notifies us that a buffer was closed

        **INPUTS**

        *STR* buff_name -- name of the buffer which was closed

        **OUTPUTS**

        *none*
        """
        debug.virtual('StateStack.close_buffer_cbk')

class StateStackBasic(StateStack):
    """implementation of StateStack using BufferStatesBasic

    **INSTANCE ATTRIBUTES**

    *[BufferStatesBasic] states* -- the stack of stored editor states
    before each utterance

    *[STR] initial_buffers* -- stack of initial buffers for each utterance

    *[BOOL] cross_buffer* -- stack of flags for each utterance,
    indicating which ones were cross-buffer utterances (ones which
    affected the state, position, or selection of any buffer other than 
    their initial_buffer)

    *[INT] pos_after* -- stack of positions in the initial buffer 
    after each utterance

    *[(INT, INT)] sel_after* -- stack of selections in the initial
    buffer after each utterance

    *BufferStatesBasic after_utterance* -- the state of the editor after
    the most recent utterance

    *INT max_depth* -- maximum depth of the stack

    *BOOL ignore_new* -- should we ignore new buffers (ones in the
    current state but not in the stored state) when comparing states,
    and simply restore the states of the stored buffers?

    *BOOL ignore_deleted* -- should we ignore buffers which no
    longer exist in the current state, and simply restore the states of
    the buffers which still exist?
    """
    def __init__(self, max_depth, ignore_new = 1, ignore_deleted = 0, **args):
        """
        **INPUTS**

        *INT max_depth* -- maximum depth of the stack, or -1 to allow an
        unlimited stack

        *BOOL ignore_new* -- should we ignore new buffers (ones in the
        current state but not in the stored state) when comparing states,
        and simply restore the states of the stored buffers?

        *BOOL ignore_deleted* -- should we ignore buffers which no
        longer exist in the current state, and simply restore the states of
        the buffers which still exist?
        """
        self.deep_construct(StateStackBasic, 
                            {
                             'states': [],
                             'initial_buffers': [],
                             'final_buffers': [],
                             'cross_buffer': [],
                             'pos_after': [],
                             'sel_after': [],
                             'after_utterance': None,
                             'max_depth': max_depth,
                             'ignore_new': ignore_new,
                             'ignore_deleted': ignore_deleted
                            }, args)
        self.add_owned('states')

    def before_interp(self, app, initial_buffer = None):
        """method which must be called before interpretation of a
        dictation utterance to store editor state (or compare with the
        state stored after the previous utterance).

        This method will compare the state after the most recent dictation 
        utterance to the current state.  If they do not match,
        before_interp will clear the stack.  Then, it will push the 
        current state onto the stack.

        After this method returns, the top of the stack will be the 
        editor state before the utterance is interpreted.

        **NOTE:** To maintain the integrity of the StateStack, before_interp
        MUST be called before every dictation utterance which is stored in
        the utterance stack, after_interp MUST be called after the utterance
        and before any other updates (besides those in direct response to
        the interpretation) from the editor are processed, and no other
        methods of StateStack may be called between the calls to
        before_interp and after_interp.
            
        **INPUTS**

        *AppState app* -- the editor into which the user is dictating

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        if self.after_utterance and \
            not self.after_utterance.compare_with_current(app, 
                ignore_new = self.ignore_new,
                ignore_deleted = self.ignore_deleted):
# if the current state doesn't match that after the (previous) most
# recent utterance, then the rest of the stack is invalid, so clear it
                self._clear()
        self.after_utterance = None
        current = BufferStatesBasic(app)
        if initial_buffer is None:
            initial_buffer = app.curr_buffer_name()
        self._push(current, initial_buffer)

    def interp_state_valid(self, app):
        """determines whether the user has done anything which would
        invalidate the interpreter state (including spacing and 
        formatting state) since the last utterance.

        The state is assumed to be invalid if any of the following
        conditions apply:

        (1) the user has switched buffers
        (2) the current buffer has been modified
        (3) the position or selection have changed since the last
        utterance 

        **INPUTS**

        *AppState app* -- the editor into which the user is dictating

        **OUTPUTS**

        *BOOL* -- true if the interpreter state remains valid
        """
        if len(self.states) == 0 or not self.after_utterance:
            return 0
        buff_name = app.curr_buffer_name()
        if self.cross_buffer[-1]:
            return 0
        if buff_name != self.final_buffers[-1]:
            return 0
        return self.after_utterance.compare_current_buffer(app, 
            selection = 1)

    def _clear(self):
        """private method to clear the stack(s)

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.states = []
        self.initial_buffers = []
        self.final_buffers = []
        self.pos_after = []
        self.sel_after = []
        self.cross_buffer = []

    def _pop_states(self, n):
        """private method to pop the last n states off the stack 
        (without restoring them)

        **INPUTS**

        *INT n* -- number of states to pop

        **OUTPUTS**

        *none*
        """
        del self.states[-n:]
        del self.initial_buffers[-n:]
        del self.final_buffers[-n:]
        del self.cross_buffer[-n:]
        del self.pos_after[-n:]
        del self.sel_after[-n:]

    def check_stacks(self):
        """check that all the stacks are the same height

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        d = len(self.states)
        if len(self.initial_buffers) != d:
            debug.trace('StateStackBasic.check_stacks',
                '%d states but %d initial_buffers' % \
                (d, len(self.initial_buffers)))
        if len(self.cross_buffer) != d:
            debug.trace('StateStackBasic.check_stacks',
                '%d states but %d cross_buffer' % \
                (d, len(self.cross_buffer)))
        if len(self.pos_after) != d:
            debug.trace('StateStackBasic.check_stacks',
                '%d states but %d pos_after' % \
                (d, len(self.pos_after)))
        if len(self.sel_after) != d:
            debug.trace('StateStackBasic.check_stacks',
                '%d states but %d sel_after' % \
                (d, len(self.sel_after)))
        if len(self.final_buffers) != d:
            debug.trace('StateStackBasic.check_stacks',
                '%d states but %d final_buffers' % \
                (d, len(self.sel_after)))

    def _push(self, state, initial_buffer):
        """private method to push a state onto the stack, removing an
        element from the bottom if the depth exceeds max_depth

        **INPUTS**

        *BufferStateBasic state* -- the state to push onto the stack

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        self.check_stacks()
        self.states.append(state)
        self.initial_buffers.append(initial_buffer)
# we only push new entries onto the states and initial_buffers stack here,
# so after_interp must push new entries onto the rest
        if self.max_depth > 0 and len(self.states) > self.max_depth:
            del self.states[0]
            del self.initial_buffers[0]
#            del self.final_buffers[0]
#            del self.cross_buffer[0]
#            del self.pos_after[0]
#            del self.sel_after[0]

    def _delete_invalid_states(self, n):
        """private method which deletes all but the last n entries
        from the state stack and all corresponding stacks.

        **INPUTS**

        *INT n* -- the number of states to LEAVE on the stack

        **OUTPUTS**

        *BOOL* -- true if we were not between before_interp and
        after_interp and could safely delete these entries
        """
        if self.after_utterance is None:
            return 0
        del self.states[0:-n]
        del self.initial_buffers[0:-n]
        del self.final_buffers[0:-n]
        del self.cross_buffer[0:-n]
        del self.pos_after[0:-n]
        del self.sel_after[0:-n]
        self.check_stacks()
        return 1

    def after_interp(self, app, initial_buffer = None):
        """method which must be called after interpretation of a
        dictation utterance to store editor state.

        This method will store the current state, but not on the stack.

        **NOTE:** To maintain the integrity of the StateStack, before_interp
        MUST be called before every dictation utterance which is stored in
        the utterance stack, after_interp MUST be called after the utterance
        and before any other updates (besides those in direct response to
        the interpretation) from the editor are processed, and no other
        methods of StateStack may be called between the calls to
        before_interp and after_interp.
        
        **INPUTS**

        *AppState app* -- the editor into which the user is dictating

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        **OUTPUTS**

        *none*
        """
        current = BufferStatesBasic(app)
        self.after_utterance = current

        final_buffer = app.curr_buffer_name()
        if initial_buffer is None:
            initial_buffer = final_buffer
        buffer = app.find_buff(initial_buffer)
        if not buffer:
            self._push_cross_buffer(final_buffer = final_buffer)
            debug.trace('StateStackBasic.after_interp', 
                    'buffer %s not found (so cross-buffer utterance)' \
                    % initial_buffer)
            return

        before = self.states[-1]
        changed = before.changed_buffers(app, selection = 1, 
            ignore_new = 0, ignore_deleted = 0)
        if len(changed) > 1 \
            or (len(changed) == 1 and changed[0] != initial_buffer):
            debug.trace('StateStackBasic.after_interp', 
                    'initial, changed buffers are %s, %s' \
                    % (initial_buffer, repr(changed)))
            self._push_cross_buffer(final_buffer = final_buffer)
            return
        cookie = current.cookie(initial_buffer)
        pos, sel = buffer.get_state_pos_selection(cookie)
        self._push_single_buffer(pos, sel)

    def _push_cross_buffer(self, final_buffer):
        """push supplementary information about a cross_buffer utterance
        onto the appropriate stacks

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.cross_buffer.append(1)
        self.final_buffers.append(final_buffer)
        self.pos_after.append(None)
        self.sel_after.append(None)
        if self.max_depth > 0 and len(self.final_buffers) > self.max_depth:
            del self.final_buffers[0]
            del self.cross_buffer[0]
            del self.pos_after[0]
            del self.sel_after[0]
        self.check_stacks()

    def _push_single_buffer(self, pos, selection):
        """push supplementary information about a single buffer utterance
        onto the appropriate stacks

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.cross_buffer.append(0)
        self.final_buffers.append(self.initial_buffers[-1])
        self.pos_after.append(pos)
        self.sel_after.append(selection)
        if self.max_depth > 0 and len(self.final_buffers) > self.max_depth:
            del self.final_buffers[0]
            del self.cross_buffer[0]
            del self.pos_after[0]
            del self.sel_after[0]
        self.check_stacks()

    def safe_depth_preliminaries(self, app):
        """does preliminary checks for the full safe_depth/safe_reinterp_depth 
        checks, including synchronizing VoiceCode with the editor

        **INPUTS**

        *AppState app* -- the editor 

        **OUTPUTS**

        *BOOL* -- true if the state stack is not empty, we are not in
        between before_interp and after_interp, and the buffer has not
        changed since after the last utterance
        """
        if len(self.states) == 0:
            debug.trace('StateStackBasic.safe_depth_preliminaries', 'empty stack')
            return 0
        if self.after_utterance is None:
            debug.trace('StateStackBasic.safe_depth_preliminaries', 
                'no state after utterance')
            return 0
        if debug.trace_is_active('StateStackBasic.safe_depth_preliminaries'):
            debug.trace('StateStackBasic.safe_depth_preliminaries',
                 'called from thread %s' %
                 threading.currentThread().getName())
#            debug.trace_call_stack('StateStackBasic.safe_depth_preliminaries')
# have to synchronize to ensure that we've processed any pending updates
# and that "current" is really current.
        debug.trace('StateStackBasic.safe_depth_preliminaries', 
            'about to synchronize')
        app.synchronize_with_app()
        debug.trace('StateStackBasic.safe_depth_preliminaries', 
            'just synchronized, about to compare')
        same = self.after_utterance.compare_with_current(app, 
            ignore_new = self.ignore_new, 
            ignore_deleted = self.ignore_deleted)
        if not same:
            debug.trace('StateStackBasic.safe_depth_preliminaries', 'not same')
            return 0
        return 1

    def safe_depth(self, app):
        """returns the number of entries in the stack which can be
        safely restored

        **INPUTS**

        *AppState app* -- the editor 

        **OUTPUTS**

        *INT* -- the depth in the state stack to which we can
        safely restore the editor
        """
        if not self.safe_depth_preliminaries(app):
            return 0
        debug.trace('StateStackBasic.safe_depth', 'same as after_utterance')
        self.check_stacks()
# in this implementation, we clear the stack whenever the state before
# the next dictation utterance doesn't match the state after
# the previous utterance.  Therefore, the only thing left to check is
# whether the states in the stack have valid cookies
        for n in range(1, len(self.states) + 1):
            if not self.states[-n].valid_cookies(app, 
                ignore_deleted = self.ignore_deleted):
                debug.trace('StateStackBasic.safe_depth', 
                    'cookie at %d no longer valid, deleting' % n)
# if the cookies aren't valid, we might as well delete those states
                self._delete_invalid_states(n-1)
                return n-1
        return len(self.states)


    def safe_reinterp_depth(self, app):
        """returns the number of entries in the stack which can be
        safely restored and then reinterpreted

        **INPUTS**

        *AppState app* -- the editor 

        **OUTPUTS**

        *INT* -- the depth in the state stack to which we can
        safely restore the editor and then reinterpret the undone utterances
        """
        if not self.safe_depth_preliminaries(app):
            return 0
        debug.trace('StateStackBasic.safe_reinterp_depth', 
            'same as after_utterance')
        self.check_stacks()
# in this implementation, we don't allow reinterpretation of 
#
# (1) any  cross-buffer utterances, or 
# 
# (2) any single-buffer utterance whose buffer no longer exists
#
# (3) any single-buffer utterances for which the position and selection 
# in that buffer were changed after that utterance was interpreted and 
# before the next utterance with the same initial buffer (if any)

# To enforce the third condition, we need to work backwards through the
# state stack, keeping track of which utterances most recently started in 
# each buffer.  We do this with the most_recent_utterance map
        most_recent_utterance = {}

        for n in range(1, len(self.states) + 1):
#           first check for valid cookies
            if not self.states[-n].valid_cookies(app, 
                ignore_deleted = self.ignore_deleted):
                debug.trace('StateStackBasic.safe_reinterp_depth', 
                    'cookie at %d no longer valid, deleting' % n)
#               if the cookies aren't valid, we might as well delete 
#               those states
                self._delete_invalid_states(n-1)
                debug.trace('StateStackBasic.safe_reinterp_depth',
                            'Returning n-1=%s' % (n-1))
                return n - 1
#           next, check if the utterance was a cross-buffer utterance
            if self.cross_buffer[-n]:
                debug.trace('StateStackBasic.safe_reinterp_depth', 
                    'cross-buffer utterance at %d' % n)
                debug.trace('StateStackBasic.safe_reinterp_depth',
                                'Returning n-1=%s' % (n-1))
                return n - 1
#           otherwise, for single-buffer utterances, check that the
#           buffer still exists
            buff_name = self.initial_buffers[-n]
            if buff_name is None:
                if self.ignore_deleted:
                    continue
                else:
                    debug.trace('StateStackBasic.safe_reinterp_depth', 
                        'no buffer for utterance at %d' % n)
                    debug.trace('StateStackBasic.safe_reinterp_depth',
                                'Returning n-1=%s' % (n-1))
                    return n - 1
            debug.trace('StateStackBasic.safe_reinterp_depth', 
                    'buffer %s for utterance at %d' % (buff_name, n))
            buffer = app.find_buff(buff_name)
            if buffer is None:
                debug.trace('StateStackBasic.safe_reinterp_depth', 
                    'deleted buffer %s for utterance at %d' % (buff_name, n))
                debug.trace('StateStackBasic.safe_reinterp_depth',
                                'Returning n-1=%s' % (n-1))
                return n - 1
#           and finally, check against most_recent_utterance
            try:
                debug.trace('StateStackBasic.safe_reinterp_depth', 
                        'utterance %d with initial buffer %s' \
                        % (n, buff_name))
                next = most_recent_utterance[buff_name]
                debug.trace('StateStackBasic.safe_reinterp_depth', 
                        'next utterance in same buffer is %d' % next)
                this_cookie = self.states[-n].cookie(buff_name)
                pos = self.pos_after[-n]
                sel = self.sel_after[-n]
                debug.trace('StateStackBasic.safe_reinterp_depth', 
                        'pos, sel = %d, %s' % (pos, repr(sel)))
                next_cookie = self.states[-next].cookie(buff_name)
                next_pos, next_sel = \
                    buffer.get_state_pos_selection(next_cookie)
                debug.trace('StateStackBasic.safe_reinterp_depth', 
                        'next pos, sel = %d, %s' % (next_pos, repr(next_sel)))
                if pos != next_pos or sel != next_sel:
                    debug.trace('StateStackBasic.safe_reinterp_depth', 
                        'pos in %s changed\n from %d to %d' \
                        % (buff_name, n, next))
                    debug.trace('StateStackBasic.safe_reinterp_depth',
                                'Returning n-1=%s' % (n-1))
                    return n - 1
            except KeyError:
                pass
# update most_recent_utterance
            most_recent_utterance[buff_name] = n
            
        debug.trace('StateStackBasic.safe_reinterp_depth', 
                    'Nothing particular, returning len(self.states)=%s' % len(self.states))
        return len(self.states)

    def undo_manual_changes(self, app):
        """restores the editor to its state just after the most recent 
        dictated utterance, but prior to any subsequent manual changes.
        (Any new buffers will not be removed, nor will buffers deleted
        since the utterance be restored).

        **NOTE:** use this method with extreme caution, as the
        StateStack provides no means to redo these changes, and the 
        user may not expect manual changes to vanish.

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        **OUTPUTS**

        *BOOL* -- true if we successfully restored to that state
        """
        debug.trace('StateStackBasic.undo_manual_changes', 'invoked')
        if self.after_utterance is None:
            return 0
        return self.after_utterance.restore_state(app)

    def can_restore(self, app, n):
        """can we safely restore the editor to its nth most recent
        stored state (popping n entries off our stack)?

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        *INT n* -- the depth in the editor state stack to which we are 
        trying to restore the state (n = 1 refers to the top entry)

        **OUTPUTS**

        *BOOL* -- true if we can safely restore to that state
        """
        if n < 1:
            return 0
        if len(self.states) < n:
            return 0
        safe = self.safe_depth(app)
        if safe >= n:
            return 1
        return 0

    def can_reinterpret(self, app, n):
        """can we safely restore the editor to its nth most recent
        stored state (popping n entries off our stack), and then
        reinterpret the corresponding utterances?

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        *INT n* -- the depth in the editor state stack to which we are 
        trying to restore the state (n = 1 refers to the top entry)

        **OUTPUTS**

        *BOOL* -- true if we can safely restore to that state, and then
        reinterpret the undone utterances
        """
        if n < 1:
            return 0
        if len(self.states) < n:
            return 0
        safe = self.safe_reinterp_depth(app)
        if safe >= n:
            return 1
        return 0


    def old_pop(self, app, n):
        """restores the editor to its nth most recent stored state, if
        this can be done safely, popping n - 1 entries off our stack.

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        *INT n* -- the depth in the editor state stack to which we are 
        trying to restore the state (n = 1 refers to the top entry)

        *BOOL* -- true if we sucessfully restored the editor to that state
        """
        debug.trace('StateStackBasic.pop', 'called with n = %d' % n)
        if not self.can_restore(app, n):
            debug.trace('StateStack.pop', 'unable to restore')
            return 0
        temporary = BufferStatesBasic(app)
        if not self.undo_manual_changes(app):
            warning = \
                'StateStack.pop: unexpected failure to restore state\n'
            debug.critical_warning(warning)
            temporary.restore_state(app)
            return 0
        for i in range(1, n+1):
            debug.trace('StateStack.pop', 'popping %d' % i)
            if not self.states[-i].restore_state(app):
# since we checked cookies in can_restore, this shouldn't happen, but
# if it does, un-restore the state before returning 0 to signal failure
                warning = \
                    'StateStack.pop: unexpected failure to restore state\n'
                debug.critical_warning(warning)
                temporary.restore_state(app)
                return 0
        current = BufferStatesBasic(app)
        self.after_utterance = current
# in general, we need a whole new set of cookies because we've just
# popped the old ones
#        self.after_utterance = self.states[-n]
        self._pop_states(n)
        return 1

    def pop(self, app, n):
        """restores the editor to its nth most recent stored state, if
        this can be done safely, popping n - 1 entries off our stack.

        **NOTE:** This method must not 
        be called between before_interp and after_interp.

        **INPUTS**

        *AppState app* -- the editor 

        *INT n* -- the depth in the editor state stack to which we are 
        trying to restore the state (n = 1 refers to the top entry)

        *BOOL* -- true if we sucessfully restored the editor to that state
        """
        debug.trace('StateStackBasic.pop', 'called with n = %d' % n)
        if not self.can_restore(app, n):
            debug.trace('StateStackBasic.pop', 'unable to restore')
            return 0
        temporary = BufferStatesBasic(app)
# create a map from buffer names to the oldest BufferStatesBasic which
# knows about that buffer
        buff_indices = {}
        for i in range(1, n+1):
            for buff in self.states[-i].known_buffers():
                buff_indices[buff] = i
        which_buffers = []
        for buff in self.after_utterance.known_buffers():
            if not buff_indices.has_key(buff):
                which_buffers.append(buff)
        if which_buffers:
            if not self.after_utterance.restore_state(app, buffers =
                which_buffers):
# since we checked cookies in can_restore, this shouldn't happen, but
# if it does, un-restore the state before returning 0 to signal failure
                warning = \
                    'StateStackBasic.pop: unexpected failure to restore state\n'
                debug.critical_warning(warning)
                temporary.restore_state(app)
                return 0
        for i in range(1, n+1):
            debug.trace('StateStackBasic.pop', 'popping %d' % i)
            which_buffers = []
            for buff in self.states[-i].known_buffers():
                if buff_indices[buff] == i:
                    which_buffers.append(buff)
# only restore those buffers for which we aren't later going to restore an
# earlier state.  This makes restores more efficient by avoiding multiple 
# restores of the same buffer.
            if not self.states[-i].restore_state(app, buffers =
                which_buffers):
# since we checked cookies in can_restore, this shouldn't happen, but
# if it does, un-restore the state before returning 0 to signal failure
                warning = \
                    'StateStackBasic.pop: unexpected failure to restore state\n'
                debug.critical_warning(warning)
                temporary.restore_state(app)
                return 0
        current = BufferStatesBasic(app)
        self.after_utterance = current
# in general, we need a whole new set of cookies because we've just
# popped the old ones
#        self.after_utterance = self.states[-n]
        self._pop_states(n)
        return 1

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the editor
        has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        for state in self.states:
            state.rename_buffer_cbk(old_buff_name, new_buff_name)
        for i in range(len(self.initial_buffers)):
            if self.initial_buffers[i] == old_buff_name:
                self.initial_buffers[i] = new_buff_name
        for i in range(len(self.final_buffers)):
            if self.final_buffers[i] == old_buff_name:
                self.final_buffers[i] = new_buff_name

    def close_buffer_cbk(self, buff_name):
        """callback which notifies us that a buffer was closed

        **INPUTS**

        *STR* buff_name -- name of the buffer which was closed

        **OUTPUTS**

        *none*
        """
        for state in self.states:
            state.close_buffer_cbk(buff_name)
        for i in range(len(self.initial_buffers)):
            if self.initial_buffers[i] == buff_name:
                self.initial_buffers[i] = None
        for i in range(len(self.final_buffers)):
            if self.final_buffers[i] == buff_name:
                self.final_buffers[i] = None


class ResMgrBasic(ResMgrStd):
    """implementation of ResMgrStd providing services necessary for
    basic correction.

    **INSTANCE ATTRIBUTES**

    *INT max_utterances* -- the maximum number of recent dictation utterances 
    to store

    *CorrectUtteranceEvent correct_evt* -- doorbell used to send an 
    event to bring up a correction box asynchronously

    *CorrectRecentEvent correct_recent_evt* -- doorbell used to send an 
    event to bring up a correct recent box asynchronously
    
    *ReformatSymbolEvent reformat_recent_evt* -- doorbell used to send an 
    event to bring up a reformat recent box asynchronously

    *[SpokenUtterance] utterances* -- stack of recent utterances, sorted with
    most recent last (technically a queue, since it has finite size and
    the oldest utterances can be dropped on push to maintain this limit)

    *[UtteranceInterpretation] interpreted* -- stack of recent interpreation
    results, sorted with most recent last (technically a queue, since it 
    has finite size and the oldest result can be dropped on push to 
    maintain this limit)

    *[StoredInterpState] interp_states* -- stack of strore interpreter
     (formatting and spacing) states before each utterance

    *[STR] initial_buffers* -- stack of names of initial buffers 
    corresponding to the utterances (technically a queue, since it has 
    finite size and the oldest utterances can be dropped on push to 
    maintain this limit)

    *[INT] numbers* -- stack of numbers 
    corresponding to the utterances (technically a queue, since it has 
    finite size and the oldest utterances can be dropped on push to 
    maintain this limit)

    *INT next_number* -- next number to be assigned to the next new
    utterance

    *INT call_count* -- TEMPORARY counter used to track calls to
    recent_dictation for debugging purposes - DCF

    *StateStackBasic states* -- stack representing the state of the editor 
    application before/after recent utterances
    """
    def __init__(self, correct_evt, correct_recent_evt, reformat_recent_evt,
        max_utterances = 30, **args):
        self.deep_construct(ResMgrBasic,
                            {
                             'max_utterances': max_utterances,
                             'correct_evt': correct_evt,
                             'correct_recent_evt': correct_recent_evt,
                             'reformat_recent_evt': reformat_recent_evt,
                             'utterances': [],
                             'interpreted': [],
                             'interp_states': [],
                             'numbers': [],
                             'next_number': 0,
                             'initial_buffers': [],
                             'call_count': 0,
                             'states': StateStackBasic(max_utterances)
                            },
                            args)
        self.add_owned('utterances')
        self.add_owned('states')

    def remove_other_references(self):
        self.correct_evt = None
        self.correct_recent_evt = None
        self.reformat_recent_evt = None
        
    def store(self, result, interpreted, initial_buffer, number):
        """store the result and its interpretation information after
        each utterance

        **INPUTS**

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *UtteranceInterpretation interpreted* -- an object representing the
        results of interpretation

        *INT number* -- number assigned to the utterance by
        interpret_dictation

        **OUTPUTS**

        *none*
        """
        debug.trace('ResMgrBasic.store', 
            'storing an utterance (already have %d)' % len(self.utterances))
        if len(self.utterances) >= self.max_utterances:
            debug.trace('ResMgrBasic.store', 'removing the oldest')
            del self.utterances[0]
            del self.interpreted[0]
            del self.initial_buffers[0]
            del self.numbers[0]
            # this is appended before the utterance, but we can wait
            # until after to truncate the stack
            del self.interp_states[0]
        self.utterances.append(result)
        self.interpreted.append(interpreted)
        self.initial_buffers.append(initial_buffer)
        self.numbers.append(number)
        result.id = number

    def interpret_dictation(self, result, initial_buffer = None,
        utterance_number = None):
        """interpret the result of recognition by a dictation grammar,
        and store the relevant information to allow for correction.

        **INPUTS**

        *SpokenUtterance result* -- a SpokenUtterance object
        representing the recognition results

        *STR initial_buffer* -- the name of the initial buffer which was
        active at recognition-starting

        *INT utterance_number* -- number previously assigned to the
        utterance, if we are re-interpreting an earlier utterance, or
        None if result is from a new utterance and should be assigned 
        a new number

        **OUTPUTS**

        *none*
        """
        debug.trace('ResMgrBasic.interpret_dictation', 'about to interpret, initial_buffer=%s' % initial_buffer)
        if debug.trace_is_active('ResMgrBasic.interpret_dictation'):
            debug.trace('ResMgrBasic.interpret_dictation',
                 'called from thread %s' %
                 threading.currentThread().getName())
        app = self.editor()
        interpreter = self.interpreter()
        self.interp_states.append(interpreter.get_state())
        debug.trace('ResMgrBasic.interpret_dictation',
             'last interp state is %s' %
             self.interp_states[-1].formatting_state.__dict__)
        clear_state = not self.states.interp_state_valid(app)
        debug.trace('ResMgrBasic.interpret_dictation',
                    'clear state = %d' % clear_state)
        interpreted = ResMgrStd._std_interp(self, result, app, 
            initial_buffer = initial_buffer, 
            clear_state = clear_state,
            before = self.states.before_interp,
            after = self.states.after_interp)
        debug.trace('ResMgrBasic.interpret_dictation', 
            'storing result and states')
        if utterance_number is None:
            self.store(result, interpreted, initial_buffer = initial_buffer, 
                number = self.next_number)
            self.next_number = self.next_number + 1
        else:
            self.store(result, interpreted, initial_buffer = initial_buffer, 
                number = utterance_number)
        debug.trace('ResMgrBasic.interpret_dictation', 
            'returning')

    def rename_buffer_cbk(self, old_buff_name, new_buff_name):
        """callback which notifies us that the given editor
        instance has renamed a buffer

        **INPUTS**

        *STR* old_buff_name -- old name of the buffer 

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        self.states.rename_buffer_cbk(old_buff_name, new_buff_name)
        for i in range(len(self.initial_buffers)):
            if self.initial_buffer[i] == old_buff_name:
                self.initial_buffer[i] = new_buff_name

    def close_buffer_cbk(self, buff_name):
        """callback which notifies us that a buffer was closed

        **INPUTS**

        *STR* buff_name -- name of the buffer which was closed

        *STR* new_buff_name -- new name of the buffer 

        **OUTPUTS**

        *none*
        """
        self.states.close_buffer_cbk(buff_name)
# just leave initial_buffers alone - StateStackBasic will make sure we
# can't reinterpret those utterances

    def stored_utterances(self):
        """tells how many dictated utterances have been stored

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- number of utterances which can be retrieved with
        recent_dictation
        """
        return len(self.utterances)
        
    def recent_correctable_dictation(self, n = None):
        """returns a list of SpokenUtterance objects that can be reinterpreted.

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

        *INT n* -- the number of utterances to return, or None to return 
        all available utterances.

        **OUTPUTS**

        *[(SpokenUtterance, INT, UtteranceInterpretation)]* -- the n most recent 
        reinterpretable dictation utterances (or all available if < n), sorted most 
        recent last, each with a corresponding identifying number and the result
        of interpreting it.

        The utterance number is unique, within a given editor instance.

        Note:  These utterances should not be stored permanently, nor
        should they be modified except as part of the correction
        process.  Also, the status of whether a given utterance can be
        re-interpreted may change if the user makes other changes to the 
        buffer during correction.        
        """
        utterances = self.recent_dictation(n)
        debug.trace('ResMgrBasic.recent_correctable_dictation', '** utterances=%s' % repr(utterances))
        reinterpretable_utterances = []
        if utterances:
           for an_utter_info in utterances:
              if an_utter_info[2]:
                 debug.trace('recent_correctable_dictation', '** an_utter_info=%s' % repr(an_utter_info))
                 reinterpretable_utterances.append([an_utter_info[0], an_utter_info[1], an_utter_info[3]])
        return reinterpretable_utterances

    def recent_dictation(self, n = None):
        """returns a list of SpokenUtterance objects

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

        *INT n* -- the number of utterances to return, or None to return 
        all available utterances.

        **OUTPUTS**

        *[(SpokenUtterance, INT, BOOL, UtteranceInterpretation)]* -- the n most recent dictation 
        utterances (or all available if < n), sorted most recent last, 
        each with:
          - a corresponding identifying number 
          - a flag indicating if the utterance can be undone and re-interpreted
            (or None if no utterances are stored)
          - results of the interpretation of that utterance

        The utterance number is unique, within a given editor instance.

        Note:  These utterances should not be stored permanently, nor
        should they be modified except as part of the correction
        process.  Also, the status of whether a given utterance can be
        re-interpreted may change if the user makes other changes to the 
        buffer during correction.
        """
        available = self.stored_utterances()
        self.call_count = self.call_count + 1
        call_count = self.call_count
        debug.trace('ResMgrBasic.recent_dictation', 
            'call %d' % call_count)
        debug.trace('ResMgrBasic.recent_dictation', '%s requested' % n)
        debug.trace('ResMgrBasic.recent_dictation', '%d available' % available)
        if available == 0:
            return None
        if n == None:
            m = available
        else:
            m = min(n, available)
        if debug.trace_is_active('ResMgrBasic.recent_dictation'):
            debug.trace('ResMgrBasic.recent_dictation',
                 'called from thread %s' %
                 threading.currentThread().getName())
        safe = self.states.safe_reinterp_depth(self.editor())
        debug.trace('ResMgrBasic.recent_dictation', 
            'call %d' % call_count)
        if debug.trace_is_active('ResMgrBasic.recent_dictation'):
            debug.trace('ResMgrBasic.recent_dictation',
                 'called from thread %s' %
                 threading.currentThread().getName())
        debug.trace('ResMgrBasic.recent_dictation', 'safe depth = %d' % safe)
        utterances = []
        for i in range(m, 0, -1):
            debug.trace('ResMgrBasic.recent_dictation', 
                'adding %s, number = %d, safe = %d' % (self.utterances[-i], self.numbers[-i], i <= safe))
            utterances.append((self.utterances[-i], self.numbers[-i], i <= safe, self.interpreted[-i]))
        return utterances
    
    def recent_symbols(self, n=None):
        """returns a list of the most recently uttered symbols.

        **Note:** additional dictation into the editor will increment
        the indices of specific utterances, so the mediator must not
        allow dictation into the editor between the call to 
        recent_dictation to get the utterances and the call to 
        reinterpret_recent.

        **INPUTS**

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
        utterances = self.recent_correctable_dictation(n)
        debug.trace('ResMgrBasic.recent_symbols', '** utterances=%s' % repr(utterances))
        rec_symbols = []
        for an_utter_info in utterances:
           an_interp_phrase = an_utter_info[2]
           debug.trace('ResMgrBasic.recent_symbols', '** an_utter_info=%s' % repr(an_interp_phrase))
           rec_symbols.extend(an_interp_phrase.symbol_results)
        debug.trace('ResMgrBasic.recent_symbols', '** returning rec_symbols=%s' % repr(rec_symbols))
        return rec_symbols
           
    
    def scratch_recent(self, n = 1):
        """undo the effect of the most recent n utterances, if possible.

        **INPUTS**

        *INT n* -- number of utterances to undo

        **OUTPUTS**

        *INT* -- number of utterances actually undone
        """
        debug.trace('ResMgrBasic.scratch_recent', 
            'attempting to scratch n = %d' % n)
        safe = self.states.safe_depth(self.editor())
        if safe < 1:
            return 0
        m = min(safe, n)
        app = self.editor()
        success = self.states.pop(app, m)
        if success:
            self.remove_symbols(m)
            del self.utterances[-m:]
            del self.interpreted[-m:]
            del self.initial_buffers[-m:]
            del self.numbers[-m:]
            debug.trace('ResMgrBasic.scratch_recent',
                 'last interp state was %s' % \
                 self.interp_states[-1].formatting_state.__dict__)
# because interp_states are stored before utterances, we want to pop one
# fewer
            if m > 1:
                del self.interp_states[- (m - 1):]
            if self.interp_states:
                debug.trace('ResMgrBasic.scratch_recent',
                     'last interp state is %s' % \
                     self.interp_states[-1].formatting_state.__dict__)
                self.interpreter().restore_state(self.interp_states[-1])
            return m
        return 0
    
    def remove_symbols(self, n = 1):
        """remove any tentative symbols added in the last n utterances

        **INPUTS**

        *INT n* -- number of utterances being undone

        **OUTPUTS**

        *none*
        """
        debug.trace('ResMgrBasic.remove_symbols', 
            'removing symbols from the last %d utterances' % n)
        interpreter = self.interpreter()
        for i in range(1, n+1):
            interpreted = self.interpreted[-i]
            for symbol in interpreted.symbols():
                native = symbol.native_symbol()
                debug.trace('ResMgrBasic.remove_symbols', 
                    'removing symbol %s' % native)
                interpreter.remove_symbol_if_tentative(native)

    def reinterpret_recent(self, changed, delete_tentative_syms = 1):
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
        
        possible = []
        i_possible = []
        for j in changed:
            i = self.find_utterance(j)
            if not (i is None):
                possible.append(j)
                i_possible.append(i)
        n = max(i_possible)
        debug.trace('ResMgrBasic.reinterpret_recent', 
            'max changed = %d' % n)
        app = self.editor()
        n = min(n, self.stored_utterances())
        debug.trace('ResMgrBasic.reinterpret_recent', 
            'max changed and stored = %d\n' % n)
        m = self.states.safe_reinterp_depth(self.editor())
        debug.trace('ResMgrBasic.reinterpret_recent', 
            'safe depth = %d' % m)
        m = min(m, n)
        debug.trace('ResMgrBasic.reinterpret_recent', 
            'so popping %d' % m)
        interpreter = self.interpreter()
# for any utterances which were changed but can't be reinterpreted, we
# should still remove symbols which no longer appear in them (just like
# we adapt the speech engine based on those corrections)
        if delete_tentative_syms: 
            for i in range(m+1, n+1):
                if i in i_possible:

                    symbols = self.interpreted[-i].symbols()
                    utterance = self.utterances[-i]
                    new_spoken_forms = utterance.spoken_forms()
                    spoken = string.join(new_spoken_forms)
                    for symbol in symbols:
                        spoken_symbol = string.join(symbol.spoken_phrase())
                        native = symbol.native_symbol()
                        if spoken.find(spoken_symbol) == -1:

                            interpreter.remove_symbol_if_tentative(native)
# with ResMgrBasic, we must undo all utterances back to the first one to
# be reinterpreted
        if not self.states.pop(app, m) and delete_tentative_syms:
# for any utterances which were changed but can't be reinterpreted, we
# should still remove symbols which no longer appear in them (just like
# we adapt the speech engine based on those corrections)
            for i in range(1, m+1):
                if i in i_possible:
                    symbols = self.interpreted[-i].symbols()
                    utterance = self.utterances[-i]
                    new_spoken_forms = utterance.spoken_forms()
                    spoken = string.join(new_spoken_forms)
                    for symbol in symbols:
                        spoken_symbol = string.join(symbol.spoken_phrase())
                        native = symbol.native_symbol()
                        if spoken.find(spoken_symbol) == -1:
                            interpreter.remove_symbol_if_tentative(native)
            return None
# and then reinterpret all those utterances.
# First, pop information about those utterances off the top of the stack
        
        if delete_tentative_syms: 
           self.remove_symbols(m)
        to_do = self.utterances[-m:]
        buffers = self.initial_buffers[-m:]
        numbers = self.numbers[-m:]
        del self.utterances[-m:]
        del self.interpreted[-m:]
        del self.initial_buffers[-m:]
        del self.numbers[-m:]
# because interp_states are stored before utterances, we want to pop one
# fewer
        if m > 1:
            del self.interp_states[- (m - 1):]
        if self.interp_states:
            debug.trace('ResMgrBasic.reinterpret_recent',
                        'restoring state')
            interpreter.restore_state(self.interp_states[-1])
# we're going to reinterpret the utterance, which will push a new copy
# of this state, so we need to pop the old copy to avoid duplication
            del self.interp_states[-1]
        debug.trace('ResMgrBasic.reinterpret_recent', 
            'about to reinterpret %s' % repr(to_do))
        for i in range(len(to_do)):
            debug.trace('ResMgrBasic.reinterpret_recent', 
                'reinterpreting %d' % (m - i))
            utterance = to_do[i]
            debug.trace('ResMgrBasic.reinterpret_recent', 
                'spoken_forms %s' % repr(utterance.spoken_forms()))
            self.interpret_dictation(utterance,
                initial_buffer = buffers[i], 
                utterance_number = numbers[i])
# interpret_dictation will place the information back on the stack
        return range(m, 0, -1)

    def can_reinterpret(self, n):
        """can we safely reinterpret the nth most recent utterance

        **INPUTS**

        *INT n* -- the depth in the editor state stack of the utterance
        to be reinterpreted

        **OUTPUTS**

        *BOOL* -- true if we can safely reinterpret that utterance
        """
        app = self.editor()
        if n <= self.stored_utterances() and \
            n <= self.states.safe_reinterp_depth(app):
            return 1
        return 0

    def find_utterance(self, utterance_number):
        """find current position within the stack of an utterance 
        with a given utterance number

        **INPUTS**

        *INT utterance_number* -- the number assigned to the utterance by
        interpret_dictation

        **OUTPUTS**

        *INT* -- depth of this utterance in the stack, or None if not
        found
        """
        debug.trace('ResMgr.find_utterance', 'utterance_number=%s, self.numbers=%s' % 
                                             (utterance_number, repr(self.numbers)))
        for i in range(1, len(self.numbers)+1):
            if utterance_number == self.numbers[-i]:
                return i
        return None

    def correct_utterance(self, utterance_number):
        """initiate user correction of the utterance with a given
        utterance number

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
        n = self.find_utterance(utterance_number)
        if n is None:
            return
        self.correct_nth_synchronous(n)

    def correct_nth_synchronous(self, n = 1):
        """initiate user correction of the most recent dictation utterance 
        into the given editor, if possible

        **INPUTS**

        *INT n* -- the depth in the editor state stack of the utterance
        to be corrected

        **OUTPUTS**

        *none*
        """
        console = self.console()
        if n > self.stored_utterances():
            return
        number = self.numbers[-n]
        utterance = self.utterances[-n]
        symbols = self.interpreted[-n].symbols()
        can_reinterpret = self.can_reinterpret(n)
        interpreter = self.interpreter()
        if console.correct_utterance(self.name, utterance, 
            can_reinterpret, should_adapt = 1):
            reinterpreted = self.reinterpret_recent(changed = [number])
# if we could not reinterpret the utterance, we should still remove any
# tentative symbols no longer present in the phrase
            if not (n in reinterpreted):
                new_spoken_forms = self.utterances[-n].spoken_forms()
                spoken = string.join(new_spoken_forms)
                for symbol in symbols:
                    spoken_symbol = string.join(symbol.spoken_phrase())
                    native = symbol.native_symbol()
                    if spoken.find(spoken_symbol) == -1:
                        interpreter.remove_symbol_if_tentative(native)

            
            
#            nn = self.find_utterance(number)
#            if nn is not None:
#                self.reinterpret_recent(changed = [nn])
    
    def correct_nth(self, n = 1):
        """initiate user correction of the most recent dictation utterance 
        into the given editor, if possible

        **INPUTS**

        *INT n* -- the depth in the editor state stack of the utterance
        to be corrected

        **OUTPUTS**

        *none*
        """
        if n > self.stored_utterances():
            return
        number = self.numbers[-n]
        console = self.console()
        if console.already_modal():
            console.dismiss_modal()
        self.correct_evt.notify(self.name, number)

    def correct_recent(self):
        """initiate user correction of one or more recent dictation 
        utterances into the given editor, if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        console = self.console()
        if console.already_modal():
            console.dismiss_modal()
        self.correct_recent_evt.notify(self.name)
    
    def correct_recent_synchronous(self):
        """initiate user correction of one or more recent dictation 
        utterances into the given editor, if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        console = self.console()
        utterances = self.recent_dictation()
        interpreted = self.interpreted[:]
        if utterances:
            i_changed = console.correct_recent(self.name, utterances)
            print "phrases changed were: ", i_changed
            if i_changed:
                changed = []
                for i in i_changed:
                    changed.append(utterances[-i][1])
                print "corresponding utterance numbers were: ", changed
                reinterpreted = self.reinterpret_recent(changed)
                interpreter = self.interpreter()
                for n in i_changed:
                    if not (n in reinterpreted):
                        symbols = interpreted[-n].symbols()
                        phrase = interpreted[-n].phrase()
                        spoken = string.join(phrase)
                        for symbol in symbols:
                            spoken_symbol = string.join(symbol.spoken_phrase())
                            native = symbol.native_symbol()
                            if spoken.find(spoken_symbol) == -1:
                                interpreter.remove_symbol_if_tentative(native)

    
    def correct_last(self):
        """initiate user correction of the most recent dictation utterance 
        into the given editor, if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.correct_nth()

    def reformat_recent_synchronous(self):
        """initiate user reformatting of one or more recent symbol 
        uttered into the given editor, if possible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace("ResMgrBasicreformat_recent_synchronous", "invoked")
        console = self.console()
        symbols = self.recent_symbols()
        if symbols:
           reformatted_symbols = console.reformat_recent(self.name, symbols)
           self.console().correct_symbol_results(self.name, reformatted_symbols)
        


    def reformat_recent(self):
        console = self.console()
        if console.already_modal():
            console.dismiss_modal()
        self.reformat_recent_evt.notify(self.name)


class ResMgrFactory(Object):
    """abstract class defining the interface for a factory to create
    new ResMgr objects

    """
    def __init__(self, **args):
        self.deep_construct(ResMgrFactory, {}, args)

    def new_manager(self, recog_mgr, instance_name):
        """returns a new ResMgr for the named instance

        **INPUTS**

        *RecogStartMgr recog_mgr* -- the parent recognition starting
        manager

        *STR instance_name* -- the name of the instance whose results
        will be managed
        """
        debug.virtual('ResMgrFactory.new_manager')

class ResMgrStdFactory(ResMgrFactory):
    """factory creating ResMgrStd objects
    """
    def __init__(self, **args):
        self.deep_construct(ResMgrStdFactory, {}, args)

    def new_manager(self, recog_mgr, instance_name):
        """returns a new ResMgr for the named instance

        **INPUTS**

        *RecogStartMgr recog_mgr* -- the parent recognition starting
        manager

        *STR instance_name* -- the name of the instance whose results
        will be managed
        """
        return ResMgrStd(recog_mgr = recog_mgr, instance_name = instance_name)

class ResMgrBasicFactory(ResMgrFactory):
    """factory creating ResMgrBasic objects

    **INSTANCE ATTRIBUTES**

    *INT max_utterances* -- the maximum number of recent dictation 
    utterances each ResMgrBasic should store

    *CorrectUtteranceEvent correct_evt* -- doorbell used to send an 
    event to bring up a correction box asynchronously

    *CorrectRecentEvent correct_recent_evt* -- doorbell used to send an 
    event to bring up a correct recent box asynchronously
    
    *ReformatSymbolEvent reformat_recent_evt* -- doorbell used to send an 
    event to bring up a reformat recent box asynchronously
    """
    def __init__(self, correct_evt, correct_recent_evt, reformat_recent_evt,
        max_utterances = 30, **args):
        """
        *INT max_utterances* -- the maximum number of recent dictation 
        utterances each ResMgrBasic should store
        """
        self.deep_construct(ResMgrBasicFactory, 
                            {'correct_evt': correct_evt,
                             'correct_recent_evt': correct_recent_evt,
                             'reformat_recent_evt': reformat_recent_evt,
                             'max_utterances': max_utterances}, 
                            args)

    def new_manager(self, recog_mgr, instance_name):
        """returns a new ResMgr for the named instance

        **INPUTS**

        *RecogStartMgr recog_mgr* -- the parent recognition starting
        manager

        *STR instance_name* -- the name of the instance whose results
        will be managed
        """
        return ResMgrBasic(recog_mgr = recog_mgr, 
            instance_name = instance_name, 
            correct_evt = self.correct_evt,
            correct_recent_evt = self.correct_recent_evt,
            reformat_recent_evt = self.reformat_recent_evt,
            max_utterances = self.max_utterances)

   
# defaults for vim - otherwise ignore
# vim:sw=4

