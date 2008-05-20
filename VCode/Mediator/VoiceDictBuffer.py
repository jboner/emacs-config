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

"""interfaces for editor buffers with change notification"""


import debug
import sys
from Object import Object
import TextBuffer
import natlink
from natlinkutils import *


class VoiceDictBuffer(TextBuffer.TextBufferChangeSpecify, 
    TextBuffer.SpeechBufferRecogStart,
    TextBuffer.LockableSpeechBuffer, TextBuffer.SelectionBuffer):
    """concrete implementation of TextBufferChangeSpecify, 
    SpeechBufferRecogStart, SelectionBuffer, and LockableSpeechBuffer,
    using the DictObj class from Joel Gould's
    natlink python interface to NaturallySpeaking.

    **INSTANCE ATTRIBUTES**

    *DictObj* underlying -- underlying Natlink DictObj (exported from C)

    *BOOL* was_activated -- has underlying DictObj been activated
    *BOOL* activated -- is underlying DictObj currently activated
    *BOOL* dict_globally -- is DictObj activated globally
    *BOOL* window_handle -- handle of window for window-specific
    activation (or 0 if global or unset)
    *BOOL* program_initiated -- flag which indicates whether a
    change was initiated by the program (i.e. by a call to set_text) or
    by voice
    
    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, **args):
        """
        
        **INPUTS**
        
        *none*
        """
        self.deep_construct(VoiceDictBuffer,
                            {'underlying': natlink.DictObj(),
                            'was_activated': 0,
                            'activated': 0,
                            'dict_globally': 0,
                            'program_initiated': 0,
                            'window_handle': 0},
                            args)
    
        self.underlying.setBeginCallback(self._on_begin)
        self.underlying.setChangeCallback(self._on_voice_change)

    def __del__(self):
        self.underlying.deactivate()
        self.underlying.setChangeCallback(None)
        self.underlying.setBeginCallback(None)
#        print 'dying'
        self.underlying = None

    def _on_begin(self, app_and_window):
        """private method.  Should only be called by callback from
        underlying DictObj.

        **INPUTS**

        *(STR, STR, INT)* app_and_window --
        full path to module of active application, title of active
        window, window handle of active window
        """
        print '_on_begin', app_and_window
        sys.stdout.flush()
        match = 1
        if self.window_handle and \
            self.window_handle != app_and_window[2]:
            match = 0
            
#        print '_on_begin'
#        self.set_lock(1)
        if match:
            self._on_recog_start(match)
#        self.set_lock(0)

    def _on_voice_change(self, start, end, text, selection_start,
        selection_end):
        """private method.  Should only be called by callback from
        underlying DictObj.

        **INPUTS**

        *INT* start, *INT* end --
        character range of region replaced or deleted of region.  
        (start == end means text was simply inserted

        *STR* text -- inserted text

        *INT* selection_start, *INT* selection_end --
        the selection range after insertion
        """
        self.set_lock(1)
#        print 'VDB._on_voice_change - %d %d %s %d %d %d' % \
#          (start, end, text, selection_start,
#            selection_end, self.program_initiated)
        self._on_change_specification(start, end, text, selection_start,
            selection_end, self.program_initiated)
        self.set_lock(0)

    def get_length(self):
        return self.underlying.getLength()

    def range_defaults(self, start = None, end = None):
        """translates from TextBuffer defaults for specifying start and
        end of a range to the appropriate values for DictObj
        
        **INPUTS**
        
        *INT* start -- offset of start of range, or None to
        default to the beginning of the buffer

        *INT* end -- offset of character following end of 
        range, or None to default to the end of the buffer

        **OUTPUTS**

        *(INT, INT)* -- DictObj offsets
        
        """

# note: this uses internal positions
        if (start == None):
          s = 0
        else:
          s = start
        if (end == None):
          e = self.get_length()
        else:
          e = end
        return s, e

    def set_text(self, text, start = None, end = None):
        """changes a portion of the buffer

        **INPUTS**

        *STR text* is the new text.
        
        *INT start* is the offset into the buffer of the text to the
        replaced.  Defaults to start of buffer.

        *INT end* is the offset into the buffer of the character following 
        the text to be replaced (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *none*
        """
        s, e = self.range_defaults(start, end)
#        print 'VDB.set_text ', s, e, text
        self.program_initiated = 1
        self.underlying.setText(text, s, e)
# DictObj, unlike CDgnDictCustom, appears to send change events only on
# internally initiated changes, so we have to call
# _on_change_specification manually
        selection_start, selection_end = self.get_selection()
        self._on_change_specification(s, e, text, selection_start,
            selection_end, self.program_initiated)
        self.program_initiated = 0

    def get_text(self, start = None, end = None):
        """retrieves a portion of the buffer

        **INPUTS**

        *INT start* is the start of the region returned.
        Defaults to start of buffer.

        *INT end* is the offset into the buffer of the character following 
        the region to be returned (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *STR* -- contents of specified range of the buffer
        """
        s, e = self.range_defaults(start, end)
        return self.underlying.getText(s, e)

    def get_selection(self):
        """retrieves range of current selection

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *INT* (start, end)

        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        return self.underlying.getTextSel()
      
    def cur_pos(self):
        """returns current position (= end of the current selection)

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the offset into the buffer of the current cursor
        position.
        """
        return self.get_selection()[1]

    def set_selection(self, start = None, end = None):
        """changes range of current selection

        **INPUTS**

        *INT start* is the start of the region to be selected.
        Defaults to start of buffer.

        *INT end* is the offset into the buffer of the character following 
        the region to be selected (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *none*
        """
        s, e = self.range_defaults(start, end)
#        print 'vdb.set_selection ', s, e
        self.underlying.setTextSel(s, e)

    def activate(self, dict_globally = 0):
        """activates the speech buffer for dictation, either globally or
        tied to the current window.

        **INPUTS**

        *BOOL* dict_globally -- if true, activate globally, otherwise
        note the current window and activate only when it is the active window.
        Additional conditions can be placed on the activation by
        activating or deactivating manually or on recognition starting
        (see SpeechBufferRecogStart below)

        **OUTPUTS**

        *none*
        """
#        Note: NaturallySpeaking's window-specific dictation is 
#        more general, but requires specifying a MS Windows window
#        handle, and I haven't figured out yet how to abstract the window
#        ID to handle other operating systems or speech engines which
#        may specify the window differently, so I figured that the
#        current window would be good enough to start.
#
#        Also note that, for the same reason, the default behavior
#        activate is different from that of NaturallySpeaking (either
#        Natlink DictObj, or SDK CDgnDictCustom)
    
        window_handle = 0
        if (not dict_globally):
            app_and_window = natlink.getCurrentModule()
            window_handle = app_and_window[2]
        self.window_handle = window_handle 
        self.dict_globally = dict_globally
        self.underlying.activate(window_handle)
        self.was_activated = 1
        self.activated = 1
    
    def deactivate(self):
        """disable dictation into the SpeechBuffer

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.underlying.deactivate()
        self.activated = 0

    def reactivate(self):
        """reactivate dictation using the same window (or globally).
        This method should not be called, unless the buffer has
        previously been activated and then deactivate.
        
        (for VoiceDictBuffer, if never activated, this is equivalent 
        to activate with current window.)

        **INPUTS**

        *none*
    
        **OUTPUTS**

        *none*
        """
        if not self.activated:
            if self.was_activated:
                self.underlying.activate(self.window_handle)
                self.activated = 1
            else:
                self.activate(dict_globally = 0)

    def has_been_activated(self):
        """indicates whether the activate method has been invoked, or
        whether it needs to be called to activate dictation.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- returns true if activate was previously called, and
        the SpeechBuffer is still active, or can be reactivated with a
        call to reactivate (instead of a new call to activate)."""

        # note: if the activation condition (specific window, or globally
        # active) is lost when deactivate is called, has_been_activated
        # should return false
        return self.was_activated

    def is_active(self):
        """indicates whether dictation into the SpeechBuffer is currently 
        active (activated globally, or activated with the current
        window)

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- returns true iff dictation into the buffer is 
        currently active.
        """
        if self.is_activated():
            if is_global():
                return 1
            app_and_window = natlink.getCurrentModule()
            if app_and_window[2] == self.window_handle:
                return 1
        return 0

    def is_activated(self):
        """indicates whether the  SpeechBuffer is currently activated or
        deactivated (not whether it is active)

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- returns true iff the buffer is currently activated.
        Note that if the buffer was activated for a specific window,
        but that window is not active, is_activated will still return
        true.  To see if dictation is active now, use is_active.
        """
        return self.activated

    def is_global(self):
        """tells whether the buffer (when activated) is activated
        globally.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is buffer set for global dictation.
        """
        return self.dict_globally

    def set_lock(self, state):
        """locks/unlocks changes to the contents of a hidden speech 
        buffer, to ensure consistency between multiple get operations.
        When the buffer is
        locked, all speech-initiated changes to the buffer will be 
        deferred until it is unlocked.  No speech should be lost.
        
        **INPUTS**

        *INT* state

        **OUTPUTS**

        *none*
        """
        self.underlying.setLock(state)

    def set_visible(self, range = (0, -1)):
        """tells the SpeechBufferSelection the current visible range
        which should be available to Select XYZ.
        
        **INPUTS**

        *(INT, INT)* range --  character range which the buffer should
        is visible and therefore selectable.  If the end of the range is
        less than the start (as in the default), the entire buffer will 
        be considered visible.  Select xyz can be disabled by setting
        the range to (0, 0)
        
        **OUTPUTS**

        *none*
        """        
        start, end = range
        if end < start:
            self.underlying.setVisibleText(0)
        else:
            self.underlying.setVisibleText(start, end)

    def get_visible(self):
        """returns the current visible range
        which should be available to Select XYZ.  If ta concrete
        subclass of SpeechBufferSelection does not support returning the
        current visible range, then get_visible should return None.
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *(INT, INT)* range --  visible character range.  None means ehat
        the concrete implementation of SpeechBufferSelection does not
        support reporting the current visible range.  An empty range
        means that Select xyz is disabled.  A range of (0, -1) means the
        entire buffer is visible.  

        *none*
        """
        start, end = self.underlying.getVisibleText()
        if end > self.get_length():
            end = -1
        return start, end

