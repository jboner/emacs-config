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

"""interfaces for editor buffers with change notification.

TextBuffer is an abstract interface which is used
as a wrapper around various text/edit controls, usually either visible 
(GUI) text controls or hidden (speech) edit controls like the VDct.
 
It is a fairly simple, lightweight interface.  You can find the length
of the buffer.  You can set/get the text.  You can set/get the
selection.  That's about it.
 
Various subclasses add additional functionality, such as notification or
specification of changes.  Also, there are other abstract add-in classes
which add specific functionality (get_visible for VisibleBuffer,
activate for SpeechBuffer, set_visible for SelectionBuffers).  In order
to allow the user to mix and match these additional functionality
without running into problems with multiple inheritance due to
ambiguity, these add-in classes are not derived from TextBuffer,
although it doesn't make much sense to create a concrete class which
inherits from VisibleBuffer and not from TextBuffer (or a subclass of
TextBuffer).
 
Then there are concrete subclasses, like TextBufferWX which implements
TextBufferChangeSpecify and VisibleBuffer using a wxTextCtrl, or
VoiceDictBuffer which implements TextBufferChangeSpecify,
SpeechBufferRecogStart, LockableSpeechBuffer, and SelectionBuffer using the
natlink.DictObj."""


import debug
import find_difference
import string
from Object import Object

class TextBuffer(Object):
    """abstract class defining basic text buffer interface.

    **INSTANCE ATTRIBUTES**

    *none*
    
    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, **args):
        """abstract base class - no arguments
        
        **INPUTS**
        
        *none*
        """
        self.deep_construct(TextBuffer,
                            {},
                            args)

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
        debug.virtual('TextBuffer.set_text')

    def get_length(self):
        """returns the length of the buffer

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* number of characters in the buffer"""
        debug.virtual('TextBuffer.get_length')
         
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
        debug.virtual('TextBuffer.get_text')

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
        debug.virtual('TextBuffer.get_selection')

    def len(self):
        """returns length of buffer 

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the length of the buffer
        """
        debug.virtual('TextBuffer.len')


    def cur_pos(self):
        """returns current position (either the start or end of
        the current selection, and usually the end)

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the offset into the buffer of the current cursor
        position.
        """
        debug.virtual('TextBuffer.cur_pos')

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
        debug.virtual('TextBuffer.set_selection')

class SpeechBuffer:
    """abstract base class describing additional interfaces for a
    (hidden) speech buffers, used to keep track of dictated text and
    context.

    Note: Generally, a concrete class will inherit from TextBuffer (or a
    subclass) and often, either VisibleBuffer or SpeechBuffer.  
    SpeechBuffer does not inherit from TextBuffer so as to simplify
    such mix-and-match multiple inheritance.

    **INSTANCE ATTRIBUTES**

    *none*
    
    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, **args):
        self.deep_construct(SpeechBuffer,
                            {},
                            args)

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
        debug.virtual('SpeechBuffer.activate')
    
    def deactivate(self):
        """disable dictation into the SpeechBuffer

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('SpeechBuffer.deactivate')

    def reactivate(self):
        """reactivate dictation using the same window (or globally).
        This method should not be called, unless the buffer has
        previously been activated and then deactivate.
        
        Otherwise, its effect is undefined (although the concrete
        subclass of SpeechBuffer should still attempt to do something
        sensible)

        **INPUTS**

        *none*
    
        **OUTPUTS**

        *none*
        """
        debug.virtual('SpeechBuffer.reactivate')

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
        debug.virtual('SpeechBuffer.has_been_activated')

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
        debug.virtual('SpeechBuffer.is_active')

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
        debug.virtual('SpeechBuffer.is_activated')

    def is_global(self):
        """tells whether the buffer (when activated) is activated
        globally.

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- is buffer set for global dictation.
        """
        debug.virtual('SpeechBuffer.is_global')

class SpeechBufferRecogStart(SpeechBuffer):
    """abstract subclass of SpeechBuffer which adds a recognition
    starting callback interface.

    **INSTANCE ATTRIBUTES**

    *FCT* recog_start_callback --
        recog_start_callback( *SpeechBuffer* buffer, *BOOL*
        window_matches)
    callback which indicates the start of recognition.  This callback
    can activate or deactivate the SpeechBuffer (allowing more specific
    activation conditions than just the active window).  If necessary
    it should also update the state of the internal SpeechBuffer to match the
    corresponding editor buffer.  (This is necessary if the
    SpeechBuffer is not immediately updated whenever the editor buffer 
    changes).
  
    If the SpeechBuffer is set to be active only when a specific
    window is active, window_matches will return true only if this
    is the case.
    
    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, recog_start_callback = None, **args):
        """abstract class initialization, but does common handling of
        the recog_start_callback

        **INPUTS**

        *FCT* recog_start_callback -- recog_start_callback(
        *SpeechBuffer* buffer, *BOOL* window_matches) 
        callback which indicates the start of
        recognition.  
        
        This callback can activate or deactivate the
        SpeechBuffer (allowing more specific activation conditions than
        just the active window).  

        If necessary it should also update the
        state of the internal SpeechBuffer to match the corresponding
        editor buffer.  (This is necessary if the SpeechBuffer is not
        immediately updated whenever the editor buffer changes).

        If the SpeechBuffer is set to be active only when a specific
        window is active, window_matches will return true only if this
        is the case.

        **OUTPUTS**

        *none*
        """
        self.deep_construct(SpeechBufferRecogStart,
                            {'recog_start_callback':
                            recog_start_callback},
                            args)

    
    def set_recog_start_callback(self, recog_start_callback = None):
        """changes the callback to a new function

        **INPUTS**

        *FCT* recog_start_callback -- recog_start_callback(
        *SpeechBuffer* buffer, *BOOL* window_matches) 
        callback which indicates the start of
        recognition.  This callback can activate or deactivate the
        SpeechBuffer (allowing more specific activation conditions than
        just the active window).  If necessary it should also update the
        state of the internal SpeechBuffer to match the corresponding
        editor buffer.  (This is necessary if the SpeechBuffer is not
        immediately updated whenever the editor buffer changes).

        If the SpeechBuffer is set to be active only when a specific
        window is active, window_matches will return true only if this
        is the case.

        **OUTPUTS**

        *none*
        """
        self.recog_start_callback = recog_start_callback

    def _on_recog_start(self, window_matches):
        """internal function which triggers the
        recog_start_callback.  Only the concrete subclass of
        SpeechBufferRecogStart implementing the change notification 
        should call this function"""
        if self.recog_start_callback:
            self.recog_start_callback(self, window_matches)
            # note: recog_start_callback is an attribute of 
            # SpeechBufferRecogStart, which is a function, not a method of
            # SpeechBufferRecogStart.  This looks a bit funny (like a
            # method being called with a duplicate self argument) but it
            # is actually correct.

class LockableSpeechBuffer:
    """adds a locking interface to a SpeechBuffer
    **INSTANCE ATTRIBUTES**

    *none*
    
    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, **args):
        self.deep_construct(LockableSpeechBuffer,
                            {},
                            args)
    
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
        pass

class SelectionBuffer:
    """abstract base class describing additional interfaces for a
    (hidden) speech buffers with Select xyz

    Note: Generally, a concrete class will inherit from TextBuffer (or a
    subclass), SpeechBuffer (or a subclass) and SelectionBuffer.  
    In order to facilitate mixing and matching different subclasses of
    TextBuffer and SpeechBuffer, with or without SelectionBuffer,
    SelectionBuffer does not inherit from either, although deriving a
    concrete class from SpeechBuffer without the TextBuffer interface
    for setting and reading the buffer contents doesn't make much sense.

    **INSTANCE ATTRIBUTES**

    *none*
    
    **CLASS ATTRIBUTES**
    
    *none*
    """
    def __init__(self, **args):
        self.deep_construct(SelectionBuffer,
                            {},
                            args)


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
        debug.virtual('SelectionBuffer.set_visible')

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
        debug.virtual('SelectionBuffer.get_visible')

    
class VisibleBuffer:
    """abstract base class describing additional interfaces for visible
    text buffers.

    Note: Generally, a concrete class will inherit from TextBuffer (or a
    subclass) and often, either VisibleBuffer or SpeechBuffer.  
    VisibleBuffer does not inherit from TextBuffer so as to simplify
    such mix-and-match multiple inheritance.

    **INSTANCE ATTRIBUTES**

    *none*
    
    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, **args):
        self.deep_construct(VisibleBuffer,
                            {},
                            args)
        

    def get_visible(self):
        """ get start and end offsets of the currently visible region of
        the buffer.  End is the offset of the first character not
        visible (matching Python's slice convention)

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* (start, end)
        """
        debug.virtual('VisibleBuffer.get_visible')

    def make_position_visible(self, position = None):
        """scroll buffer (if necessary) so that  the specified position
        is visible.  Position defaults to the current cursor position.
        Note: if a particular subclass of VisibleBuffer cannot support
        this method, it should just leave it as a no-op.

        **INPUTS**

        *INT* position

        **OUTPUTS**

        *none*
        """
        pass

    def refresh(self):
        """force a refresh of the buffer.
        Note: if a particular subclass of VisibleBuffer cannot support
        this method, it should just leave it as a no-op.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        pass
      
class StoreableTextBuffer(Object):
    """abstract base class for loading and saving a text buffer, and for
    keeping track of whether the buffer has been modified and needs to
    be saved.  Note: this is a fairly low-level interface (so that it
    can easily be supported by as many buffers as possible), so it
    doesn't allow for fancy features like prompting before overwriting a
    file.

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(StoreableTextBuffer, {}, args)
    
    def modified(self):
        """has the buffer been modified since the last time it was
        saved?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the buffer has been modified since the last
        save (or load)
        """
        debug.virtual('StoreableTextBuffer.modified')

    def save_file(self, f_path):
        """save the buffer to a file

        **INPUTS**

        *STR f_path* -- full path of the file

        **OUTPUTS**

        *BOOL* -- true if the file was saved successfully
        """
        debug.virtual('StoreableTextBuffer.save_file')

    def load_file(self, f_path):
        """load the buffer from a file (erasing the current contents)

        **INPUTS**

        *STR f_path* -- full path of the file

        **OUTPUTS**

        *BOOL* -- true if the file was loaded successfully
        """
        debug.virtual('StoreableTextBuffer.load_file')




class NumberedLines(Object):
    """abstract base class describing additional interfaces for moving
    by line.

    Note: Generally, a concrete class will inherit from TextBuffer (or a
    subclass) and NumberedLines.
    NumberedLines does not inherit from TextBuffer so as to simplify
    such mix-and-match multiple inheritance.

    **INSTANCE ATTRIBUTES**

    *none*
    
    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, **args):
        """abstract base class - no arguments
        
        **INPUTS**
        
        *none*
        """
        self.deep_construct(NumberedLines,
                            {},
                            args)

    def line_num_of( self, pos = None):
        """find line number of position pos

        **INPUTS**

        *INT pos* -- the offset into the buffer of the desired position. 
         Defaults to the current position.

        **OUTPUTS**

        *INT* -- corresponding line number (starting with 0)
        """
        debug.virtual('NumberedLines.line_num_of')
      
    def line_nums_of_range(self, range = None):
        """find line numbers of a range of positions

        **INPUTS**

        *(INT, INT) range* -- range of character offsets into the buffer. 
         Defaults to the current selection.

        **OUTPUTS**

        *(INT, INT)* -- corresponding pair of line numbers (starting with 0)
        """

        debug.virtual('NumberedLines.line_nums_of_range')

    def lines(self):
        """return number of lines in the buffer
        
        **INPUTS**
        
        *none*
        
        **OUTPUT**
        
        *int* -- number of lines in the buffer (incomplete lines are
        counted, so this is always > 0
        """
        debug.virtual('NumberedLines.lines')

    def range_of_line(self, line = None):
        """returns the character range corresponding to the specified line 
        (not including the newline)

        **INPUTS**

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is out of range, last line is used.

        **OUTPUTS**

        *(INT, INT)* -- offsets into the buffer of the start and end of
        the line.
        """
        debug.virtual('NumberedLines.range_of_line')

    def range_of_lines(self, first_line, last_line):
        """returns the character range corresponding to the specified range
        of lines (not including the final newline)

        **INPUTS**

        *INT first_line, second_line* -- line numbers (starting with 0)

        **OUTPUTS**

        *(INT, INT)* -- offsets into the buffer of the start and end of
        the range of lines.
        """
        debug.virtual('NumberedLines.range_of_lines')
    
    def ranges_of_lines(self, first_line, last_line):
        """returns a list of the character ranges corresponding to the 
        specified range of lines (not including the final newline of
        each line)

        **INPUTS**

        *INT first_line, second_line* -- line numbers (starting with 0)

        **OUTPUTS**

        *[(INT, INT), ...]* -- offsets into the buffer of the start and end of
        the each line in the range of lines.
        """
        debug.virtual('NumberedLines.ranges_of_lines')

    def get_line(self, line = None):
        """returns the contents of the specified line 
        (not including the newline)
        **INPUTS**

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is out of range, last line is used.

        **OUTPUTS**

        *(INT, INT)* -- offsets into the buffer of the start and end of
        the line.
        """
        start, end = self.range_of_line(line)
        return self.get_text(start, end)

    def position_of_line(self, line = None, where = -1):
        """returns the position of the start or end of the specified line 

        **INPUTS**

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is out of range, returns position of end of buffer.

        *INT where* indicates whether the position of the end
         (*where > 0*) or at the beginning (*where < 0*) of the line
         should be returned.

        **OUTPUTS**

        *INT* -- position of start/end of that line.
        """
        debug.virtual('NumberedLines.position_of_line')

    def line_length(self, line = None):
        """returns the length of the specified line

        **INPUTS**

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is out of range, returns None.

        **OUTPUTS**

        *INT* -- length of start of that line.
        """

        debug.virtual('NumberedLines.line_length')

    def goto_line(self, line = None, where = -1):
        """Go to a particular line in a buffer.

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is greater than the number of lines, goes to the end of
        the buffer.

        *INT where* indicates if the cursor should go at the end
         (*where > 0*) or at the beginning (*where < 0*) of the line.

        **OUTPUTS**

        *none*
        """

        debug.virtual('NumberedLines.goto_line')

class TextBufferChangeNotify(TextBuffer):
    """abstract class wrapper for text buffers with change notification,
    but not change specification (tell you when something changed, but
    not what).  Both Windows CEdit controls and wxWindows TextCtrl's share
    this stupid characteristic.  However, we can convert such a buffer
    into a more civilized form by wrapping it in a TextBufferChangeNotify, 
    and then passing it to TextBufferSpecifyFromNotify.

    Note: TextBufferChangeNotify provides the infrastructure for change
    notification, but a concrete subclass of TextBufferChangeNotify is
    responsible for calling TextBufferChangeNotify._on_change with the
    proper value of the program_initiated flag to
    initiate the processing of the change event, for both user- and
    program-initiated changes.  See TextBufferWX for an example of Hal
    how to do this.

    **INSTANCE ATTRIBUTES**

    *FCT* notification_callback --
      notification_callback( *TextBufferChangeNotify* buffer,
    *BOOL* program_initiated).
    function to be called on change to
    the underlying buffer.   program_initiated will be true if the change
    came from the program (through set_text).
    The only other argument passed will be a reference to this buffer,
    so if the callback function wants to know what changed,
    it will have to call other methods of
    TextBufferChangeNotify (or its parent classes).
    Note that the notification callback will be called on ANY change
    to the buffer, whether initiated by the user or by a caller to a
    buffer method, so no changes should be made during the callback.

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, notification_callback=None, **args):
        """

        **INPUTS**

        *FCT* notification_callback --
          notification_callback( *TextBufferChangeNotify* buffer,
        *BOOL* user_initiated) 
        see TextBufferChangeNotify documentation for details.
        """
    
        self.deep_construct(TextBufferChangeNotify,
                            {'notification_callback':notification_callback}, 
                            args)

    def set_change_notification_callback(self, notification_callback = None):
        """changes the callback to a new function

        **INPUTS**

        *FCT* notification_callback --
        notification_callback( *TextBufferChangeNotify* buffer,
        *BOOL* program_initiated)
        function to be called on change to
        the underlying buffer.  
        see TextBufferChangeNotify documentation for details.

        **OUTPUTS**

        *none*
        """
        self.notification_callback = notification_callback

    def _on_change(self, program_initiated):
        """internal function which triggers the
        notification_callback.  Only the concrete subclass of
        TextBufferChangeNotify implementing the change notification 
        should call this function"""
        if self.notification_callback:
            self.notification_callback(self, program_initiated)
            # note: notification_callback is an attribute of 
            # TextBufferChangeNotify, which is a function, not a method of
            # TextBufferChangeNotify.  This looks a bit funny (like a
            # method being called with a duplicate self argument) but it
            # is actually correct.

class TextBufferChangeSpecify(TextBuffer):
    """abstract class defining an interface for text buffers with 
    change specification (i.e. they tell you what changed).
    VDct/CDgnDictCustom meet this criterion.
    TextBufferSpecifyFromNotify below will provide a TextBufferChangeSpecify 
    wrapper for a TextBufferChangeNotify class.

    **INSTANCE ATTRIBUTES**

    *FCT* change_callback --
      change_callback( *INT* start, *INT* end, *STR* text, 
      *INT* selection_start, *INT* selection_end, 
      *TextBufferChangeSpecify* buffer, *BOOL* program_initiated) 
    function to be called on change to
    the underlying buffer.  
    Note that the change callback will be called on ANY change
    to the buffer, whether initiated by the user or by a caller to a
    buffer method, so no changes should be made during the callback.

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, change_callback=None, **args):
        """

        **INPUTS**

        *FCT* change_callback --
        change_callback( *INT* start, *INT* end, *STR* text, 
        *INT* selection_start,
        *INT* selection_end, TextBufferChangeSpecify buffer,
        *BOOL* program_initiated) 
        see TextBufferChangeSpecify documentation for details.
        """
    
        self.deep_construct(TextBufferChangeSpecify,
                            {'change_callback':change_callback}, args)

    def set_change_callback(self, change_callback = None):
        """changes the callback to a new function

        **INPUTS**
        *FCT* change_callback --
        change_callback( *INT* start, *INT* end, *STR* text, 
        *INT* selection_start, *INT* selection_end, 
        *TextBufferChangeSpecify* buffer, *BOOL* program_initiated) 
        see TextBufferChangeSpecify documentation for details.
        -- function to be called on change to
        the underlying buffer.  
        see TextBufferChangeSpecify documentation for details.

        **OUTPUTS**

        *none*
        """
        self.change_callback = change_callback

    def get_change_callback(self):
        """return change callback function (used by wrappers like
        TextBufferCRToNL)

        **INPUTS**

        *none*

        **OUTPUTS**

        *FCT* change_callback --
        change_callback( *INT* start, *INT* end, *STR* text, 
        *INT* selection_start, *INT* selection_end, 
        *TextBufferChangeSpecify* buffer, *BOOL* program_initiated) 
        """
        return self.change_callback
    
    def _on_change_specification(self, start, end, text,
        selection_start, selection_end, program_initiated):
        """internal function which triggers the
        change_callback.  Only the concrete subclass of 
        TextBufferChangeSpecify implementing change specification
        should call this function"""
#        print 'hi there'
#        print repr(self.change_callback)
#        print 'changeSpec._on_change_specification ', start, end, text, \
#        selection_start, selection_end, program_initiated
        if self.change_callback:
            (self.change_callback)(start, end, text, selection_start, 
                selection_end, self, program_initiated)
            # note: change_callback is an attribute of 
            # TextBufferChangeSpecify, which is a function, not a method of
            # TextBufferChangeSpecify.  This looks a bit funny (like a
            # method being called with a duplicate self argument) but it
            # is actually correct.

class TextBufferSpecifyFromNotify(TextBufferChangeSpecify):
    """creates a TextBufferChangeSpecify given an underlying
    TextBufferChangeNotify

    note: TextBufferSpecifyFromNotify uses the __getattr__ method to
    delegate unknown attributes and methods to the underlying
    TextBufferChangeNotify.  Thus, if a specific subclass of
    TextBufferChangeNotify has additional attributes or methods, they
    will be visible through the containing TextBufferSpecifyFromNotify.
    However, since TextBufferSpecifyFromNotify inherits from TextBuffer, any
    attributes added to TextBuffer will not be unknown, and will not be
    automatically delegated.  Like get_text, set_text, etc.,
    corresponding methods must be added manually to
    TextBufferSpecifyFromNotify to ensure that it will properly support
    these new attributes.

    TextBufferSpecifyFromNotify intercepts and disables the 
    set_change_notification_callback method, since it needs to receive
    the change notification callbacks itself.

    **INSTANCE ATTRIBUTES**

    *TextBufferChangeNotify* underlying -- underlying change notification
    TextBuffer

    *STR* contents -- copy of buffer, for detecting differences when we
    are notified that the underlying buffer has changed.

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, change_notification, **args):
        """

        **INPUTS**

        *TextBufferChangeNotify* change_notification
         - underlying buffer to provide change notification and rest of
           services

        [Inherited from TextBufferChangeSpecify: 

        *FCT* change_callback --
        change_callback( *INT* start, *INT* end, *STR* text, 
        *INT* selection_start, *INT* selection_end, 
        *TextBufferChangeSpecify* buffer, *BOOL* program_initiated) 

        see TextBufferChangeSpecify documentation for details.]
        """
    
        self.deep_construct(TextBufferSpecifyFromNotify,
                            {'change_notification':change_notification,
                            'contents': change_notification.get_text()},
                            args)

        self.change_notification.set_change_notification_callback(
            self._on_change_translator)
    
    def _on_change_translator(self, internal, program_initiated):
        """private method which handles change notification callbacks
        from the underlying TextBufferChangeNotify object and translates
        to change specification callbacks.  Should not be called
        manually."""

        contents = self.change_notification.get_text()
        if self.change_callback:
            start, end, text = \
                find_difference.find_string_difference(self.contents, contents)
#            text = self.contents[start: end]
            selection_start, selection_end = \
                self.change_notification.get_selection()
#            start, end, text = 0, 0, ""
            self._on_change_specification(start, end, text,
                selection_start, selection_end, program_initiated)
        self.contents = contents

    def __getattr__(self, name):
        """delegates unknown attributes (including methods) to the underlying
        TextBufferChangeNotify object"""
        return getattr(self.change_notification, name)

# no-op versions of these functions are defined in TextBuffer, so
# __getattr__ will not automatically delegate them to the underlying
# TextBufferChangeNotify

    def set_text(self, text, start = None, end = None):
        """changes a portion of the buffer - see TextBuffer"""
        return self.change_notification.set_text(text, start, end)

    def get_length(self):
        return self.change_notification.get_length()

    def get_text(self, start = None, end = None):
        """retrieves a portion of the buffer
        - see TextBuffer"""
        return self.change_notification.get_text(start, end)

    def get_selection(self):
        """retrieves range of current selection
        - see TextBuffer"""
        return self.change_notification.get_selection()

    def set_selection(self, start = None, end = None):
        """changes range of current selection
        - see TextBuffer"""
        return self.change_notification.set_selection(start, end)

# this WOULD be delegated automatically, but we don't want it to.
# TextBufferSpecifyFromNotify is a TextBufferChangeSpecify object, not a
# TextBufferChangeNotify object, so it shouldn't have a
# set_change_notification_callback method.
    def set_change_notification_callback(self, callback):
        raise AttributeError("'%s' object has no attribute '%s'" % \
        (self.__class__.__name__, 'set_change_notification_callback'))
    
class TextBufferCRToNL(TextBufferChangeSpecify):
    """newline only wrapper around a TextBufferChangeSpecify which uses 
    carriage return + line feed.

    note: TextBufferCRToNL uses the __getattr__ method to
    delegate unknown attributes and methods to the underlying
    TextBufferChangeSpecify.  Thus, if a specific subclass of
    TextBufferChangeSpecify has additional attributes or methods, they
    will be visible through the containing TextBufferCRToNL.
    However, since TextBufferCRToNL inherits from
    TextBufferChangeSpecify, any
    attributes later added to the definition of TextBufferChangeSpecify will 
    not be unknown, and will not be
    automatically delegated.  Like get_text, set_text, etc.,
    corresponding methods must be added manually to
    TextBufferCRToNL to ensure that it will properly support
    these new attributes.

    **INSTANCE ATTRIBUTES**

    *TextBufferChangeSpecify* underlying -- 
    underlying TextBufferChangeSpecify using CR-LF for
    newline
    *STR* contents_internal -- copy of contents of the buffer, with CR-LF
    *STR* contents_external -- copy of contents of the buffer, with only NL
    *STR* crnl -- rep of CR-NL pair in underlying buffer
    *STR* nl -- rep of new line externally and in contents
    *INT* delta_width -- difference in lengths of crnl and nl

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, underlying, **args):
        """
        initializes the wrapper

        **INPUTS**

        *TextBufferChangeSpecify* underlying -- underlying buffer 
        """
    
        self.deep_construct(TextBufferCRToNL,
                            {'underlying':underlying,
                            'contents_internal': underlying.get_text(),
                            'contents_external': '',
                            'crnl': '\015\012',
                            'nl': '\n',
                            'delta_width': 0},
                            args)

        self.contents_external = string.replace(self.contents_internal, \
            self.crnl, self.nl)
        self.delta_width = len(self.crnl)-len(self.nl)
# preserve existing change_callback for ourselves
        self.set_change_callback(self.underlying.get_change_callback())
    
# intercept underlying change_callback so that we can keep track of the
# buffer contents ourselves
        self.underlying.set_change_callback(self._on_change_filter)
    
    def range_defaults(self, start = None, end = None):
        """converts TextBuffer range defaults to actual offsets.  Uses
        external offsets, which must be converted to internal with
        external_to_internal.

        **INPUTS**

        *INT* start -- external offset of start of range, or None for
        beginning of buffer
        *INT* end -- external offset of end of range, or None for
        end of buffer

        **OUTPUTS**

        *(INT, INT)* -- actual character range (external offsets)
        """
        if (start == None):
            s = 0
        else:
            s = start
        if (end == None):
            e = len(self.contents_external)
        else:
            e = end
        return s, e

    def line_range_internal(self, start, end):
        """find line numbers of a range of internal positions within
        contents_internal

        **INPUTS**

        *INT* start -- character offset into contents_internal of start of range
        *INT* end  -- character offset into contents_internal of end of range

        **OUTPUTS**

        *(INT, INT)* -- corresponding range of line numbers
        """
        first = len(string.split(self.contents_internal[0:start], self.crnl)) -1
        range = self.contents_internal[start:end]
        n = len(string.split(range, self.crnl)) -1
        second = first + n
        return first, second

    
    def line_range_external(self, start, end):
        """find line numbers of a range of external positions within
        contents_external

        **INPUTS**

        *INT* start -- character offset into contents_external of start of range
        *INT* end  -- character offset into contents_external of end of range

        **OUTPUTS**

        *(INT, INT)* -- corresponding range of line numbers
        """
        first = len(string.split(self.contents_external[0:start], self.nl)) -1
        range = self.contents_external[start:end]
        n = len(string.split(range, self.nl)) -1
        second = first + n
        return first, second

    def line_num_of_internal(self, internal):
        """find line number of an internal position within
        contents_internal

        **INPUTS**

        *INT* internal -- internal character offset into
        contents_internal

        **OUTPUTS**

        *INT* -- corresponding line number
        """
        return len(string.split(self.contents_internal[0:pos], self.crnl)) -1
    
    def external_to_internal(self, start, end):
        """converts a range of external positions (NL only) to
        internal positions (in the underlying
        buffer which uses CR-LF)

        **INPUTS**

        *INT* start -- start of range (external)
        *INT* end -- end of range (external)

        **OUTPUTS**

        *(INT, INT)* -- corresponding character range 
        internally, using CR-LF
        """
        lines = self.line_range_external(start, end)
        s = start +lines[0]*self.delta_width
        e = end +lines[1]*self.delta_width
        return s, e

    def internal_to_external(self, start, end):
        """converts a range of internal positions (in the underlying
        buffer which uses CR-LF) to external positions (NL only)

        **INPUTS**

        *INT* start -- start of range (internal)
        *INT* end -- end of range (internal)

        **OUTPUTS**

        *(INT, INT)* -- corresponding character range 
        externally, assuming only newlines
        """
        lines = self.line_range_internal(start, end)
        s = start -lines[0]*self.delta_width
        e = end -lines[1]*self.delta_width
        return s, e

    def _on_change_filter(self, start, end, text,
        selection_start, selection_end, buffer, program_initiated):
        """private method which handles change specification
        callbacks from the underlying TextBufferChangeSpecify object, 
        updating our internal copy of the buffer and translating character 
        ranges to get rid of CR-NL.
        Should not be called manually.
        """

        old = self.contents_internal[start:end]
#        print 'cr._on_change_filter old: [%s] new: [%s]' % (old, text)
        before = self.contents_internal[0:start]
        after = self.contents_internal[end:]
        s, e = self.internal_to_external(start, end)
        self.contents_internal = before + text + after
        dif = \
            find_difference.find_string_difference(self.contents_internal, 
            self.underlying.get_text())
        if dif:
            d_start, d_end, d_text = dif
            if d_text:
                print 'discrepancy: ', d_start, d_end
                print '\ninternal:\n', repr(self.contents_internal[d_start:d_end])
                print '\nvoice:\n', repr(d_text)
                print '\n'
        t = string.replace(text, self.crnl, self.nl)
#        print 'filter external', s, e, t
        before = self.contents_external[0:s]
        after = self.contents_external[e:]
        self.contents_external = before + t + after
        should_be = string.replace(self.contents_internal, self.crnl, self.nl)
        dif = \
            find_difference.find_string_difference(self.contents_external,
            should_be)
        if dif:
            d_start, d_end, d_text = dif
            if d_text:
                print 'discrepancy: ', d_start, d_end
                print '\nexternal:\n', repr(self.contents_external[d_start:d_end])
                print '\ninternal:\n', repr(d_text)
                print '\n'
        s_sel, e_sel = self.internal_to_external(selection_start, selection_end)
        self._on_change_specification(s, e, t, s_sel, e_sel, program_initiated)

    def __getattr__(self, name):
        """delegates unknown attributes (including methods) to the underlying
        TextBufferChangeNotify object"""
        return getattr(self.underlying, name)

# no-op versions of these functions are defined in
# TextBufferChangeSpecify, so
# __getattr__ will not automatically delegate them to the underlying
# TextBufferChangeSpecify, and besides we need to do translation

    def set_text(self, text, start = None, end = None):
        """changes a portion of the buffer - see TextBuffer"""
        s, e = self.range_defaults(start,end)
        s, e = self.external_to_internal(s, e)
        t = string.replace(text, self.nl, self.crnl)
#        print 'cr.set_text changing', s, e, t
        return self.underlying.set_text(t, s, e)

    def get_length(self):
        return len(self.contents_external)

    def get_text(self, start = None, end = None):
        """retrieves a portion of the buffer
        - see TextBuffer"""
# avoid translation by using contents_external directly
        s, e = self.range_defaults(start, end)
        return self.contents_external[s:e]

    def get_selection(self):
        """retrieves range of current selection
        - see TextBuffer"""
        selection_start, selection_end = self.underlying.get_selection()
        return self.internal_to_external(selection_start, selection_end)

    def set_selection(self, start = None, end = None):
        """changes range of current selection
        - see TextBuffer"""
        s, e = self.range_defaults(start,end)
#        print 'cr.set_selection (ext)', s, e
        s, e = self.external_to_internal(s, e)
#        print 'cr.set_selection (int)', s, e
        return self.underlying.set_selection(s, e)

class SelectionTextBufferCRToNL(TextBufferCRToNL):
    """newline only wrapper around a buffer inheriting from both
    TextBufferChangeSpecify and SelectionBuffer which uses 
    carriage return + line feed.

    note: SelectionTextBufferCRToNL uses the __getattr__ method to
    delegate unknown attributes and methods to the underlying
    TextBufferChangeSpecify/SelectionBuffer.  Thus, if a specific subclass of
    TextBufferChangeSpecify/SelectionBuffer has additional attributes or 
    methods, they will be visible through the containing TextBufferCRToNL.
    However, since SelectionTextBufferCRToNL inherits from
    TextBufferCRToNL, any
    attributes later added to the definition of TextBufferChangeSpecify will 
    not be unknown, and will not be
    automatically delegated.  Like get_text, set_text, etc.,
    corresponding methods must be added manually to
    TextBufferCRToNL or SelectionTextBufferCRToNL to ensure that 
    it will properly support
    these new attributes.

    **INSTANCE ATTRIBUTES**

    see TextBufferCRToNL

    **CLASS ATTRIBUTES**
    
    *none*
    """

    def __init__(self, **args):
        """
        initializes the wrapper

        **INPUTS**

        *none*

        """
    
        self.deep_construct(SelectionTextBufferCRToNL,
                            {},
                            args)


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
        s, e = range
        if e >= s:
            s, e = self.external_to_internal(s, e)
        return self.underlying.set_visible((s, e))

            
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
        s, e = self.underlying.get_visible(s, e)
        if e >= s:
            s, e = self.internal_to_external(s, e)
        return s, e

