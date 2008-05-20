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

"""interface and partial implementation of a new generic GUI editor which 
can function as an interface to the mediator simulation, or as a test client"""


import debug
import sys, os
import TextBuffer
import Object

def silent_load_file(buffer, file_name):
    """load a new file into an existing buffer, but avoid sending the 
    change_callback to the managing SourceBuffTB, because we are about to
    rename the buffer.

    **INPUTS**

    *TextBufferChangeSpec buffer* -- the buffer

    *STR file_name* -- full path of the file to load

    **OUTPUTS**

    *BOOL* -- true if the file was loaded successfully
    """
    callback = buffer.get_change_callback()
    buffer.set_change_callback(None)
    success = buffer.load_file(file_name)
    buffer.set_change_callback(callback)
    return success

def clear_buffer(buffer):
    """clear the contents of a TextBufferChangeSpec to make room for it
    to contain a new, empty, file, but avoid sending the change_callback
    to the managing SourceBuffTB.

    **INPUTS**

    *TextBufferChangeSpec buffer* -- the buffer

    **OUTPUTS**

    *none*
    """
    callback = buffer.get_change_callback()
    buffer.set_change_callback(None)
    buffer.set_text("")
    buffer.set_change_callback(callback)

class GenEdit(Object.OwnerObject):
    """base class for new generic GUI editor which can function as an
    interface to the mediator simulation, or as a test client

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *AppState app_control* -- reference to the parent
    AppState object, which requires certain callbacks
    """
    def __init__(self, **args):
        """**Note:** GenEdit requires the app_control attribute to
        perform the appropriate callbacks to AppState.  However, since 
        AppStateGenEdit can't be constructed without a reference to an 
        existing GenEdit, AppStateGenEdit.__init__ will call 
        set_app_control to set its value."""
        self.deep_construct(GenEdit,
                            {'app_control': None},
                            args)
        self.add_grandparent('app_control')
# note: app_control is a reference to a (grand)parent object, which must
# be deleted so that that object can be deleted.  However, since
# AppState is usually considered an interface to an editor, it does not
# "own" GenEdit and its cleanup method won't call GenEdit's.
# Therefore, unless a subclass of GenEdit is also an owner object and
# has an owner, GenEdit must call cleanup itself when the user
# tells it to exit
        
    def file_name(self, buff_name):
        """returns the current filename associated with a given buffer

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *STR* -- the filename
        """
        debug.virtual('GenEdit.file_name')

    def set_app_control(self, app_control):
        """method called by app_control's (AppStateGenEdit.) __init__ 
        to supply reference to itself, so that GenEdit can perform
        callbacks.

        **INPUTS**

        *AppStateGenEdit app_control* -- the AppState interface

        **OUTPUTS**

        *none*
        """
        self.app_control = app_control

    def mic_change(self, state):
        """function to receive microphone state change callbacks

        **INPUTS**

        *STR* state -- new state ('on', 'off', 'sleeping', 'disabled')

        **OUTPUTS**

        *none*
        """
        pass
# no-op by default, can be overridden

    def is_active(self):
        """indicates whether an editor frame is active

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if frame window is active
        """
        debug.virtual('GenEdit.is_active')

    def editor_has_focus(self):
        """indicates whether the editor window has the focus

        **INPUTS**

        *none*

        **OUTPUTS**
        *BOOL* -- true if editor window has the focus
        """
        debug.virtual('GenEdit.editor_has_focus')

    def app_active_buffer_name(self):
        """Returns the name of the buffer currently active in the
        GenEdit editor.

        **INPUTS**

        *none* 
        
        **OUTPUTS**

        *STR* -- buffer name of current buffer, or None if there is none

        """

        debug.virtual('GenEdit.app_active_buffer_name')

    def app_change_buffer(self, buff_name):
        """Changes the external application's active buffer. 

        **INPUTS**
        
        STR *buff_name* -- Name of the buffer to switch to.
       
        **OUTPUTS**
        
        *BOOL* -- true if buff_name exists and the application
        successfully switches to it
        """
        debug.virtual('GenEdit.app_change_buffer')

    def open_buffers(self):
        """retrieve a list of the names of open buffers from the
        application.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of the names of open buffers
        """
        debug.virtual('GenEdit.open_buffers')

    def editor_buffer(self, buff_name):
        """returns a reference to the TextBufferChangeSpec embedded 
        in the GUI which corresponds to buffer buff_name

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *TextBufferChangeSpec* -- the TextBufferChangeSpec
        """
        debug.virtual('GenEdit.editor_buffer')

    def active_frame(self):
        """returns the currently active frame, if any

        **INPUTS** 

        *none*

        **OUTPUTS**

        *GenEditFrame* -- the currently active frame, or None if
        no frame of the editor is active.
        """ 
        debug.virtual('GenEdit.active_frame')

    def multiple_windows(self):
        """does editor support multiple windows per instance?

        Note: the purpose of this function is to allow the RecogStartMgr
        to determine whether a previously unknown window could belong to
        this known instance.  Therefore, Emacs running in text mode 
        should return false, even though it can have (sub-)windows in 
        a single frame.  
        
        Note: multiple windows of remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will not appear to the mediator 
        as having separate windows.  However, the mediator will perform a 
        separate check to detect this, so remote editors which support
        multiple windows should return true, regardless of the remote
        display method.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports opening multiple editor windows.  
        """
        debug.virtual('GenEdit.multiple_windows')

    def multiple_buffers(self):
        """does editor support multiple open buffers?

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports having multiple buffers open 
        at the same time"""
        debug.virtual('GenEdit.multiple_windows')

    def new_buffer(self, buff_name, buffer, perform_callback = 1):
        """adds a new buffer, optionally performing a callback to the
        AppState interface

        **INPUTS**

        *STR buff_name* -- the name of the new buffer

        *TextBufferChangeSpec buffer* -- the TextBufferChangeSpec 
        interface to the new buffer

        *BOOL perform_callback* -- indicates whether this method should
        invoke the parent AppState's open buffer callback

        **OUTPUTS**

        *BOOL* -- true if the new buffer was added successfully
        """
        debug.virtual('GenEdit.new_buffer')

    def rename_buffer(self, buff_name, new_buff_name, perform_callback = 1):
        """renames a buffer, optionally performing a callback to the
        AppState interface

        **INPUTS**

        *STR buff_name* -- the old name of the new buffer

        *STR new_buff_name* -- the new name of the new buffer

        *BOOL perform_callback* -- indicates whether this method should
        invoke the parent AppState's rename buffer callback

        **OUTPUTS**

        *BOOL* -- true if the new buffer was renamed successfully
        """
        debug.virtual('GenEdit.rename_buffer')

    def open_file(self, file_name, perform_callback = 0):
        """opens a new file 

        **INPUTS**

        *STR file_name*  -- the full path of the file to open

        *BOOL user_initiated* -- indicates whether this method was
        user-initiated or whether it was called by AppState.
        In the latter case, it will not invoke the parent AppState's 
        open buffer callback, because AppState.open_file invokes 
        the callback itself. 

        **OUTPUTS**
        
        STR *buff_name* -- Unique name of the buffer in which the file
        was opened. Returns *None* if the editor was not able to open
        the file.
        """
        debug.virtual('GenEdit.open_file')

    def prompt_to_save(self, buff_name):
        """prompts the user to save the current buffer before closing it, 
        or cancel.  Note: prompt_to_save should save if the user so
        indicates, and update the entry in self.filenames corresponding
        to the buffer, but should not close the buffer, because
        open_file could still fail.

        **INPUTS**

        *STR buff_name* -- the name of the buffer
        
        **OUTPUTS**

        *BOOL* -- true if the user saved or told GenEdit to proceed
        without saving, false if the user asked for the action causing
        the buffer closing to be cancelled.
        """
        debug.virtual('GenEdit.prompt_to_save')

    def overwrite_prompt(self, buff_name, full_path):
        """prompts to see if the user is sure that he/she wants to
        overwrite an existing file

        **INPUTS**

        *STR buff_name* -- the name of the buffer (used to find the
        corresponding frame over which to pop up the Save As dialog)

        *STR full_path* -- path name of file to save

        **OUTPUTS**

        *BOOL* -- true if the user approves of overwriting the file
        """
        debug.virtual('GenEdit.overwrite_prompt')

    def open_file_dialog(self):
        """prompts for a file to open

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the specified path, or None if the user cancelled 
        """
        debug.virtual('GenEdit.open_file_dialog')

    def save_as_dialog(self, buff_name):
        """prompts for a filename under which to save the file, and
        confirms overwriting

        **INPUTS**

        *STR buff_name* -- the name of the buffer (used to find the
        corresponding frame over which to pop up the Save As dialog)

        **OUTPUTS**

        *STR* -- the specified path, or None if the user cancelled 
        """
        debug.virtual('GenEdit.save_as_dialog')

    def save_file(self, buff_name, full_path = None, no_prompt = 0,
        rename_buff = 1, ask_for_new_name = 0, perform_callback = 0):
        """Saves the buffer buff_name to a file

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        *STR full_path* -- path name of file to save, or None to use the
        current file name, or prompt

        *BOOL no_prompt* -- if true, don't prompt before overwriting
        an existing file.

        *BOOL rename_buff* -- if false, do not rename the buffer

        *BOOL ask_for_new_name* -- if true, prompt for a new name, even if the
        buffer already has a corresponding filename.  Ignored unless
        full_path == None.

        *BOOL perform_callback* -- indicates whether this method should
        invoke the parent AppState's rename buffer callback.  AppState
        should use the default value of false, because
        AppState.save_file invokes the callback itself, but 
        save_file initiated from the frame by the user must 
        use perform_callback = 1

        **OUTPUTS**

        *STR* -- new buffer name if successful, or None if the save 
        failed
        """
        debug.virtual('GenEdit.save_file')

    def app_close_buffer(self, buff_name, save=0):
        """Close a buffer.
        
        **INPUTS**

        STR *buff_name* -- name of buffer to close

        INT *save* -- *-1* -> don't save the buffer
                            *0* -> query user if buffer needs saving
                            *1* -> save without querying user

        **OUTPUTS**
        
        *BOOL* -- true if the editor does close the buffer
        """
        debug.virtual('GenEdit.app_close_buffer')


    def set_instance_string(self, instance_string):
        """sets the title string which is included in the full title 
        displayed in the title bar

        **INPUTS**

        *STR* instance_string -- string to include as part of the title

        **OUTPUTS**

        *BOOL* -- true if the editor can and will include the 
        instance string in its window title for all windows 
        containing editor buffers.
        """
        debug.virtual('GenEdit.set_instance_string')

    def on_exit(self, ID = None):
        """method by which a frame can notify GenEdit that the user has
        selected the Exit item from the File menu.  The user may have an 
        opportunity to cancel this command (e.g. through the cancel button
        in a dialog prompting to save modified files)

        **NOTE:** GenEdit is responsible for telling all frames to
        cleanup and close, so the calling should not assume that
        it is in a sane state when this method returns.

        *BOOL* -- true if the editor is exiting in response to this
        event (unless, e.g., the user has hit cancel in response to a 
        save modified files dialog)

        **INPUTS**

        *INT ID* -- ID of the frame sending the event, or None if the
        event doesn't originate from a frame.

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEdit.on_exit')

  
class GenEditBuffers(GenEdit):
    """partial implementation of GenEdit

    **NOTE:** implicit in the use of corresponding_frame is the assumption
    that no buffer ever appears in more than one frame

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *{STR: TextBufferChangeSpec} buffers* -- map from buffer names to
    TextBufferChangeSpec references (in fact these buffers must 
    support the VisibleBuffer, StoreableTextBuffer, and NumberedLines
    interfaces as well)

    *{STR: BOOL} scratch_buffers* -- set of buffers designated as
    scratch buffers, not needing to be saved

    *{STR: STR} filenames* -- map from buffer names to filenames

    *STR curr_dir* -- the current directory

    *BOOL multiple* -- does this GenEdit implementation support multiple
    buffers?
    """
    def __init__(self, multiple = 0, curr_dir = None, **args):
        self.deep_construct(GenEditBuffers,
                            {'multiple': multiple,
                             'buffers': {},
                             'scratch_buffers': {},
                             'filenames': {},
                             'curr_dir': curr_dir
                            }, args)
                             
    def mark_as_scratch(self, buff_name, as_scratch = 1):
        """mark a buffer as a scratch buffer which never needs to be
        saved.

        **NOTE:** Use this method with caution, as it can cause the user
        to lose work when exiting the editor.  This method is used to
        avoid having to save buffers created/modified by regression 
        testing.

        **INPUTS**

        *STR buff_name* -- name of the buffer

        *BOOL as_scratch* -- whether to mark the buffer as a scratch
        buffer or unmark it

        **OUTPUTS**

        *none*
        """
        if as_scratch:
            self.scratch_buffers[buff_name] = 1
        else:
            try:
                del self.scratch_buffers[buff_name]
            except KeyError:
                pass

    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.buffers = None
        self.filenames = None
# subclasses must call their parent class's remove_other_references
# function, after performing their own duties
        GenEdit.remove_other_references(self)

    def file_name(self, buff_name):
        """returns the current filename associated with a given buffer

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *STR* -- the filename
        """
        try:
            return self.filenames[buff_name]
        except KeyError:
            return None

    def open_buffers(self):
        """retrieve a list of the names of open buffers from the
        application.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of the names of open buffers
        """
#        print 'open buffers: ', self.buffers.keys()
        return self.buffers.keys()

    def modified_buffer(self, buff_name):
        """returns a list of names buffers which have been modified
        since the last time they were saved

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- the list of buffer names
        """
        buffer = self.buffers[buff_name]
        if buffer.modified():
            try:
                self.scratch_buffers[buff_name]
            except KeyError:
                pass
            else:
# if buffer is a scratch buffer, pretend it is unmodified
                return 0
            if buffer.len() != 0 or self.filenames[buff_name] != None: 
# our method of creating a new buffer when one is closed inadvertently
# makes the new, empty buffer appear to have been modified
                return 1
        return 0

    def modified_buffers(self):
        """returns a list of names buffers which have been modified
        since the last time they were saved

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- the list of buffer names
        """
        modified = []
#        print 'checking modified buffers:'
#        print 'all buffers:', self.buffers.keys()
        for buff_name in self.buffers.keys():
#            print 'buff_name = "%s"' % buff_name
            if self.modified_buffer(buff_name):
                modified.append(buff_name)
        return modified

    def editor_buffer(self, buff_name):
        """returns a reference to the TextBufferChangeSpec embedded in 
        the GUI which corresponds to buffer buff_name

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *TextBufferChangeSpec* -- the TextBufferChangeSpec
        """
        if buff_name in self.buffers.keys():
            return self.buffers[buff_name]
        return None

    def multiple_buffers(self):
        """does editor support multiple open buffers?

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports having multiple buffers open 
        at the same time"""
        return self.multiple

    def new_buffer(self, buff_name, buffer, perform_callback = 1):
        """adds a new buffer, optionally performing a callback to the
        AppState interface

        **INPUTS**

        *STR buff_name* -- the name of the new buffer

        *TextBufferChangeSpec buffer* -- the TextBufferChangeSpec interface 
        to the new buffer

        *BOOL perform_callback* -- indicates whether this method should
        invoke the parent AppState's open buffer callback

        **OUTPUTS**

        *BOOL* -- true if the new buffer was added successfully
        """
        if buff_name in self.open_buffers():
            return 0
        self.buffers[buff_name] = buffer
        self.filenames[buff_name] = None
# this is incorrect -- do this in GenEdit filenames and in SourceBuffTB
# but not TextBufferChangeSpec
#        buffer.name_file("")
        if perform_callback and self.app_control:
            self.app_control.open_buffer_cbk(buff_name)
        return 1

    def rename_buffer(self, buff_name, new_buff_name, perform_callback = 1):
        """renames a buffer, optionally performing a callback to the
        AppState interface

        **INPUTS**

        *STR buff_name* -- the old name of the new buffer

        *STR new_buff_name* -- the new name of the new buffer

        *BOOL perform_callback* -- indicates whether this method should
        invoke the parent AppState's rename buffer callback

        **OUTPUTS**

        *BOOL* -- true if the new buffer was renamed successfully
        """
#        print 'rename "%s" to "%s"' % (buff_name, new_buff_name)
#        print 'buffers currently = ', self.open_buffers()
        if buff_name not in self.open_buffers():
            return 0
        if new_buff_name in self.open_buffers():
            return 0
        if buff_name == new_buff_name:
            return 1
        self.buffers[new_buff_name] = self.buffers[buff_name]
        self.filenames[new_buff_name] = self.filenames[buff_name]
        del self.buffers[buff_name]
        del self.filenames[buff_name]
#        print 'after rename:'
#        print 'buffers currently = ', self.open_buffers()
        if perform_callback and self.app_control:
            self.app_control.rename_buffer_cbk(buff_name, new_buff_name)
        return 1

    def show_buffer(self, buff_name, perform_callback = 1):
        """makes a buffer the foreground one.
        Depending on the subclass of GenEditBuffers, this may activate a
        different frame, or reveal a previously hidden buffer and update
        the window title appropriately)

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        *BOOL perform_callback* -- indicates whether this method should
        invoke the curr_buffer_name_cbk

        **OUTPUTS**

        *none*
        """
        success = self.app_change_buffer(buff_name)
        self.update_title(buff_name)
        if success and perform_callback:
            if self.app_control:
                self.app_control.curr_buffer_name_cbk(buff_name)

    def open_file_new_buffer(self, file_name, new_buff_name,
            user_initiated = 0, file_exists = 1):
        """opens a new file.  Depending on the subclass of 
        GenEditBuffers, this may open a new frame, or hide the 
        previously visible buffer in the same frame.

        **NOTE:** despite the name of this method, the 
        TextBufferChangeSpec returned can be an existing buffer, 
        if, e.g.,  the editor only has one window and one buffer.

        **INPUTS**

        *STR file_name*  -- the full path of the file to open, or None
        to create a new, empty buffer with no filename

        *STR new_buff_name*  -- the name which will be given to the new
        buffer

        *BOOL user_initiated* -- indicates whether this method was
        user-initiated or whether it was called by AppState.
        In the latter case, it will not invoke the parent AppState's 
        open buffer callback, because AppState.open_file invokes 
        the callback itself. 

        *BOOL file_exists* -- indicates whether the specified file
        exists (this allows us to specify a filename for an initially
        empty buffer)

        **OUTPUTS**

        *BOOL* -- true if the file was opened successfully.
        Note: if no file by the name file_name exists, 
        open_file_new_buffer should create a new, empty buffer with 
        that file name.
        """
        debug.virtual('GenEditBuffers.open_file_new_buffer')

    def corresponding_frame(self, buff_name):
        """returns a reference to the GenEditFrame containing the given
        buffer

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *GenEditFrame* -- the corresponding frame
        """
        debug.virtual('GenEditBuffers.corresponding_frame')

    def generate_buffer_name(self, file_name):
        """find a new, unique buffer name for a buffer, given a file
        name

        **INPUTS**

        *STR file_name*  -- the short file name of the file (without the
        path)

        **OUTPUTS**
        
        STR *buff_name* -- Unique name for the buffer 
        """
        new_buff_name = file_name
        if new_buff_name in self.buffers.keys():
            i = 2
            alt_name = "%s (%d)" % (new_buff_name, i)
            while alt_name in self.buffers.keys():
                i = i +1
                alt_name = "%s (%d)" % (new_buff_name, i)
            new_buff_name = alt_name
        return new_buff_name

    def open_file(self, file_name = None, user_initiated = 0):
        """opens a new file 

        **INPUTS**

        *STR file_name*  -- the full path of the file to open, or None
        to ask the user

        *BOOL user_initiated* -- indicates whether this method was
        user-initiated or whether it was called by AppState.
        In the latter case, it will not invoke the parent AppState's 
        open buffer callback, because AppState.open_file invokes 
        the callback itself. 

        **OUTPUTS**
        
        STR *buff_name* -- Unique name of the buffer in which the file
        was opened. Returns *None* if the editor was not able to open
        the file.  Note: if no file by the name file_name exists, the 
        regression tests expect the editor to open an empty buffer with
        that name.  Therefore, open_file should only fail
        if the user cancels the open file command (e.g. if there is an
        unsaved buffer) or if file_name was omitted and the user cancelled
        the Open File dialog box
        """
        if not self.multiple_buffers():
            buff_name = self.app_active_buffer_name()
            if buff_name != None:
                if self.modified_buffer(buff_name):
# our method of creating a new buffer when one is closed inadvertently
# makes the new, empty buffer appear to have been modified
                    proceed = self.prompt_to_save(buff_name)
                    if not proceed:
                        return None
        if file_name == None:
            file_name = self.open_file_dialog()
            if file_name == None:
                return None
        path, short = os.path.split(file_name)
        new_buff_name = self.generate_buffer_name(short)
#        print 'new buffer name is "%s"' % new_buff_name
        file_or_none = file_name
        file_exists = 1
        if not os.path.exists(file_name):
# don't throw away the filename, just warn open_file_new_buffer that it
# the file doesn't exist
#            file_or_none = None
            file_exists = 0
#        print 'before ofnb: buffers = ', self.buffers.keys()
#        print 'file_or_none is ', file_or_none
        success = self.open_file_new_buffer(file_or_none, new_buff_name,
            user_initiated, file_exists = file_exists)
#        print 'after ofnb: buffers = ', self.buffers.keys()
        if not success:
            return None
        if path:
            self.curr_dir = path
# moved to open_file_new_buffer to be before the open_buffer_cbk
#        self.filenames[new_buff_name] = file_name
# this is incorrect -- do this in GenEdit filenames and in SourceBuffTB
# but not TextBufferChangeSpec
#        self.buffers[new_buff_name].name_file(file_name)
        if user_initiated and self.app_control:
            if not self.multiple_buffers():
# AppStateGenEditor.tell_editor_to_open_file does this, if necessary, 
# but if we are user-initiated we need to do it ourselves
                self.app_control.close_buffer_cbk(buff_name)
# open_file_new_buffer will do this (because if it creates a new frame,
# it needs to do the open_buffer_cbk before the new_window_cbk)
        self.show_buffer(new_buff_name, perform_callback =
            user_initiated)
#        print 'after show buffer: buffers = ', self.buffers.keys()
# single buffer editors just rename the buffer instead of creating a new
# one, so we don't want to try to remove the old one
#        if not self.multiple:
#            self.remove_buffer(buff_name, perform_callback = 1)
        return new_buff_name

    def save_specified(self, buff_name, full_path, no_prompt = 0):
        """Saves the buffer buff_name to a file.  This version assumes
        buff_name exists and full_path has actually been specified, but
        will still prompt if asked to overwrite an existing file, unless
        no_prompt is true.

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        *STR full_path* -- path name of file to save

        *BOOL no_prompt* -- if true, don't prompt before overwriting
        an existing file.

        **OUTPUTS**

        *BOOL* -- true if file was saved successfully
        """
        buffer = self.editor_buffer(buff_name)
        if buffer == None:
            return 0
        if os.path.exists(full_path) and not no_prompt:
            frame = self.corresponding_frame(buff_name)
            okay = frame.overwrite_prompt(buff_name, full_path)
            if not okay:
                return 0
        return buffer.save_file(full_path)

    def save_file(self, buff_name, full_path = None, no_prompt = 0,
        rename_buff = 1, ask_for_new_name = 0, user_initiated = 0):
        """Saves the buffer buff_name to a file

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        *STR full_path* -- path name of file to save, or None to use the
        current file name, or prompt

        *BOOL no_prompt* -- if true, don't prompt before overwriting
        an existing file.

        *BOOL rename_buff* -- if false, do not rename the buffer

        *BOOL ask_for_new_name* -- if true, prompt for a new name, even if the
        buffer already has a corresponding filename.  Ignored unless
        full_path == None.

        *BOOL user_initiated* -- indicates whether this method was
        user-initiated or whether it was called by AppState.
        In the latter case, it will invoke the parent AppState's 
        rename buffer callback.  If this method is called by AppState,
        this would be redundant, because AppState.save_file invokes 
        the callback itself. 

        **OUTPUTS**

        *STR* -- new buffer name if successful, or None if the save 
        failed
        """
        buffer = self.editor_buffer(buff_name)
        if buffer == None:
            return None
        try:
            old_name = self.filenames[buff_name]
        except KeyError:
            old_name = None

        f_path = full_path
        quiet = no_prompt
        if not f_path:
            if old_name and not ask_for_new_name:
                f_path = old_name
# if saving under same file name, never prompt
                quiet = 1
            else:
                f_path = self.save_as_dialog(buff_name)
# save_as_dialog will already have prompted about overwriting existing
# file
                quiet = 1
                if f_path == None:
                    return None
        success = self.save_specified(buff_name, f_path, quiet)
        if not success:
            return None
        path, short = os.path.split(f_path)
        if path:
            self.curr_dir = path
        new_buff_name = buff_name
        self.filenames[buff_name] = f_path
        if rename_buff:
# if the file acquired a new name, rename the buffer accordingly
            if not old_name:
                new_buff_name = self.generate_buffer_name(short)
            else:
                old_path, old_short = os.path.split(old_name)
                if short != old_short:
                    new_buff_name = self.generate_buffer_name(short)
            if new_buff_name != buff_name:
#                print 'rename on save "%s" to "%s"' % (buff_name, new_buff_name)
                self.rename_buffer(buff_name, new_buff_name,
                    perform_callback = user_initiated)
#            buffer.name_file(f_path)
            self.update_title(new_buff_name)
        return new_buff_name

    def update_title(self, buff_name):
        """update the window title of the frame containing buff_name to
        reflect a new buffer name (unless that buffer is not the current
        buffer displayed in that frame)
        
        **INPUTS**

        *STR buff_name* -- name of the buffer

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditBuffers.update_title')

    def buffer_title(self, buff_name):
        """the buffer or file name to be used when a particular buffer is 
        active
        
        **INPUTS**

        *STR buff_name* -- name of the buffer

        **OUTPUTS**

        *STR* -- the name to combine with the application name and
        instance string
        """
        return buff_name

    def remove_buffer(self, buff_name, perform_callback = 1):
        """remove a buffer from the list of buffers.  

        **Note:** this method only removes the buffer from GenEdit's
        records, it does not destroy the underlying GUI buffer or the
        window containing it.  For that, use delete_buffer (which will
        also call this method)

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        *BOOL perform_callback* -- indicates whether this method should
        invoke the curr_buffer_name_cbk

        **OUTPUTS**

        *none*
        """
#        print 'removing buffer "%s"' %buff_name
        if buff_name in self.open_buffers():
            del self.buffers[buff_name]
            del self.filenames[buff_name]

    def delete_buffer(self, buff_name, perform_callback = 1):
        """delete a buffer 

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        *BOOL perform_callback* -- indicates whether this method should
        invoke the curr_buffer_name_cbk

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditBuffers.delete_buffer')

    def app_close_buffer(self, buff_name, save=0):
        """Close a buffer.
        
        **INPUTS**

        STR *buff_name* -- name of buffer to close

        INT *save* -- *-1* -> don't save the buffer
                            *0* -> query user if buffer needs saving
                            *1* -> save without querying user

        **OUTPUTS**
        
        *BOOL* -- true if the editor does close the buffer
        """
#        print 'closing buffer "%s"' % buff_name
#        print 'currently buffers = ', self.buffers.keys()
#        print 'GenEdit.app_close_buffer, save = ', save
        buffer = self.buffers[buff_name]
        if buffer == None:
            return 0
        if self.modified_buffer(buff_name):
            if save == 1:
#                print 'saving'
                self.save_file(buff_name)
            elif save == 0:
#                print 'prompting'
                proceed = self.prompt_to_save(buff_name)
                if not proceed:
                    return 0
#            else:
#                print 'neither'
#        print 'about to delete buffer "%s"' % buff_name
        self.delete_buffer(buff_name)
#        print 'just deleted buffer "%s"' % buff_name
        return 1

class GenEditFrame(Object.OwnerObject):
    """abstract base class for frames for GenEdit

    Note: to destroy a GenEditFrame, its owner must call the 
    cleanup method (inherited from OwnerObject), followed by close_window.  
    The reason for this is that some signals to close the frame are 
    sent as callbacks from the frame to the GenEditFrames object 
    which owns it.  Separating the call to clean up the generic frame
    from the call to destroy the GUI frame allows us to let the function
    making the callback close the frame when the callback returns, while
    still allowing an exit callback to close all frames.

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**
    
    *INT frame_ID* -- unique ID for the frame (assigned by
    GenEditFrames.add_frame, not by the GUI)
    """
    def __init__(self, **args):
        self.deep_construct(GenEditFrame,
                            {'frame_ID': None}, args)

    def set_frame_ID(self, ID):
        """set the frame ID, which the frame will use to identify itself
        to its owner in callbacks

        **INPUTS**

        *INT ID* -- unique ID for the frame (assigned by
        GenEditFrames.add_frame, not by the GUI)
        
        **OUTPUTS**

        *none*
        """
        self.frame_ID = ID

    def close_window(self):
        """close the window corresponding to this frame

        **NOTE:** The owner must call the frame's cleanup method before
        calling this method to close the actual GUI frame

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditFrame.close_window')

    def show(self, initial = 0):
        """show the window corresponding to this frame

        **INPUTS**

        *BOOL* initial -- is this the initial time the frame is shown?

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditFrame.show')


    def open_buffers(self):
        """retrieve a list of the names of open buffers associated with
        contained by this frame.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- list of the names of open buffers
        """
        debug.virtual('GenEditFrame.open_buffers')

    def is_active(self):
        """indicates whether this frame is active

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if frame window is active
        """
        debug.virtual('GenEditFrame.is_active')

    def editor_has_focus(self):
        """indicates whether the editor control has the focus in this
        frame

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if editor window has the focus
        """
        debug.virtual('GenEditFrame.editor_has_focus')

    def frame_active_buffer_name(self):
        """Returns the name of the buffer currently active in this
        frame.

        **INPUTS**

        *none* 
        
        **OUTPUTS**

        *STR* -- buffer name of current buffer, or None if there is none
        """

        debug.virtual('GenEditFrame.frame_active_buffer_name')

    def switch_to_buffer(self, buff_name):
        """Puts this frame in the foreground (if it isn't already), and
        changes the active buffer to buff_name

        **INPUTS**
        
        STR *buff_name* -- Name of the buffer to switch to.
       
        **OUTPUTS**
        
        *BOOL* -- true if buff_name exists and the external application
        successfully switches to it
        """
        debug.virtual('GenEditFrame.switch_to_buffer')
        
    def rename_buffer(self, buff_name, new_buff_name):
        """notifies the frame that one of its buffers has been renamed

        **INPUTS**

        *STR buff_name* -- the old name of the new buffer

        *STR new_buff_name* -- the new name of the new buffer

        **OUTPUTS**

        *BOOL* -- false if the old buff_name was unknown
        """
        debug.virtual('GenEditFrame.rename_buffer')

    def remove_buffer(self, buff_name):
        """remove a buffer from the list of belong to this frame.  

        **Note:** this method only removes the buffer from GenEditFrame's
        records, it does not destroy the underlying GUI buffer or the
        window containing it.

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditFrame.remove_buffer')

    def delete_buffer(self, buff_name):
        """delete a buffer 

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditFrame.delete_buffer')


    def set_instance_string(self, instance_string):
        """update the title to reflect the new instance string

        **INPUTS**

        *STR* instance_string -- string to include as part of the title

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditFrame.set_instance_string')
  
    def update_title(self):
        """update the window title of the frame reflect a new buffer name 
        
        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.virtual('GenEditFrame.update_title')

    def open_file_dialog(self, init_dir):
        """prompts for a file to open

        **INPUTS**

        *STR init_dir* -- the path of the initial directory for the Open
        File dialog

        **OUTPUTS**

        *STR* -- the specified path, or None if the user cancelled 
        """
        debug.virtual('GenEditFrame.open_file_dialog')


    def save_as_dialog(self, buff_name, init_dir):
        """prompts for a filename under which to save the file, and
        confirms overwriting

        **INPUTS**

        *STR buff_name* -- the name of the buffer (used to find the
        corresponding frame over which to pop up the Save As dialog)

        *STR init_dir* -- the path of the initial directory for the Save
        As dialog

        **OUTPUTS**

        *STR* -- the specified path, or None if the user cancelled 
        """
        debug.virtual('GenEditFrame.save_as_dialog')

    def prompt_to_save(self, buff_name):
        """prompts the user to save the current buffer before closing it, 
        or cancel.  Note: prompt_to_save should save if the user so
        indicates, and update the entry in self.filenames corresponding
        to the buffer, but should not close the buffer, because
        open_file could still fail.

        **INPUTS**

        *STR buff_name* -- the name of the buffer
        
        **OUTPUTS**

        *BOOL* -- true if the user saved or told GenEdit to proceed
        without saving, false if the user asked for the action causing
        the buffer closing to be cancelled.
        """
        debug.virtual('GenEditFrame.prompt_to_save')

    def overwrite_prompt(self, buff_name, full_path):
        """prompts to see if the user is sure that he/she wants to
        overwrite an existing file

        **INPUTS**

        *STR buff_name* -- the name of the buffer (used to find the
        corresponding frame over which to pop up the Save As dialog)

        *STR full_path* -- path name of file to save

        **OUTPUTS**

        *BOOL* -- true if the user approves of overwriting the file
        """
        debug.virtual('GenEditFrame.overwrite_prompt')

    def editor_buffer(self, buff_name):
        """returns a reference to the TextBufferChangeSpec embedded 
        in the GUI which corresponds to buffer buff_name

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *TextBufferChangeSpec* -- the TextBufferChangeSpec
        """
        debug.virtual('GenEditFrame.editor_buffer')


class GenEditFrameWithBuffers(GenEditFrame):
    """partial implementation of GenEditFrame which keeps track of
    buffers belonging to this frame

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *GenEditFrames owner* -- parent GenEditFrames object

    *STR app_name* -- portion of the title string indicating the name of this
    particular GenEdit editor.

    *STR instance_string* -- portion of the title string indicating the name of     this particular instance.
    """
    def __init__(self, owner, app_name, instance_string = "", **args):
        self.deep_construct(GenEditFrameWithBuffers,
                            {'owner': owner,
                             'app_name': app_name,
                             'instance_string': instance_string
                            }, args
                           )
        self.name_parent('owner')

    def set_instance_string(self, instance_string):
        """update the title to reflect the new instance string

        **INPUTS**

        *STR* instance_string -- string to include as part of the title

        **OUTPUTS**

        *none*
        """
        self.instance_string = instance_string
        self.update_title()
  
    def on_activate(self, activated = 1):
        """event handler for activation and deactivation events

        A concrete subclass of GenEditFrameActivateEvent must call this
        handler when this frame is activated or deactivated, so that it can
        inform its owner.  For subclasses which query for the active
        frame instead of storing the state, calling this method is
        optional

        **INPUTS**

        *BOOL active* -- whether this is an activate or deactivate event.

        **OUTPUTS**

        *none*
        """
        if activated:
# window is being activated
            self.owner.frame_activated( self.frame_ID, activated = 1)
        else:
            self.owner.frame_activated( self.frame_ID, activated = 0)

    
class GenEditFrameActivateEvent(GenEditFrameWithBuffers):
    """partial implementation of GenEditFrameWithBuffers with an event
    sent on activation/deactivation (sent to foreground/background),
    allowing us to keep track of whether the frame is active with a flag

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *INT ID* -- unique ID of this frame (usually the GUI ID), used for 
    communicating with the owner.  

    *BOOL active* -- flag indicating whether the frame is active (in the
    foreground)
    """
    def __init__(self, ID, **args):
        self.deep_construct(GenEditFrameActivateEvent,
                            {'active':0,
                             'ID': ID
                            }, args)

    def is_active(self):
        """indicates whether this frame is active

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if frame window is active
        """
        return self.active

    def on_activate(self, activated = 1):
        """event handler for activation and deactivation events

        A concrete subclass of GenEditFrameActivateEvent must call this
        handler when this frame is activated or deactivated, so that it can
        inform its owner.  

        **INPUTS**

        *BOOL active* -- whether this is an activate or deactivate event.

        **OUTPUTS**

        *none*
        """
        GenEditFrameWithBuffers.on_activate(self, activated)
        if activated:
# window is being activated
            self.active = 1
        else:
            self.active = 0


class GenEditFrames(GenEditBuffers):
    """partial implementation of GenEditBuffers

    **NOTE:** GenEditFrames assumes that no buffer ever appears in
    more than one frame

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *{INT: GenEditFrame} frames* -- map from IDs to frames

    *{STR: INT} corresponding_frames* -- map from buffer name to ID of
    the frame containing that buffer

    *INT next_ID* -- next available unique frame ID

    *STR app_name* -- name of the application
    (used as part of the title string)

    *STR instance_string* -- portion of the title string indicating the name of     this particular instance.
    """
    def __init__(self, app_name, **args):
        self.deep_construct(GenEditFrames,
                            {'frames': {},
                             'app_name': app_name,
                             'corresponding_frames': {},
                             'instance_string': "",
                             'next_ID': 0
                            }, args)
        self.add_owned('frames')

    def active_frame_ID(self):
        """returns the ID of the currently active frame, if any

        **INPUTS** 

        *none*

        **OUTPUTS**

        *INT* -- the unique ID of the currently active frame, or None if
        no frame of the editor is active.
        """ 
        debug.virtual('GenEditFrames.active_frame_ID')


    def active_frame(self):
        """returns the currently active frame, if any

        **INPUTS** 

        *none*

        **OUTPUTS**

        *GenEditFrame* -- the currently active frame, or None if
        no frame of the editor is active.
        """ 
        ID = self.active_frame_ID()
        if ID == None:
            return None
        return self.frames[ID]
                             
    def frame_activated(self, frame_ID, activated = 1):
        """callback used by GenEditFrame to notify GenEditFrames that 
        it has been activated or deactivated.

        **INPUTS**

        *STR frame_ID* -- the ID of the frame

        *BOOL activated* -- true if the frame has just been activated,
        otherwise false

        **OUTPUTS**

        *none*
        """
        pass

    def corresponding_frame(self, buff_name):
        """returns a reference to the GenEditFrame containing the given
        buffer

        **INPUTS**

        *STR buff_name* -- the name of the buffer

        **OUTPUTS**

        *GenEditFrame* -- the corresponding frame
        """
        if buff_name in self.corresponding_frames.keys():
            return self.frames[self.corresponding_frames[buff_name]]
        return None

    def new_frame(self, buff_name, instance_string = None):
        """creates a new frame of the appropriate concrete class
        open buffer and new window callbacks to the AppState interface

        **NOTE:** when adding a new frame with a buffer, you should call
        new_buffer first, followed by add_frame

        **INPUTS**

        *STR buff_name* -- the name of the initial buffer for the frame

        *STR instance_string* -- portion of the title string indicating 
        the name of this particular instance.

        **OUTPUTS**

        *GenEditFrame frame* -- the new frame 
        """
        debug.virtual('GenEditFrames.new_frame')

    def add_frame(self, frame, buff_name, user_initiated = 1):
        """adds a new buffer to self.buffers and new frame window 
        containing that buffer to self.frames, optionally performing 
        open buffer and new window callbacks to the AppState interface

        **NOTE:** when adding a new frame with a buffer, you should call
        new_buffer first, followed by add_frame

        **INPUTS**

        *GenEditFrame frame* -- the new frame 

        *STR buff_name* -- the name of the initial buffer for the frame

        *BOOL user_initiated* -- indicates whether this method was
        user-initiated or whether it was called by AppState.
        In the latter case, it will invoke the parent AppState's 
        open buffer callback, because AppState.open_file invokes 
        the callback itself. 

        **OUTPUTS**

        *INT* -- ID of the new frame, or None if the frame was not 
        added successfully
        """
        ID = self.next_ID
        self.frames[ID] = frame
        frame.set_frame_ID(ID)
        self.next_ID = ID + 1
        buffer = frame.editor_buffer(buff_name)
        self.new_buffer(buff_name, buffer, perform_callback =
            user_initiated)
        self.corresponding_frames[buff_name] = ID
        if user_initiated and self.app_control:
            self.app_control.new_window_cbk()
        return ID

    def remove_buffer(self, buff_name, perform_callback = 1):
        """remove a buffer from the list of buffers.  

        **Note:** this method only removes the buffer from GenEdit's
        records, it does not destroy the underlying GUI buffer or the
        window containing it.

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        *BOOL perform_callback* -- indicates whether this method should
        invoke the curr_buffer_name_cbk

        **OUTPUTS**

        *none*
        """
        if buff_name in self.open_buffers():
            frame = self.corresponding_frame(buff_name)
            frame.remove_buffer(buff_name)
            del self.corresponding_frames[buff_name]
            GenEditBuffers.remove_buffer(self, buff_name, perform_callback =
                perform_callback)

    def delete_buffer(self, buff_name, perform_callback = 1):
        """delete a buffer 

        **INPUTS**

        STR *buff_name* -- name of buffer to remove

        *BOOL perform_callback* -- indicates whether this method should
        invoke the curr_buffer_name_cbk

        **OUTPUTS**

        *none*
        """
        ID = self.corresponding_frames[buff_name]
        frame = self.corresponding_frame(buff_name)
        buffers = frame.open_buffers()
        if len(buffers) > 1:
# not last buffer in the window
            frame.delete_buffer(buff_name)
            if perform_callback and self.app_control:
                new_buff_name = frame.frame_active_buffer_name()
                self.app_control.curr_buffer_name_cbk(new_buff_name)
            self.remove_buffer(buff_name, 0)
            return
# last buffer in window
        if len(self.frames.keys()) == 1:
# and last window, so create a new, empty buffer in that window
            new_buff_name = self.generate_buffer_name("")
            buffer = self.editor_buffer(buff_name)
            clear_buffer(buffer)
#            print 'rename buffer on delete_buffer on last buffer in last window'
#            print 'from "%s" to "%s"' % (buff_name, new_buff_name)
            self.filenames[buff_name] = None
            self.rename_buffer(buff_name, new_buff_name,
                perform_callback = 0) 
            self.update_title(new_buff_name)
            if perform_callback and self.app_control:
                new_buff_name = frame.frame_active_buffer_name()
                self.app_control.curr_buffer_name_cbk(new_buff_name)
        else:
            self.remove_buffer(buff_name, 0)
# in case the frame doesn't get a chance to let us know it was
# deactivated
            frame.on_activate(0)
# cleanup the frame
            frame.cleanup()
            frame.close_window()
            del self.frames[ID]


# and return true so that the frame will close itself

    def prompt_to_save(self, buff_name):
        """prompts the user to save the current buffer before closing it, 
        or cancel.  Note: prompt_to_save should save if the user so
        indicates, and update the entry in self.filenames corresponding
        to the buffer, but should not close the buffer, because
        open_file could still fail.

        **INPUTS**

        *STR buff_name* -- the name of the buffer
        
        **OUTPUTS**

        *BOOL* -- true if the user saved or told GenEdit to proceed
        without saving, false if the user asked for the action causing
        the buffer closing to be cancelled.
        """
        frame = self.corresponding_frame(buff_name)
        debug.trace_call_stack('GenEditFrames.prompt_to_save')
        return frame.prompt_to_save(buff_name)

    def overwrite_prompt(self, buff_name, full_path):
        """prompts to see if the user is sure that he/she wants to
        overwrite an existing file

        **INPUTS**

        *STR buff_name* -- the name of the buffer (used to find the
        corresponding frame over which to pop up the Save As dialog)

        *STR full_path* -- path name of file to save

        **OUTPUTS**

        *BOOL* -- true if the user approves of overwriting the file
        """
        frame = self.corresponding_frame(buff_name)
        return frame.overwrite_prompt(buff_name)

    def open_file_dialog(self):
        """prompts for a file to open

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the specified path, or None if the user cancelled 
        """
#        print 'in open_file_dialog'
#        print self.active_frame
        frame = self.active_frame()
        init_dir = self.curr_dir
        return frame.open_file_dialog(init_dir)

    def save_as_dialog(self, buff_name):
        """prompts for a filename under which to save the file, and
        confirms overwriting

        **INPUTS**

        *STR buff_name* -- the name of the buffer (used to find the
        corresponding frame over which to pop up the Save As dialog)

        **OUTPUTS**

        *STR* -- the specified path, or None if the user cancelled 
        """
        frame = self.corresponding_frame(buff_name)
        init_dir = self.curr_dir
        try:
            old_file_name = self.filenames[buff_name]
            if not (old_file_name is None):
                path, short = os.path.split(old_file_name)
                if path:
                    init_dir = path
        except KeyError:
            pass
        return frame.save_as_dialog(buff_name, init_dir)

    def on_frame_close(self, ID):
        """method by which a frame can notify GenEdit that the user has
        requested that it be closed (either through the close button 
        or a menu item)

        The user may have an opportunity to cancel this command 
        (e.g. through the cancel button in a dialog prompting to save 
        modified files)

        **NOTE:** Unless the user cancels, this method will
        tell the frame to cleanup and close, so the caller should 
        not assume that it is in a sane state when this method returns.

        **INPUTS**

        *INT ID* -- ID of the frame sending the event

        **OUTPUTS**

        *BOOL* -- true if the frame should be closed in response to this
        event (unless, e.g., the user has hit cancel in response to a 
        save modified files dialog)
        """
        closing = 1
        frame = self.frames[ID]
        if len(self.frames.keys()) == 1:
            return self.on_exit(ID)

        buffers = frame.open_buffers()
        for buff_name in buffers:
            if self.modified_buffer(buff_name):
                if not self.prompt_to_save(buff_name):
                    closing = 0
        if not closing:
            return 0

# remove buffers from our records, and notify the owner that 
# the buffers are closing
        for buff_name in buffers:
            self.remove_buffer(buff_name, perform_callback = 1)
#            self.app_control.close_buffer_cbk(buff_name)

# in case the frame doesn't get a chance to let us know it was
# deactivated
        frame.on_activate(0)

# cleanup the frame
        frame.cleanup()

        del self.frames[ID]

# and return true so that the frame will close itself
        return 1

    def confirm_exit(self, ID = None):
        """method called by on_exit which allows the user to save files, 
        etc. or cancel the exit process

        **INPUTS**

        *INT ID* -- ID of the frame sending the event, or None if the
        event doesn't originate from a frame.  (Currently, this
        parameter is ignored).

        **OUTPUTS**

        *BOOL* -- true if the editor is exiting in response to this
        event (unless, e.g., the user has hit cancel in response to a 
        save modified files dialog)
        """
        exiting = 1
#        print 'here I am'
#        sys.stdout.flush()
#        print 'modified: ', repr(self.modified_buffers())
#        sys.stdout.flush()
        for buff_name in self.modified_buffers():
#            print 'buff name is "%s"' % buff_name
#            sys.stdout.flush()
            if not self.prompt_to_save(buff_name):
                exiting = 0
        if not exiting:
            return 0
        return 1

    def on_exit(self, ID = None):
        """method by which a frame can notify GenEdit that the user has
        selected the Exit item from the File menu.  The user may have an 
        opportunity to cancel this command (e.g. through the cancel button
        in a dialog prompting to save modified files)

        Unless the user cancels, on_exit will close all frames. 
        Depending on the particular GUI, this may cause the
        GUI event loop to exit.  If not, on_exit in the GenEditFrames 
        subclass for that GUI will have to call this method, and then 
        perform some additional processing if it returns true.

        **NOTE:** GenEdit is responsible for telling all frames to
        cleanup and close, so the caller should not assume that
        it is in a sane state when this method returns.

        on_exit

        **INPUTS**

        *INT ID* -- ID of the frame sending the event, or None if the
        event doesn't originate from a frame.  (Currently, this
        parameter is ignored).

        **OUTPUTS**

        *BOOL* -- true if the editor is exiting in response to this
        event (unless, e.g., the user has hit cancel in response to a 
        save modified files dialog)
        """
        if not self.confirm_exit(ID = ID):
            return 0

# notify the owner
#        print 'notifying owner'
#        sys.stdout.flush()
        if self.app_control:
            self.app_control.close_app_cbk()
#        print 'done notifying owner'
#        sys.stdout.flush()

# cleanup and close all frames
#        print self.frames.keys()
#        sys.stdout.flush()
        for ID in self.frames.keys():
#            print ID
#            sys.stdout.flush()
            frame = self.frames[ID]
#            print 'cleaning'
#            sys.stdout.flush()
            frame.cleanup()
#            print 'closing'
#            sys.stdout.flush()
            frame.close_window()
#            print 'deleting reference'
#            sys.stdout.flush()
            del self.frames[ID]

# cleanup self, unless we have an owner which will do so for us
        if self.owned_by() == None:
            self.cleanup()

        return 1

    def rename_buffer(self, buff_name, new_buff_name, perform_callback = 1):
        """renames a buffer, optionally performing a callback to the
        AppState interface

        **INPUTS**

        *STR buff_name* -- the old name of the new buffer

        *STR new_buff_name* -- the new name of the new buffer

        *BOOL perform_callback* -- indicates whether this method should
        invoke the parent AppState's rename buffer callback

        **OUTPUTS**

        *BOOL* -- true if the new buffer was renamed successfully
        """
        ID = self.corresponding_frames[buff_name]
        frame = self.corresponding_frame(buff_name)
#        print 'Frames.rename buffer "%s" to "%s"' % (buff_name, new_buff_name)
        success = GenEditBuffers.rename_buffer(self, buff_name,
            new_buff_name, perform_callback = perform_callback)
        if success:
            del self.corresponding_frames[buff_name]
            self.corresponding_frames[new_buff_name] = ID
            frame.rename_buffer(buff_name, new_buff_name)

    def update_title(self, buff_name):
        """update the window title of the frame containing buff_name to
        reflect a new buffer name (unless that buffer is not the current
        buffer displayed in that frame)
        
        **INPUTS**

        *STR buff_name* -- name of the buffer

        **OUTPUTS**

        *none*
        """
        frame = self.corresponding_frame(buff_name)
        if frame:
            frame.update_title()

    def editor_has_focus(self):
        """indicates whether the editor window has the focus

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if editor window has the focus
        """
        frame = self.active_frame()
        if frame == None:
            return 0
        return frame.editor_has_focus()

    def app_active_buffer_name(self):
        """Returns the name of the buffer currently active in the
        GenEdit editor.

        **INPUTS**

        *none* 
        
        **OUTPUTS**

        *STR* -- buffer name of current buffer, or None if there is none

        """
#        print self, self.__class__, self.__class__.__bases__
#        print self.active_frame
#        print self.active_frame_ID
        frame = self.active_frame()
        if not frame:
            return None
        return frame.frame_active_buffer_name()

    def app_change_buffer(self, buff_name):
        """Changes the external application's active buffer.

        **INPUTS**
        
        STR *buff_name* -- Name of the buffer to switch to.
       
        **OUTPUTS**
        
        *BOOL* -- true if buff_name exists and the application
        successfully switches to it
        """
        frame = self.corresponding_frame(buff_name)
        if not frame:
            return 0
        frame.switch_to_buffer(buff_name)
        return 1

    def set_instance_string(self, instance_string):
        """sets the title string which is included in the full title 
        displayed in the title bar

        **INPUTS**

        *STR* instance_string -- string to include as part of the title

        **OUTPUTS**

        *BOOL* -- true if the editor, given the title escape sequence, 
        can and will include the instance string in its window title 
        for all windows containing editor buffers.
        """
        self.instance_string = instance_string
        for frame in self.frames.values():
            frame.set_instance_string(instance_string)
        return 1
  
class ActivateEventMixIn(Object.Object):
    """mix-in which implements GenEditFrames.is_active and active_frame_ID

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *INT current_frame* -- ID of the active frame
    """
    def __init__(self, **args):
        self.deep_construct(ActivateEventMixIn,
                            {'current_frame': None
                            }, args)
                             

    def frame_activated(self, frame_ID, activated = 1):
        """callback used by GenEditFrame to notify GenEditFrames that 
        it has been activated or deactivated.

        **INPUTS**

        *STR frame_ID* -- the ID of the frame

        *BOOL activated* -- true if the frame has just been activated,
        otherwise false

        **OUTPUTS**

        *none*
        """
#        print 'activated = %d, %d' % (activated, frame_ID)
        if activated:
            self.current_frame = frame_ID
        elif self.current_frame == frame_ID:
            self.current_frame = None

    def is_active(self):
        """indicates whether an editor frame is active

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if frame window is active
        """
        if self.current_frame == None:
            return 0
        return 1

    def active_frame_ID(self):
        """returns the ID of the currently active frame, if any

        **INPUTS** 

        *none*

        **OUTPUTS**

        *INT* -- the unique ID of the currently active frame, or None if
        no frame of the editor is active.
        """ 
        return self.current_frame

class GenEditSingle(GenEditFrames):
    """partially concrete subclass of GenEditFrames with only a single
    frame and a single buffer

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *INT only_ID* -- ID of the only frame
    """
    def __init__(self, init_buff_name = "", show = 1, **args):
        self.deep_construct(GenEditSingle,
                            {'only_ID': None}, args,
                            enforce_value = {'multiple': 0})
        frame = self.new_frame(buff_name = init_buff_name)
# at this stage, GenEdit hasn't been added to AppStateGenEdit yet, so we
# use user_initiated = 1 so we don't do any AppState callbacks
        ID = self.add_frame(frame, init_buff_name, user_initiated = 1)
        if show:
            frame.show(initial = 1)
        self.only_ID = ID

    def open_file_new_buffer(self, file_name, new_buff_name,
            user_initiated = 0, file_exists = 1):
        """opens a new file.  Depending on the subclass of 
        GenEditBuffers, this may open a new frame, or hide the 
        previously visible buffer in the same frame.

        **NOTE:** despite the name of this method, the 
        TextBufferChangeSpec returned can be an existing buffer, 
        if, e.g.,  the editor only has one window and one buffer.

        **INPUTS**

        *STR file_name*  -- the full path of the file to open, or None
        to create a new, empty buffer

        *STR new_buff_name*  -- the name which will be given to the new
        buffer

        *BOOL user_initiated* -- indicates whether this method was
        user-initiated or whether it was called by AppState.
        In the latter case, it will invoke the parent AppState's 
        open buffer callback, because AppState.open_file invokes 
        the callback itself. 

        *BOOL file_exists* -- indicates whether the specified file
        exists (this allows us to specify a filename for an initially
        empty buffer)

        **OUTPUTS**

        *BOOL* -- true if the file was opened successfully.
        Note: if no file by the name file_name exists, 
        open_file_new_buffer should create a new, empty buffer with 
        that file name.
        """
        frame = self.frames[self.only_ID]
        old_buff_name = frame.frame_active_buffer_name()
#        print 'opening in old buffer "%s"' % old_buff_name
        buffer = frame.editor_buffer(old_buff_name)
# we use clear_buffer and silent_load_file, because the old_buff_name
# refers to a scratch buffer which will be eliminated if we are
# successful.  Therefore, we don't want any change events associated
# with that old buffer name.
        if not file_exists or file_name == None:
            clear_buffer(buffer)
            success = 1
        else:
            success = silent_load_file(buffer, file_name)
        if success:
            self.rename_buffer(old_buff_name, new_buff_name, 
                perform_callback = 0)
            self.filenames[new_buff_name] = file_name
            if user_initiated and self.app_control:
                self.app_control.open_buffer_cbk(new_buff_name)
        return success

    def multiple_windows(self):
        """does editor support multiple windows per instance?

        Note: the purpose of this function is to allow the RecogStartMgr
        to determine whether a previously unknown window could belong to
        this known instance.  Therefore, Emacs running in text mode 
        should return false, even though it can have (sub-)windows in 
        a single frame.  
        
        Note: multiple windows of remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will not appear to the mediator 
        as having separate windows.  However, the mediator will perform a 
        separate check to detect this, so remote editors which support
        multiple windows should return true, regardless of the remote
        display method.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports opening multiple editor windows.  
        """
        return 0
    
class GenEditSimple(GenEditFrames):
    """partially concrete subclass of GenEdit with multiple frame windows, but
    only one buffer per window.

    **CLASS ATTRIBUTES**

    *none*

    **INSTANCE ATTRIBUTES**

    *none*
    """
    def __init__(self, init_buff_name = "", show = 1, **args):
        self.deep_construct(GenEditSimple,
                            {}, args,
                            enforce_value = {'multiple': 1})
        frame = self.new_frame(buff_name = init_buff_name)
# at this stage, GenEdit hasn't been added to AppStateGenEdit yet, so we
# use user_initiated = 1 so we don't do any AppState callbacks
        ID = self.add_frame(frame, init_buff_name, user_initiated = 1)
        if show:
            frame.show(initial = 1)

    def open_file_new_buffer(self, file_name, new_buff_name,
            user_initiated = 0, file_exists = 1):
        """opens a new file and returns the corresponding
        TextBufferChangeSpec.  Depending on the subclass of 
        GenEditBuffers, this may open a new frame, or hide the 
        previously visible buffer in the same frame.

        **NOTE:** despite the name of this method, the 
        TextBufferChangeSpec returned can be an existing buffer, 
        if, e.g.,  the editor only has one window and one buffer.

        **INPUTS**

        *STR file_name*  -- the full path of the file to open, or None
        to create a new, empty buffer

        *STR new_buff_name*  -- the name which will be given to the new
        buffer

        *BOOL user_initiated* -- indicates whether this method was
        user-initiated or whether it was called by AppState.
        In the latter case, it will invoke the parent AppState's 
        open buffer callback, because AppState.open_file invokes 
        the callback itself. 

        *BOOL file_exists* -- indicates whether the specified file
        exists (this allows us to specify a filename for an initially
        empty buffer)

        **OUTPUTS**

        *BOOL* -- true if the file was opened successfully.
        Note: if no file by the name file_name exists, 
        open_file_new_buffer should create a new, empty buffer with 
        that file name.
        """
        frame = self.new_frame(buff_name = new_buff_name,
            instance_string = self.instance_string)
        buffer = frame.editor_buffer(new_buff_name)
        if not file_exists or file_name == None:
# unnecessary, since it is a new frame with a new buffer 
#            buffer.set_text("")
            success = 1
        else:
# unlike the single-buffer case above, we are working with a new buffer,
# so it is okay to send the change event (though that might result in
# the server being notified of the new file early, so maybe we shouldn't)
            success = buffer.load_file(file_name)
        if not success:
            frame.cleanup()
            frame.close_window()
            return 0
        self.filenames[new_buff_name] = file_name
        ID = self.add_frame(frame, new_buff_name, user_initiated =
            user_initiated)
        if ID != None:
            frame.show(initial = 1)
            return 1
        else:
            frame.cleanup()
            frame.close_window()
            return 0

    def multiple_windows(self):
        """does editor support multiple windows per instance?

        Note: the purpose of this function is to allow the RecogStartMgr
        to determine whether a previously unknown window could belong to
        this known instance.  Therefore, Emacs running in text mode 
        should return false, even though it can have (sub-)windows in 
        a single frame.  
        
        Note: multiple windows of remote editors running in a remote display
        which appears as a single window to be local operating system 
        (X servers in single window mode, VNC) will not appear to the mediator 
        as having separate windows.  However, the mediator will perform a 
        separate check to detect this, so remote editors which support
        multiple windows should return true, regardless of the remote
        display method.

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor supports opening multiple editor windows.  
        """
        return 1





# defaults for vim - otherwise ignore
# vim:sw=4
